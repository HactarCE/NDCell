//! NDCA black-box test suite.

use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use ndcell_core::prelude::*;

/// Constructs a `TestError`.
macro_rules! test_error {
    ($msg:tt @ $loc:expr) => {
        TestError {
            msg: $msg,
            loc: $loc,
        }
    };
}

/// Constructs a `Vec<TestError>`
macro_rules! test_errors {
    [$($msg:tt @ $loc:expr),+ $(,)?] => {
        vec![$(test_error!($msg @ $loc)),+]
    };
}

/// Constructs a `TestResult::Err`.
macro_rules! test_err {
    ($($t:tt)+) => {
        Err(test_errors!($($t)+))
    };
}

/// Constructs a `TestResult::Ok`.
macro_rules! test_ok {
    ($($val:expr),* $(,)?) => {
        Ok(vec!($($val),*))
    };
}

macro_rules! _test_result {
    (Ok($($t:tt)*)) => {
        test_ok!($($t)*)
    };
    (Err($($t:tt)+)) => {
        test_err!($($t)+)
    };
}

/// Constructs a `Vec<TestCase>`.
macro_rules! test_cases {
    [$(
        ($($in:expr),* $(,)?) => $out:tt($($t:tt)*)
    ),+ $(,)?] => {
        vec![$(TestCase {
            inputs: vec![$($in),*],
            expected_result: _test_result!($out($($t)*)),
        }),+]
    }
}

mod bools;
mod cell_arrays;
mod cmp;
mod conditions;
mod const_eval;
mod constants;
mod convert;
mod debug;
mod empty_set;
mod for_loops;
mod integer_sets;
mod math;
mod values;
mod vector_sets;
mod vectors;
mod while_loops;

use crate::data::{CellArray, GetType, LangCell, LangInt, LangUint, RtVal, Type};
use crate::errors::{self, Error};
use crate::exec::{CompiledFunction, Compiler, CompilerConfig, CtxTrait, Runtime};
use crate::utils;
use crate::{ast, LangMode};
use values::*;

/// Input/output pair.
#[derive(Debug, Clone)]
struct TestCase<'s, OK = RtVal> {
    inputs: Vec<RtVal>,
    expected_result: TestResult<'s, OK>,
}

/// Output of running some code; either a success with some values, or a failure
/// with some errors.
type TestResult<'s, OK> = Result<Vec<OK>, Vec<TestError<'s>>>;

/// Expected error information for testing.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct TestError<'s> {
    /// User-facing error message.
    msg: &'s str,
    /// String of source code where the error occured.
    loc: &'s str,
}

/// Monkeypatch iterator trait for convenience.
trait MapCollectVec: Sized + IntoIterator {
    /// Shorthand for `.into_iter().map(f).collect_vec()`.
    fn map_collect_vec<T>(self, f: impl FnMut(Self::Item) -> T) -> Vec<T> {
        self.into_iter().map(f).collect()
    }
}
impl<T: IntoIterator> MapCollectVec for T {}

#[derive(Debug, Default, Copy, Clone)]
struct TestProgram<'a> {
    mode: LangMode,
    setup: &'a str,
    exec: &'a str,
    input_types: &'a [Type],
    result_expressions: &'a [(Type, &'a str)],
}
impl<'a> TestProgram<'a> {
    /// Creates a new blank test program.
    pub fn new() -> Self {
        Self {
            mode: LangMode::Internal,
            setup: "/* no setup */",
            exec: "/* no exec */",
            input_types: &[],
            result_expressions: &[],
        }
    }
    /// Adds initialization directives to the top of the program.
    pub fn with_setup(mut self, setup: &'a str) -> Self {
        self.setup = setup;
        self
    }
    /// Adds statements to be compiled or interpreted.
    pub fn with_exec(mut self, exec: &'a str) -> Self {
        self.exec = exec;
        self
    }
    /// Defines input types to get information into the program.
    pub fn with_input_types(mut self, input_types: &'a [Type]) -> Self {
        self.input_types = input_types;
        self
    }
    /// Defines output types and expressions to get information out of the program.
    pub fn with_result_expressions(mut self, result_expressions: &'a [(Type, &'a str)]) -> Self {
        self.result_expressions = result_expressions;
        self
    }

    /// Returns an AST for interpreting the program.
    fn ast_for_interpreter_test(self) -> (Arc<ast::Program>, ast::StmtId, Vec<ast::ExprId>) {
        use crate::parser;

        let mut ast = ast::Program::new();

        parse_or_panic(
            parser::parse_directives,
            &mut ast,
            "setup.test",
            self.setup,
            self.mode,
        );

        let stmt_src = format!("{{\n{}\n}}", self.exec);
        let stmt_id = parse_or_panic(
            parser::parse_statement,
            &mut ast,
            "exec.test",
            &stmt_src,
            self.mode,
        );

        let expr_ids = self
            .result_expressions
            .iter()
            .enumerate()
            .map(|(i, (_ty, expr))| {
                parse_or_panic(
                    parser::parse_expression,
                    &mut ast,
                    &format!("expression{}.test", i),
                    expr,
                    self.mode,
                )
            })
            .collect();

        (Arc::new(ast), stmt_id, expr_ids)
    }
    /// Returns source code for compiling the program.
    fn source_for_compiler_test(self) -> String {
        let mut param_types = vec![];
        param_types.extend_from_slice(self.input_types);
        param_types.extend(self.result_expressions.iter().map(|(ty, _expr)| ty.clone()));

        let mut source = String::new();
        source.push_str(self.setup);
        source.push_str("\n\n");
        source.push_str(&format!(
            "@compile {}",
            utils::display_bracketed_list(&param_types),
        ));

        source.push_str(" {\n");
        let n = self.input_types.len();
        for i in 0..n {
            source.push_str(&format!("x{} = $compiled_arg[{}]\n", i, i));
        }
        source.push_str(self.exec);
        source.push('\n');
        for (i, (ty, expr)) in self.result_expressions.iter().enumerate() {
            if matches!(ty, Type::CellArray(_)) {
                // Cell arrays use reference semantics, so do not assign to the
                // argument.
                source.push_str(&format!("_ = {}\n", expr));
                continue;
            } else {
                // All other types use value semantics.
                source.push_str(&format!("$compiled_arg[{}] = {}\n", n + i, expr));
            }
        }
        source.push_str("}\n");

        source
    }
    /// Returns an AST for compiling the program.
    fn ast_for_compiler_test(self) -> Arc<ast::Program> {
        use crate::parser;

        let mut ast = ast::Program::new();
        parse_or_panic(
            parser::parse_directives,
            &mut ast,
            "compiled.test",
            &self.source_for_compiler_test(),
            self.mode,
        );
        Arc::new(ast)
    }

    /// Creates and initializes a new `Runtime`, panicking if any error occurs.
    fn init_new_runtime(self, ast: &ast::Program) -> Runtime {
        let mut runtime = Runtime::init_new(&ast);
        let task_str = format!("initializing {:#?}", self);
        let result = runtime.get_errors_and_warnings_result();
        assert_results_eq(&ast, &task_str, &result, &Ok(()));
        runtime
    }

    fn call_interpreted_func(
        self,
        mut runtime: Runtime,
        ast: &Arc<ast::Program>,
        stmt_id: ast::StmtId,
        expr_ids: &[ast::ExprId],
        inputs: &[RtVal],
    ) -> (String, Result<Vec<String>, Vec<Error>>) {
        let task_str = format!("intepreting {:#?} with inputs {:?}", self, inputs);
        for (i, value) in inputs.iter().enumerate() {
            let var_name = Arc::new(format!("x{}", i));
            runtime.assign_var(&var_name, value.clone());
        }
        let actual_result: Result<Vec<_>, _> = runtime
            .exec_stmt(ast.get_node(stmt_id))
            .map_err(|e| {
                runtime.report_error(e);
                runtime.ctx().errors.clone()
            })
            .and_then(|_| {
                let mut ret = vec![];

                // Return result expressions.
                for &expr_id in expr_ids {
                    ret.push(
                        runtime
                            .eval_expr(ast.get_node(expr_id))
                            .map(|v| v.node.to_string())
                            .map_err(|e| {
                                runtime.report_error(e);
                                runtime.ctx().errors.clone()
                            })?,
                    );
                }

                // Also return mutated cell arrays.
                for i in 0..inputs.len() {
                    let var_name = Arc::new(format!("x{}", i));
                    if let Some(RtVal::CellArray(a)) = runtime.vars.remove(&var_name) {
                        ret.push(a.to_string());
                    }
                }

                Ok(ret)
            });
        (task_str, actual_result)
    }
    fn setup_compiled_func(self) -> (Arc<ast::Program>, CompiledFunction) {
        let ast = self.ast_for_compiler_test();
        let runtime = self.init_new_runtime(&ast);
        let f = match Compiler::compile(Arc::clone(&ast), runtime, CompilerConfig::default()) {
            Ok(ok) => ok,
            Err(e) => {
                let task_str = &format!("compiling {:#?}", self);
                assert_results_eq(&ast, task_str, &Err(e), &Ok(()));
                unreachable!();
            }
        };
        (ast, f)
    }
    fn call_compiled_func(
        self,
        f: &mut CompiledFunction,
        inputs: &[RtVal],
    ) -> (String, Result<Vec<String>, Vec<Error>>) {
        let task_str = format!("running compiled {:#?} with inputs {:?}", self, inputs);
        let mut arg_values = inputs.to_vec();
        for (ty, _expr) in self.result_expressions {
            arg_values.push(match ty {
                Type::Integer => RtVal::Integer(0),
                Type::Cell => RtVal::Cell(0),
                Type::Tag => todo!("tag default value"),
                Type::Vector(Some(len)) => RtVal::Vector(vec![0; *len]),
                Type::CellArray(Some(shape)) => RtVal::CellArray(Arc::new(
                    CellArray::from_cell(Arc::clone(shape), 0_u8).into(),
                )),
                Type::CellSet => todo!("cell set default value"),
                _ => panic!("no default for this type"),
            });
        }
        let n = inputs.len();
        let actual_result = match f.call(&mut arg_values) {
            Ok(()) => Ok({
                let mut ret = vec![];

                // Return result expressions.
                ret.extend(arg_values[n..].iter().map(|v| v.to_string()));

                // Also return mutated cell arrays.
                for arg in arg_values {
                    if let RtVal::CellArray(a) = arg {
                        ret.push(a.to_string());
                    }
                }

                ret
            }),
            Err(e) => Err(vec![e]),
        };
        (task_str, actual_result)
    }

    /// Asserts that attempting to parse the program produces a syntax error.
    pub fn assert_syntax_error(self, expected_error: TestError<'_>) -> Self {
        let name = "testfile.test".to_owned();
        let source = self.source_for_compiler_test();
        let mut ast = ast::Program::new();
        let actual_result = match crate::parser::parse_file(&mut ast, name, source, self.mode) {
            Ok(_) => Ok("parsed successfully"),
            Err(error) => Err(vec![error]),
        };
        let task_str = format!("parsing {:#?}", self);
        assert_results_eq(&ast, &task_str, &actual_result, &Err(vec![expected_error]));
        self
    }
    /// Asserts that attempting to interpret the initialization directives of
    /// the program produces a runtime error and attempting to compile the
    /// program produces a compile error.
    pub fn assert_compile_or_interpreted_errors(
        self,
        inputs: Vec<RtVal>,
        expected_errors: Vec<TestError<'_>>,
    ) -> Self {
        let expected_result = Err(expected_errors.clone());
        let test_case = TestCase {
            inputs,
            expected_result,
        };
        self.assert_interpreted_test_cases::<&str>(vec![test_case])
            .assert_compile_errors(expected_errors)
    }
    /// Asserts that attempting to interpret the initialization directives of
    /// the program produces a runtime error.
    pub fn assert_init_errors(self, expected_errors: Vec<TestError<'_>>) -> Self {
        let ast = self.ast_for_compiler_test();
        let mut runtime = Runtime::init_new(&ast);
        let actual_result = runtime.get_errors_and_warnings_result();
        let task_str = format!("running initialization for {:#?}", self);
        assert_results_eq(&ast, &task_str, &actual_result, &Err(expected_errors));
        self
    }
    /// Asserts that attempting to compile the program produces a compile error.
    pub fn assert_compile_errors(self, expected_errors: Vec<TestError<'_>>) -> Self {
        let ast = self.ast_for_compiler_test();
        let runtime = self.init_new_runtime(&ast);
        let actual_result = Compiler::compile(Arc::clone(&ast), runtime, CompilerConfig::default())
            .map(|_| "compiled successfully");
        let task_str = format!("compiling {:#?}", self);
        assert_results_eq(&ast, &task_str, &actual_result, &Err(expected_errors));
        self
    }

    /// Asserts that the program compiles.
    pub fn assert_compile_ok(self) -> Self {
        self.assert_compiled_test_cases::<&str>(vec![])
    }

    /// Asserts that all test cases produce the specified output, both when
    /// interpreted and when compiled.
    pub fn assert_test_cases<'s, OK: Clone + ToString>(
        self,
        test_cases: Vec<TestCase<'s, OK>>,
    ) -> Self {
        self.assert_interpreted_test_cases(test_cases.clone())
            .assert_compiled_test_cases(test_cases)
    }
    /// Asserts that all test cases produce the specified output when
    /// interpreted.
    pub fn assert_interpreted_test_cases<'s, OK: Clone + ToString>(
        self,
        test_cases: Vec<TestCase<'s, OK>>,
    ) -> Self {
        let (ast, stmt_id, expr_ids) = self.ast_for_interpreter_test();
        let clean_runtime = self.init_new_runtime(&ast);
        for case in test_cases {
            let (task_str, actual_result) = self.call_interpreted_func(
                clean_runtime.clone(),
                &ast,
                stmt_id,
                &expr_ids,
                &case.inputs,
            );
            // Check results.
            let expected_result = case
                .expected_result
                .map(|oks| oks.iter().map(|s| s.to_string()).collect_vec());
            assert_results_eq(&ast, &task_str, &actual_result, &expected_result);
        }
        self
    }
    /// Asserts that all test cases produce the specified output when compiled.
    pub fn assert_compiled_test_cases<'s, OK: Clone + ToString>(
        self,
        test_cases: Vec<TestCase<'s, OK>>,
    ) -> Self {
        let (ast, mut f) = self.setup_compiled_func();
        for case in test_cases {
            let (task_str, actual_result) = self.call_compiled_func(&mut f, &case.inputs);
            // Check result.
            let expected_result = case
                .expected_result
                .map(|oks| oks.iter().map(|s| s.to_string()).collect_vec());
            assert_results_eq(&ast, &task_str, &actual_result, &expected_result);
        }
        self
    }
    /// Asserts that the compiled and interpreted programs produce the same
    /// outputs for each set of inputs.
    pub fn assert_compiled_and_interpreted_agree(self, cases: Vec<Vec<RtVal>>) -> Self {
        let (compiled_ast, mut compiled_f) = self.setup_compiled_func();
        let (interpreted_ast, stmt_id, expr_ids) = self.ast_for_interpreter_test();
        let clean_runtime = self.init_new_runtime(&interpreted_ast);
        for inputs in cases {
            let (_task_str, interpreted_result) = self.call_interpreted_func(
                clean_runtime.clone(),
                &interpreted_ast,
                stmt_id,
                &expr_ids,
                &inputs,
            );
            let interpreted_result_str = match interpreted_result {
                Ok(ok) => format!("{:?}", ok),
                Err(e) => errors::format_errors(interpreted_ast.codemap(), &e),
            };

            let (_task_str, compiled_result) = self.call_compiled_func(&mut compiled_f, &inputs);
            let compiled_result_str = match compiled_result {
                Ok(ok) => format!("{:?}", ok),
                Err(e) => errors::format_errors(compiled_ast.codemap(), &e),
            };

            if interpreted_result_str != compiled_result_str {
                panic!(
                    "Interpreted and compiled functions disagreed\n\nInterpreted function produces this:\n{:?}\nCompiled function produces this:\n{:?}\nwhen executing this program:\n{:#?}",
                    interpreted_result_str, compiled_result_str, self,
                );
            }
        }
        self
    }
}

fn parse_or_panic<T>(
    f: fn(&mut ast::Program, &codemap::File, LangMode) -> Result<T, Error>,
    ast: &mut ast::Program,
    file_name: &str,
    file_contents: &str,
    mode: LangMode,
) -> T {
    let file = ast.add_file(file_name.to_owned(), file_contents.to_owned());
    match f(ast, &file, mode) {
        Ok(x) => x,
        Err(e) => panic!(
            "NDCA syntax error:\n{}",
            errors::format_errors(ast.codemap(), &[e]),
        ),
    }
}

fn assert_results_eq<T: fmt::Debug + PartialEq>(
    ast: &ast::Program,
    task_str: &str,
    actual_result: &Result<T, Vec<Error>>,
    expected_result: &Result<T, Vec<TestError<'_>>>,
) {
    match (actual_result, expected_result) {
        (_, Err(expected_errors)) if expected_errors.is_empty() => {
            panic!("empty expected error list is invalid");
        }

        (Ok(actual_ok), Ok(expected_ok)) => {
            if *expected_ok != *actual_ok {
                panic!(
                    "Expected {:?} but got {:?} when {}",
                    expected_ok, actual_ok, task_str,
                );
            }
        }

        (Ok(actual_ok), Err(expected_errors)) => {
            let expected = expected_errors[0];
            panic!(
                "Expected error at {:?} with message {:?} but got {:?} when {}",
                expected.loc, expected.msg, actual_ok, task_str,
            );
        }

        (Err(actual_errors), Ok(expected_ok)) => {
            let formatted_actual_errors = errors::format_errors(ast.codemap(), actual_errors);
            panic!(
                "Expected {:?} but got error when {}:\n{}",
                expected_ok, task_str, formatted_actual_errors,
            );
        }

        (Err(actual_errors), Err(expected_errors)) => {
            for (actual_error, expected) in actual_errors.iter().zip(expected_errors.iter()) {
                let diagnostic = actual_error.as_diagnostic().unwrap();
                let span = diagnostic.spans[0].span;
                let file = ast.codemap().look_up_span(span).file;
                if expected.loc != file.source_slice(span) || expected.msg != diagnostic.message {
                    let formatted_actual_errors =
                        errors::format_errors(ast.codemap(), actual_errors);
                    panic!(
                        "Expected error at {:?} with message {:?} but got a different error when {}:\n{}",
                        expected.loc, expected.msg, task_str, formatted_actual_errors,
                    );
                }
            }
        }
    }
}
