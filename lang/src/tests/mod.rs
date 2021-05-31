//! NDCA black-box test suite.

use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

// mod bools;
// mod branch;
// mod byterepr;
// mod cmp;
mod convert;
// mod debug;
mod const_eval;
// mod filters;
// mod funcs;
// mod is;
// mod loops;
mod math;
// mod patterns;
// mod ranges;
// mod rects;
// mod types;
mod values;
// mod vars;
// mod vecs;

use crate::ast;
use crate::data::{LangInt, LangUint, RtVal, Type};
use crate::errors::Error;
use crate::exec::{Compiler, CompilerConfig, CtxTrait, Runtime};
use crate::utils;
use values::*;

type TestError<'s> = (&'s str, &'s str);
type TestResult<'s> = Result<Vec<RtVal>, Vec<TestError<'s>>>;
type TestCase<'s> = (Vec<RtVal>, TestResult<'s>);

#[derive(Debug, Default, Copy, Clone)]
struct TestProgram<'a> {
    setup: &'a str,
    exec: &'a str,
    input_types: &'a [Type],
    result_expressions: &'a [(Type, &'a str)],
}
impl<'a> TestProgram<'a> {
    /// Creates a new blank test program.
    pub fn new() -> Self {
        Self {
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

        parse_or_panic(parser::parse_file, &mut ast, "setup.test", self.setup);

        let stmt_src = format!("{{\n{}\n}}", self.exec);
        let stmt_id = parse_or_panic(parser::parse_statement, &mut ast, "exec.test", &stmt_src);

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
        source.push_str(self.exec);
        source.push('\n');
        for i in 0..n {
            source.push_str(&format!("x{} = __compiled_arg__[{}]\n", i, i));
        }
        for (i, (_ty, expr)) in self.result_expressions.iter().enumerate() {
            source.push_str(&format!("__compiled_arg__[{}] = {}\n", n + i, expr));
        }
        source.push_str("}\n");

        source
    }
    /// Returns an AST for compiling the program.
    fn ast_for_compiler_test(self) -> Arc<ast::Program> {
        use crate::parser;

        let mut ast = ast::Program::new();
        parse_or_panic(
            parser::parse_file,
            &mut ast,
            "compiled.test",
            &self.source_for_compiler_test(),
        );
        Arc::new(ast)
    }

    /// Creates and initializes a new `Runtime`, panicking if any error occurs.
    fn init_new_runtime(self, ast: &ast::Program) -> Runtime {
        let mut runtime = Runtime::init_new(&ast);
        let task_str = format!("initializing {:#?}", self);
        assert_results_eq(&ast, &task_str, &runtime.get_errors_list(), &Ok(()));
        runtime
    }

    /// Asserts that attempting to parse the program produces a syntax error.
    pub fn assert_syntax_error(self, expected_error: (&str, &str)) {
        let source = self.source_for_compiler_test();
        let mut ast = ast::Program::new();
        let file = ast.add_file("testfile.test".to_owned(), source);
        let actual_result = match crate::parser::parse_file(&mut ast, &file) {
            Ok(_) => Ok("parsed successfully"),
            Err(error) => Err(vec![error]),
        };
        let task_str = format!("parsing {:#?}", self);
        assert_results_eq(&ast, &task_str, &actual_result, &Err(vec![expected_error]));
    }
    /// Asserts that attempting to interpret the initialization directives of
    /// the program produces a runntime error.
    pub fn assert_init_errors(self, expected_errors: Vec<(&str, &str)>) {
        let ast = self.ast_for_compiler_test();
        let mut runtime = Runtime::init_new(&ast);
        let actual_result = runtime.get_errors_list();
        let task_str = format!("running initialization for {:#?}", self);
        assert_results_eq(&ast, &task_str, &actual_result, &Err(expected_errors));
    }
    /// Asserts that attempting to compile the program produces a compile error.
    pub fn assert_compile_errors(self, expected_errors: Vec<(&str, &str)>) {
        let ast = self.ast_for_compiler_test();
        let runtime = self.init_new_runtime(&ast);
        let actual_result = Compiler::compile(Arc::clone(&ast), runtime, CompilerConfig::default())
            .map(|_| "compiled successfully");
        let task_str = format!("compiling {:#?}", self);
        assert_results_eq(&ast, &task_str, &actual_result, &Err(expected_errors));
    }

    /// Asserts that all test cases produce the specified output, both when
    /// interpreted and when compiled.
    pub fn assert_test_cases(
        self,
        inputs_and_expected_results: &[(Vec<RtVal>, Result<Vec<RtVal>, Vec<(&str, &str)>>)],
    ) {
        self.assert_interpreted_test_cases(inputs_and_expected_results);
        self.assert_compiled_test_cases(inputs_and_expected_results);
    }
    /// Asserts that all test cases produce the specified output when
    /// interpreted.
    pub fn assert_interpreted_test_cases(
        self,
        inputs_and_expected_results: &[(Vec<RtVal>, Result<Vec<RtVal>, Vec<(&str, &str)>>)],
    ) {
        let (ast, stmt_id, expr_ids) = self.ast_for_interpreter_test();
        let clean_runtime = self.init_new_runtime(&ast);
        for (inputs, expected_result) in inputs_and_expected_results {
            let mut runtime = clean_runtime.clone();

            let task_str = &format!("intepreting inputs {:?} on {:#?}", inputs, self);
            // Set input variables.
            for (i, value) in inputs.iter().enumerate() {
                let var_name = Arc::new(format!("x{}", i));
                runtime.vars.insert(var_name, value.clone());
            }
            // Execute statement and result expressions.
            let actual_result: Result<Vec<_>, _> = runtime
                .exec_stmt(ast.get_node(stmt_id))
                .map_err(|_| runtime.ctx().errors.clone())
                .and_then(|_| {
                    expr_ids
                        .iter()
                        .map(|&expr_id| {
                            let expr = ast.get_node(expr_id);
                            runtime
                                .eval_expr(expr)
                                .map(|v| v.node)
                                .map_err(|_| runtime.ctx().errors.clone())
                        })
                        .collect()
                });
            // Check results.
            assert_results_eq(&ast, task_str, &actual_result, expected_result);
        }
    }
    /// Asserts that all test cases produce the specified output when compiled.
    pub fn assert_compiled_test_cases(
        self,
        inputs_and_expected_results: &[(Vec<RtVal>, Result<Vec<RtVal>, Vec<(&str, &str)>>)],
    ) {
        let ast = self.ast_for_compiler_test();
        let runtime = self.init_new_runtime(&ast);
        let mut f = match Compiler::compile(Arc::clone(&ast), runtime, CompilerConfig::default()) {
            Ok(ok) => ok,
            Err(e) => panic!("\n{}", format_actual_errors(&ast, &e)),
        };

        for (inputs, expected_result) in inputs_and_expected_results {
            let task_str = &format!("evaluating inputs {:?} on compiled {:#?}", inputs, self);
            // Set input parameters.
            let mut arg_values = inputs.to_vec();
            for (ty, _expr) in self.result_expressions {
                arg_values.push(match ty {
                    Type::Integer => RtVal::Integer(0),
                    Type::Cell => RtVal::Cell(0),
                    Type::Tag => todo!("tag default value"),
                    Type::Vector(Some(len)) => RtVal::Vector(vec![0; *len]),
                    Type::Array(_) => todo!("array default value"),
                    Type::CellSet => todo!("cell set default value"),
                    _ => panic!("no default for this type"),
                });
            }
            // Call function.
            let n = inputs.len();
            let actual_result = match f.call(&mut arg_values) {
                Ok(()) => Ok(arg_values[n..].to_vec()),
                Err(e) => Err(vec![e]),
            };
            // Check result.
            assert_results_eq(&ast, task_str, &actual_result, expected_result);
        }
    }
}

fn parse_or_panic<T>(
    f: fn(&mut ast::Program, &codemap::File) -> Result<T, Error>,
    ast: &mut ast::Program,
    file_name: &str,
    file_contents: &str,
) -> T {
    let file = ast.add_file(file_name.to_owned(), file_contents.to_owned());
    match f(ast, &file) {
        Ok(x) => x,
        Err(e) => panic!("NDCA syntax error:\n{}", format_actual_errors(&ast, &[e])),
    }
}

fn format_actual_errors(ast: &ast::Program, errors: &[Error]) -> String {
    let mut v = vec![];
    {
        let mut emitter = codemap_diagnostic::Emitter::vec(&mut v, Some(ast.codemap()));
        emitter.emit(&errors.iter().map(|e| e.0.clone()).collect_vec());
    }
    String::from_utf8(v)
        .unwrap_or_else(|e| format!("diagnostic message contains invalid UTF-8: {}", e))
}

fn assert_results_eq<T: fmt::Debug + PartialEq>(
    ast: &ast::Program,
    task_str: &str,
    actual_result: &Result<T, Vec<Error>>,
    expected_result: &Result<T, Vec<(&str, &str)>>,
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
            let (expected_loc, expected_msg) = expected_errors[0];
            panic!(
                "Expected error at {:?} with message {:?} but got {:?} when {}",
                expected_loc, expected_msg, actual_ok, task_str,
            );
        }

        (Err(actual_errors), Ok(expected_ok)) => {
            let formatted_actual_errors = format_actual_errors(&ast, actual_errors);
            panic!(
                "Expected {:?} but got error when {}:\n{}",
                expected_ok, task_str, formatted_actual_errors,
            );
        }

        (Err(actual_errors), Err(expected_errors)) => {
            for (actual_error, (expected_loc, expected_msg)) in
                actual_errors.iter().zip(expected_errors.iter())
            {
                let span = actual_error.0.spans[0].span;
                let file = ast.codemap().look_up_span(span).file;
                if *expected_loc != file.source_slice(span)
                    || *expected_msg != actual_error.0.message
                {
                    let formatted_actual_errors = format_actual_errors(&ast, actual_errors);
                    panic!(
                        "Expected error at {:?} with message {:?} but got a different error when {}:\n{}",
                        expected_loc, expected_msg, task_str, formatted_actual_errors,
                    );
                }
            }
        }
    }
}
