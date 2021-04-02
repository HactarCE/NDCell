//! NDCA black-box test suite.

use codemap::Spanned;
use itertools::Itertools;
use std::fmt;
use std::sync::Arc;
use std::thread::LocalKey;

// mod bools;
// mod branch;
// mod byterepr;
// mod cmp;
// mod debug;
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
use crate::data::{LangInt, Value};
use crate::errors::Error;
use crate::runtime::{Runtime, RuntimeError};
use values::*;

fn test_expr(
    source: &str,
    inputs_and_expected_results: &[(Vec<Value>, Result<Value, &[(&str, &str)]>)],
) {
    let mut ast = ast::Program::new();
    let file = ast.add_file("expression.test".to_owned(), source.to_owned());
    let expr_id = match crate::parser::parse_expression(&mut ast, &file) {
        Ok(id) => id,
        Err(e) => panic!("NDCA syntax error:\n{}", format_actual_errors(&ast, &[e])),
    };
    let expr = ast.get_node(expr_id);

    let mut runtime = Runtime::new();
    for (inputs, expected_result) in inputs_and_expected_results {
        let task_str = &format!("evaluating {:?} with inputs {:?}", source, inputs);
        // Set input variables.
        for (i, value) in inputs.iter().enumerate() {
            let var_name = Arc::new(format!("x{}", i));
            runtime.vars.insert(var_name, value.clone());
        }
        // Evaluate expression.
        let mut errors = vec![];
        let actual_result = runtime
            .eval_expr(expr)
            .map(|v| v.node)
            .map_err(|e| match e {
                RuntimeError::CannotConstEval(_) => panic!("cannot const eval when {}", task_str),
                RuntimeError::Other(e) => {
                    errors.push(e);
                    &errors[..]
                }
            });
        // Verify result.
        assert_results_eq(&ast, task_str, &actual_result, expected_result);
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
    actual_result: &Result<T, &[Error]>,
    expected_result: &Result<T, &[(&str, &str)]>,
) {
    match (actual_result, expected_result) {
        (_, Err([])) => panic!("empty expected error list is invalid"),

        (Ok(actual_ok), Ok(expected_ok)) => {
            if *expected_ok != *actual_ok {
                panic!(
                    "Expected {:?} but got {:?} when {}",
                    expected_ok, actual_ok, task_str,
                );
            }
        }

        (Ok(actual_ok), Err([(expected_loc, expected_msg), ..])) => {
            panic!(
                "Expected error at {:?} with message {:?} but got {:?} when {}",
                expected_loc, expected_msg, actual_ok, task_str,
            );
        }

        (Err(actual_errors), Ok(expected_ok)) => {
            let formatted_actual_err = format_actual_errors(&ast, actual_errors);
            panic!(
                "Expected {:?} but got error when {}:\n{}",
                expected_ok, task_str, formatted_actual_err,
            );
        }

        (Err(actual_errors), Err(expected_errors)) => {
            for (actual_error, (expected_loc, expected_msg)) in
                actual_errors.iter().zip(expected_errors.iter())
            {
                let span = actual_errors[0].0.spans[0].span;
                let file = ast.codemap().look_up_span(span).file;
                if *expected_loc != file.source_slice(span)
                    || *expected_msg != actual_errors[0].0.message
                {
                    let formatted_actual_err = format_actual_errors(&ast, actual_errors);
                    panic!(
                        "Expected error at {:?} with message {:?} but got a different error when {}:\n{}",
                        expected_loc, expected_msg, task_str, formatted_actual_err,
                    );
                }
            }
        }
    }
}

// /// Asserts that the given source code produces the given error when attempting
// /// to produce an AST.
// fn assert_compile_error(source_code: &str, expected: (&str, &str)) {
//     let expected_err = (expected.0.to_owned(), expected.1.to_owned());
//     let actual_result = make_ast(source_code);
//     match actual_result {
//         Ok(rule) => assert!(false, "Rule should have produced error {:?}, but instead it produced this AST:\n{:#?}\n\nRule source code:\n{}\n\n", expected_err, rule, source_code),
//         Err(actual_err) => assert_eq!(
//             expected_err,
//             actual_err.pair(),
//             "<-- actual\n\nRule source code:\n{}\n\n",
//             source_code,
//         )
//     }
// }

// /// Asserts that the given source code produces the given error when attempting
// /// to compile the function with the given name (or the transition function, if
// /// the function name is None).
// fn assert_fn_compile_error(fn_name: Option<&str>, source_code: &str, expected: (&str, &str)) {
//     let expected_err = (expected.0.to_owned(), expected.1.to_owned());
//     let actual_result = compile_fn(fn_name, source_code);
//     match actual_result {

//         Ok(_) => assert!(false, "Rule should have produced error {:?}, but instead it compiled successfully.\n\nRule source code:\n{}\n\n", expected_err,  source_code),
//         Err(actual_err) => assert_eq!(
//             expected_err,
//             actual_err.pair(),
//             "<-- actual\n\nRule source code:\n{}\n\n",
//             source_code,
//         )
//     }
// }

// /// Same as assert_fn_result(), but works with LocalKey for use with
// /// threadlocal!().
// fn assert_threadlocal_fn_result(
//     function: &'static LocalKey<CompiledFunction>,
//     args: &mut [ConstValue],
//     expected: Result<ConstValue, (&str, &str)>,
// ) {
//     let mut function = function.with(|f| f.clone());
//     assert_fn_result(&mut function, args, expected);
// }

// /// Calls the given compiled function with the given arguments and asserts that
// /// it produces the correct output (whether Err or Ok).
// fn assert_fn_result(
//     function: &mut CompiledFunction,
//     args: &mut [ConstValue],
//     expected: Result<ConstValue, (&str, &str)>,
// ) {
//     let expected_result = expected.map_err(|(a, b)| (a.to_owned(), b.to_owned()));
//     let actual_result = function
//         .call(args)
//         .map_err(|e| e.with_source(&function.rule_meta().source_code));
//     assert_eq!(
//         expected_result,
//         actual_result.map_err(|e| e.pair()),
//         "<-- actual\n\nRule source code:\n{}\n\n",
//         function.rule_meta().source_code,
//     );
// }

// /// Compiles the function named "test" in the given source code, panicking if compilation fails.
// fn compile_test_fn(source_code: &str) -> CompiledFunction {
//     compile_fn(Some("test"), source_code).expect("Compilation failed")
// }

// /// Compiles the transition function in the given source code, panicking if compilation fails.
// fn compile_transition_fn(source_code: &str) -> CompiledFunction {
//     compile_fn(None, source_code).expect("Compilation failed")
// }

// /// Compiles the specified function of the given source code. If the function
// /// name is None, then the transition function is returned.
// fn compile_fn(fn_name: Option<&str>, source_code: &str) -> CompleteLangResult<CompiledFunction> {
//     crate::compile_blocking(Arc::new(source_code.to_owned()), fn_name.map(str::to_owned))
//         .map_err(|e| e.with_source(source_code))
// }

// /// Constructs a Rule AST from the given source code.
// fn make_ast(source_code: &str) -> CompleteLangResult<ast::Rule> {
//     ast::make_rule(Arc::new(source_code.to_owned())).map_err(|e| e.with_source(source_code))
// }
