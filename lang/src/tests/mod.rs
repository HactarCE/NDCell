//! NDCA black-box test suite.

use std::sync::Arc;
use std::thread::LocalKey;

// mod bools;
// mod branch;
// mod byterepr;
// mod cmp;
// emod debug;
// meod filters;
// moed funcs;
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
use crate::data::Value;
use crate::errors::Error;
// use crate::compiler::CompiledFunction;
// use crate::errors::CompleteLangResult;
// use crate::types::{CellStateFilter, LangInt, Pattern, PatternShape};
// use crate::ConstValue;
// use values::*;

fn test_expr(
    source: &str,
    inputs_and_expected_results: &[(&[Value], Result<Value, (&str, &str)>)],
) {
    let mut ast = ast::Program::new();
    let file = ast.add_file("expression.test".to_owned(), source.to_owned());
    let expr_id = match crate::parser::parse_expression(&mut ast, &file) {
        Ok(id) => id,
        Err(e) => panic!("NDCA syntax error:\n{}", format_actual_error(&ast, e)),
    };
    let expr = ast.get_node(expr_id);

    let mut runtime = crate::runtime::Runtime::new();
    for (inputs, expected_result) in inputs_and_expected_results {
        for (i, value) in inputs.iter().enumerate() {
            let var_name = Arc::new(format!("x{}", i));
            runtime.vars.insert(var_name, value.clone());
        }
        let actual_result = runtime.eval_expr(expr);

        match (actual_result, expected_result) {
            (Ok(actual_ok), Ok(expected_ok)) => {
                if *expected_ok != actual_ok.node {
                    panic!(
                        "Expected {:?} but got {:?} when evaluating {:?} with inputs {:?}",
                        expected_ok, actual_ok.node, source, inputs,
                    );
                }
            }

            (Ok(actual_ok), Err((expected_loc, expected_msg))) => {
                panic!(
                    "Expected error at {:?} with message {:?} but got {:?} when evaluating {:?} with inputs {:?}",
                    expected_loc, expected_msg, actual_ok.node, source, inputs,
                );
            }

            (Err(actual_err), Ok(expected_ok)) => {
                let formatted_actual_err = format_actual_error(&ast, actual_err);
                panic!(
                    "Expected {:?} but got error when evaluating {:?} with inputs {:?}:\n{}",
                    expected_ok, source, inputs, formatted_actual_err,
                );
            }

            (Err(actual_err), Err((expected_loc, expected_msg))) => {
                if *expected_loc != file.source_slice(actual_err.0.spans[0].span)
                    || *expected_msg != actual_err.0.message
                {
                    let formatted_actual_err = format_actual_error(&ast, actual_err);
                    panic!(
                        "Expected error at {:?} with message {:?} but got a different error when evaluating {:?} with inputs {:?}:\n{}",
                        expected_loc, expected_msg, source, inputs, formatted_actual_err,
                    );
                }
            }
        }
    }
}

fn format_actual_error(ast: &ast::Program, error: Error) -> String {
    let mut v = vec![];
    {
        let mut emitter = codemap_diagnostic::Emitter::vec(&mut v, Some(ast.codemap()));
        emitter.emit(&[error.0]);
    }
    String::from_utf8(v)
        .unwrap_or_else(|e| format!("diagnostic message contains invalid UTF-8: {}", e))
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
