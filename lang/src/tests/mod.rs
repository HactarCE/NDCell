//! NDCA black-box test suite.

use std::rc::Rc;

// mod cmp;
mod math;
// mod vars;
// mod vecs;

use super::ast;
use super::compiler::{CompiledFunction, Compiler};
use super::errors::CompleteLangResult;
use super::types::LangInt;
use super::ConstValue;

/// Integer corner cases to be used as inputs for testing.
const INT_TEST_VALUES: &[LangInt] = &[
    LangInt::MIN,
    LangInt::MIN + 1,
    -2,
    -1,
    0,
    1,
    2,
    LangInt::MAX - 1,
    LangInt::MAX,
];

// #[test]
// fn test_become() {
//     assert_output(
//         Ok(ConstValue::CellState(10)),
//         "
//         @transition {
//             become #10
//         }
//         @states 11",
//     );
//     assert_output(
//         Err("Error at line 3; column 20
// become #10
//        ^^^   Cell state out of range"),
//         "
//         @transition {
//             become #10
//         }
//         @states 10",
//     );
//     assert_output(
//         Err("Error at line 3; column 20
// become #10
//        ^^^   Cell state out of range"),
//         "
//         @transition {
//             become #10
//         }",
//     );
// }

/// Compiles the specified function of the given source code. If the function
/// name is None, then the transition function is returned.
fn compile(fn_name: Option<&str>, source_code: &str) -> CompleteLangResult<CompiledFunction> {
    let rule =
        ast::make_rule(Rc::new(source_code.to_owned())).map_err(|e| e.with_source(source_code))?;
    let user_fn = if let Some(name) = fn_name {
        &rule.helper_functions()[name]
    } else {
        rule.transition_function()
    };
    user_fn
        .compile(&mut Compiler::new().unwrap())
        .map_err(|e| e.with_source(source_code))
}

fn assert_fn_result(
    function: &CompiledFunction,
    args: &[ConstValue],
    expected: Result<ConstValue, (&str, &str)>,
) {
    let mut function = function.clone();
    let expected_result = expected.map_err(|(a, b)| (a.to_owned(), b.to_owned()));
    function.set_args(args);
    let actual_result = function
        .call()
        .map_err(|e| e.with_source(function.source_code()));
    assert_eq!(
        expected_result,
        actual_result.map_err(|e| e.pair()),
        "\n\nRule source code:\n{}\n\n",
        function.source_code()
    );
}

/// Compiles and runs the specified function of the given source code with the
/// given arguments. If a function name is not given, then the transition
/// function is called.
fn assert_output(
    args: &[ConstValue],
    expected: Result<ConstValue, (&str, &str)>,
    source_code: &str,
    fn_name: Option<&str>,
) {
    let expected_result = expected.map_err(|(a, b)| (a.to_owned(), b.to_owned()));

    let rule = ast::make_rule(Rc::new(source_code.to_owned()));
    let actual_result: CompleteLangResult<ConstValue> = match rule {
        Ok(rule) => {
            // Compile the rule.
            let mut compiler = Compiler::new().expect("Failed to create compiler");
            let user_fn = if let Some(name) = fn_name {
                &rule.helper_functions()[name]
            } else {
                rule.transition_function()
            };
            user_fn
                .compile(&mut compiler)
                .and_then(|mut compiled_function| {
                    compiled_function.set_args(args);
                    compiled_function.call()
                })
                .map_err(|e| e.with_source(source_code))
        }
        Err(e) => Err(e.with_source(source_code)),
    };

    assert_eq!(
        expected_result,
        actual_result.map_err(|e| e.pair()),
        "\n\nRule source code:\n{}\n\n",
        source_code
    );
}

fn display_result<T: std::fmt::Debug, E: std::fmt::Display>(result: &Result<T, E>) -> String {
    match result {
        Ok(t) => format!("{:?}", t),
        Err(e) => format!("{}", e),
    }
}
