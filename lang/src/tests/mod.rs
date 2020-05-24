//! NDCA black-box test suite.

use std::borrow::Cow;
use std::rc::Rc;

mod cmp;
mod math;
mod vars;
mod vecs;

use super::ast;
use super::compiler::Compiler;
use super::types::LangInt;
use super::ConstValue;

#[test]
fn test_become() {
    assert_output(
        Ok(ConstValue::CellState(10)),
        "
        @transition {
            become #10
        }
        @states 11",
    );
    assert_output(
        Err("Error at line 3; column 20
become #10
       ^^^   Cell state out of range"),
        "
        @transition {
            become #10
        }
        @states 10",
    );
    assert_output(
        Err("Error at line 3; column 20
become #10
       ^^^   Cell state out of range"),
        "
        @transition {
            become #10
        }",
    );
}

/// Compiles and runs the transition function of the given source code.
fn assert_output<'a>(expected: Result<ConstValue, &'a str>, source_code: &str) {
    assert_func_output(&[], expected, source_code, None)
}

/// Compiles and runs the specified function of the given source code.
fn assert_func_output<'a>(
    args: &[ConstValue],
    expected: Result<ConstValue, &'a str>,
    source_code: &str,
    fn_name: Option<&str>,
) {
    let expected_result = expected.map_err(|e| e.into());

    let rule = ast::make_rule(Rc::new(source_code.to_owned()));
    let actual_result: Result<ConstValue, Cow<'a, str>> = match rule {
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
                .map_err(|e| e.with_source(source_code).to_string().into())
        }
        Err(e) => Err(e.with_source(source_code).to_string().into()),
    };

    assert!(
        expected_result == actual_result,
        "\n\nExpected:\n{}\n\nGot:\n{}\n\nRule source code:\n{}\n\n",
        display_result(&expected_result),
        display_result(&actual_result),
        source_code
    );
}

fn display_result<T: std::fmt::Debug, E: std::fmt::Display>(result: &Result<T, E>) -> String {
    match result {
        Ok(t) => format!("{:?}", t),
        Err(e) => format!("{}", e),
    }
}
