//! NDCA black-box test suite.

use std::borrow::Cow;

mod cmp;
mod math;
mod vars;

use super::ast;
use super::errors::*;
use super::types::LangCellState;
use super::{compiler, interpreter};
use ast::Rule;

#[test]
fn test_become() {
    assert_output(
        Ok(10),
        "
        @transition {
            become #10
        }
        ",
    );
}

struct TestOutput {
    interpreted_output: CompleteLangResult<LangCellState>,
    compiled_output: CompleteLangResult<LangCellState>,
}

/// Compile and run the given source code
fn assert_output<'a>(expected: Result<LangCellState, &'a str>, source_code: &str) {
    let expected_result = expected.map_err(|e| e.into());

    let rule = ast::make_ndca_rule(source_code);
    let actual_result: Result<LangCellState, Cow<'a, str>> = match rule {
        Ok(rule) => {
            let TestOutput {
                interpreted_output,
                compiled_output,
            } = run_rule(rule);
            // Make sure that the output is the same.
            assert!(
                interpreted_output == compiled_output,
                "\n\nInterpreted output:\n{}\n\nCompiled output:\n{}\n\nInterpreted and compiled output differ when running this rule:\n{}\n\n",
                display_result(&interpreted_output),
                display_result(&compiled_output),
                source_code
            );
            interpreted_output.map_err(|e| e.to_string().into())
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

fn display_result<T: std::fmt::Display, E: std::fmt::Display>(result: &Result<T, E>) -> String {
    match result {
        Ok(t) => format!("{}", t),
        Err(e) => format!("{}", e),
    }
}

/// Run the given rule using the interpreter and the compiler and return the results
/// of each.
fn run_rule(rule: Rule) -> TestOutput {
    // Interpret the rule.
    let interpreted_output = interpreter::run_rule(rule.clone());
    // Compile the rule.
    let compiled_output = compiler::jit_compile_rule(rule)
        .and_then(|compiled_rule| compiled_rule.call_transition_fn());

    TestOutput {
        interpreted_output,
        compiled_output,
    }
}
