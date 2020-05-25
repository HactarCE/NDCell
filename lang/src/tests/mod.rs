//! NDCA black-box test suite.

use std::rc::Rc;
use std::thread::LocalKey;

mod branch;
mod cmp;
mod debug;
mod math;
mod types;
mod values;
mod vars;
mod vecs;

use super::ast;
use super::compiler::{CompiledFunction, Compiler};
use super::errors::CompleteLangResult;
use super::types::LangInt;
use super::ConstValue;
use values::*;

/// Asserts that the given source code produces the given error when attempting
/// to produce an AST.
fn assert_compile_error(source_code: &str, expected: (&str, &str)) {
    let expected_err = (expected.0.to_owned(), expected.1.to_owned());
    let actual_result = make_ast(source_code);
    match actual_result {
        Ok(rule) => assert!(false, "Rule should have produced error {:?}, but instead it produced this AST:\n{:#?}\n\nRule source code:\n{}\n\n", expected_err, rule, source_code),
        Err(actual_err) => assert_eq!(
            expected_err,
            actual_err.pair(),
            "<-- actual\n\nRule source code:\n{}\n\n",
            source_code
        )
    }
}

/// Same as assert_fn_result(), but works with LocalKey for use with
/// threadlocal!().
fn assert_threadlocal_fn_result(
    function: &'static LocalKey<CompiledFunction>,
    args: &[ConstValue],
    expected: Result<ConstValue, (&str, &str)>,
) {
    let mut function = function.with(|f| f.clone());
    assert_fn_result(&mut function, args, expected);
}

/// Calls the given compiled function with the given arguments and asserts that
/// it produces the correct output (whether Err or Ok).
fn assert_fn_result(
    function: &mut CompiledFunction,
    args: &[ConstValue],
    expected: Result<ConstValue, (&str, &str)>,
) {
    let expected_result = expected.map_err(|(a, b)| (a.to_owned(), b.to_owned()));
    function.set_args(args);
    let actual_result = function
        .call()
        .map_err(|e| e.with_source(function.source_code()));
    assert_eq!(
        expected_result,
        actual_result.map_err(|e| e.pair()),
        "<-- actual\n\nRule source code:\n{}\n\n",
        function.source_code()
    );
}

/// Compiles the function named "test" in the given source code, panicking if compilation fails.
fn compile_test_fn(source_code: &str) -> CompiledFunction {
    compile_fn(Some("test"), source_code).expect("Compilation failed")
}

/// Compiles the specified function of the given source code. If the function
/// name is None, then the transition function is returned.
fn compile_fn(fn_name: Option<&str>, source_code: &str) -> CompleteLangResult<CompiledFunction> {
    let rule = make_ast(source_code)?;
    let user_fn = if let Some(name) = fn_name {
        &rule.helper_functions()[name]
    } else {
        rule.transition_function()
    };
    user_fn
        .compile(&mut Compiler::new().unwrap())
        .map_err(|e| e.with_source(source_code))
}

/// Constructs a Rule AST from the given source code.
fn make_ast(source_code: &str) -> CompleteLangResult<ast::Rule> {
    ast::make_rule(Rc::new(source_code.to_owned())).map_err(|e| e.with_source(source_code))
}
