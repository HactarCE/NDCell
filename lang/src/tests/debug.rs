use super::*;

use RtVal::Integer;

#[test]
fn test_user_error() {
    TestProgram::new()
        .with_exec("error")
        .assert_test_cases::<&str>(test_cases![() => Err("error" @ "error")]);
    TestProgram::new()
        .with_exec("error \"some message\"")
        .assert_test_cases::<&str>(
            test_cases![() => Err("some message" @ "error \"some message\"")],
        );
}

#[test]
fn test_user_assertion() {
    TestProgram::new()
        .with_exec("assert")
        .assert_syntax_error(test_error!("expected an expression" @ "}"));
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_exec("assert x0")
        .assert_test_cases::<&str>(test_cases![
            (Integer(0)) => Err("assertion failed" @ "assert x0"),
            (Integer(10)) => Ok(),
        ]);
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_exec("assert x0, \"some message\"")
        .assert_test_cases::<&str>(test_cases![
            (Integer(0)) => Err("some message" @ "assert x0, \"some message\""),
            (Integer(-1)) => Ok(),
        ]);
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_exec("assert x0, 5")
        .assert_syntax_error(test_error!("expected a string literal" @ "5"));
}
