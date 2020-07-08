use super::{assert_fn_result, compile_test_fn, ConstValue};

/// Tests custom errors.
#[test]
fn test_errors() {
    let mut f = compile_test_fn(
        "@function Void test(Int x) {
            if x < 0 {
                error 'x is negative! very scary'
            } else if x {
                error
            }
        }",
    );

    let expected = Err(("error", r#"Error"#));
    assert_fn_result(&mut f, &[ConstValue::Int(5)], expected);

    let expected = Err((
        "error 'x is negative! very scary'",
        r#"Error: "x is negative! very scary""#,
    ));
    assert_fn_result(&mut f, &[ConstValue::Int(-5)], expected);

    let expected = Ok(ConstValue::Void);
    assert_fn_result(&mut f, &[ConstValue::Int(0)], expected);
}

/// Tests assertions.
#[test]
fn test_assertions() {
    // Test assert without message.
    let mut f = compile_test_fn(
        "@function Int test() {
            assert 3 == 3
            assert 3 == 2
        }",
    );
    let expected = Err(("3 == 2", "Assertion failed"));
    assert_fn_result(&mut f, &[], expected);

    // Test assert with message.
    let mut f = compile_test_fn(
        "@function Int test() {
            assert 3 == 3, 'broken'
            assert 3 == 2, 'ok'
        }",
    );
    let expected = Err(("3 == 2", r#"Assertion failed: "ok""#));
    assert_fn_result(&mut f, &[], expected);
}
