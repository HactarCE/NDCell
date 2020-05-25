use super::{assert_compile_error, assert_fn_result, compile_test_fn, ConstValue};

/// Tests that the AST generator gives a nicer error when the user forgets the
/// 'set' keyword.
#[test]
fn test_missing_set_error() {
    let expected = ("x", "Variable assignment requires the 'set' keyword");

    let source_code = "@function int test() { x = 3 }";
    assert_compile_error(source_code, expected);

    let source_code = "@function int test() { x *= 3 }";
    assert_compile_error(source_code, expected);
}

/// Tests that undeclared variables produce an error when generating the AST.
#[test]
fn test_undeclared_variable_error() {
    let expected = ("x", "This variable must be initialized before it is used");

    let source_code = "@function int test() { return x }";
    assert_compile_error(source_code, expected);

    let source_code = "@function int test() { set x += 1 }";
    assert_compile_error(source_code, expected);

    let source_code = "@function int test() { set y = x }";
    assert_compile_error(source_code, expected);
}

/// Tests that integer variables are automatically initialized to 0.
#[test]
fn test_var_init() {
    let mut f = compile_test_fn(
        "@function int test() {
            if 0 { set x = 5 } // never executed, but declares x as an integer
            return x
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

/// Tests basic variable usage.
#[test]
fn test_variables() {
    let mut f = compile_test_fn(
        "@function int test() {
            set x = 3
            set another_variable = 9
            set x = another_variable - 2
            if 1 {
                return x + 3
            }
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(10)));
}
