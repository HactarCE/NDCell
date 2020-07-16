use super::{assert_compile_error, assert_fn_result, compile_test_fn, ConstValue};

/// Tests that functions return a default value if there is no return statement.
#[test]
fn test_implicit_return_int() {
    let mut f = compile_test_fn("@function Int test() { }");
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));
}

/// Tests that undeclared variables produce an error when generating the AST.
#[test]
fn test_undeclared_variable_error() {
    let expected = ("x", "This variable must be initialized before it is used");

    let source_code = "@function Int test() { return x }";
    assert_compile_error(source_code, expected);

    let source_code = "@function Int test() { x += 1 }";
    assert_compile_error(source_code, expected);

    let source_code = "@function Int test() { y = x }";
    assert_compile_error(source_code, expected);
}

/// Tests that integer variables are automatically initialized to 0.
#[test]
fn test_var_init() {
    let mut f = compile_test_fn(
        "@function Int test() {
            if 0 { x = 5 } // never executed, but declares x as an integer
            return x
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));
}

/// Tests basic variable usage.
#[test]
fn test_variables() {
    let mut f = compile_test_fn(
        "@function Int test() {
            x = 3
            another_variable = 9
            x = another_variable - 2
            if 1 {
                return x + 3
            }
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(10)));
}

/// Tests throwaway variable.
#[test]
fn test_throwaway_variable() {
    // Can assign any type to it
    let mut f = compile_test_fn(
        "@function Void test() {
            _ = 99
            _ = #0
            _ = [1, 2, 3]
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));

    // Value is always void, even after assigning to it
    let mut f = compile_test_fn(
        "@function Void test() {
            _ = 3
            return _
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
}
