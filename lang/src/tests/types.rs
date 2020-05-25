use super::{assert_compile_error, compile_test_fn};

/// Tests type checking on variables.
#[test]
fn test_variable_types() {
    // Cell state -> integer
    let source_code = "@function int test() {
        set s = #2
        set s = 3
    }";
    let expected = ("3", "Type error: expected cellstate but got integer");
    assert_compile_error(source_code, expected);

    // Integer -> cell state
    let source_code = "@function int test() {
        set s = 3
        set s = #2
    }";
    let expected = ("#2", "Type error: expected integer but got cellstate");
    assert_compile_error(source_code, expected);
}

/// Tests 'become' vs. 'return'.
#[test]
fn test_return_vs_become_error() {
    let source_code = "@function int test() { become 0 }";
    let expected = (
        "become 0",
        "Use 'return' instead of 'become' outside of transition functions",
    );
    assert_compile_error(source_code, expected);

    let source_code = "@transition { return #0 }";
    let expected = (
        "return #0",
        "Use 'become' instead of 'return' in transition functions",
    );
    assert_compile_error(source_code, expected);
}

/// Tests type checking on return type.
#[test]
fn test_return_type_error() {
    let source_code = "@transition { become 0 }";
    let expected = ("0", "Type error: expected cellstate but got integer");
    assert_compile_error(source_code, expected);

    let source_code = "@function cellstate test(int x) { return x }";
    let expected = ("x", "Type error: expected cellstate but got integer");
    assert_compile_error(source_code, expected);
    let source_code = "@function cellstate test() { return 0 }";
    let expected = ("0", "Type error: expected cellstate but got integer");
    assert_compile_error(source_code, expected);

    let source_code = "@function int test(cellstate x) { return x }";
    let expected = ("x", "Type error: expected integer but got cellstate");
    assert_compile_error(source_code, expected);
    let source_code = "@function int test() { return #0 }";
    let expected = ("#0", "Type error: expected integer but got cellstate");
    assert_compile_error(source_code, expected);
}

/// Tests type checking on conditional statementts.
#[test]
fn test_condition_type_error() {
    let source_code = "@function int test() { if #1 {} }";
    let expected = ("#1", "Type error: expected integer but got cellstate");
    assert_compile_error(source_code, expected);
}

/// Tests type checking between vectors.
#[test]
fn test_vector_types() {
    let source_code = "@function int test() {
        set x = [1, 2, 3]
        set y = [4, 5, 6]
        set x = y
    }";
    compile_test_fn(source_code); // no error

    let source_code = "@function int test() {
        set x = [1, 2, 3]
        set y = [1]
        set x = y
    }";
    let expected = ("y", "Type error: expected vector3 but got vector1");
    assert_compile_error(source_code, expected);

    let source_code = "@function int test() {
        set x = [1, 2, 3]
        set x = [1, 2, 3, 4]
    }";
    let expected = (
        "[1, 2, 3, 4]",
        "Type error: expected vector3 but got vector4",
    );
    assert_compile_error(source_code, expected);

    let source_code = "@function int test() {
        set x = [1]
        set x = 12
    }";
    let expected = ("12", "Type error: expected vector1 but got integer");
    assert_compile_error(source_code, expected);
}
