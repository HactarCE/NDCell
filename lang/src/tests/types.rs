use super::{assert_compile_error, compile_test_fn};

/// Tests type checking on variables.
#[test]
fn test_variable_types() {
    // CellState -> Integer
    let source_code = "@function Int test() {
        set s = #2
        set s = 3
    }";
    let expected = ("3", "Type error: expected CellState but got Integer");
    assert_compile_error(source_code, expected);

    // Integer -> CellState
    let source_code = "@function Int test() {
        set s = 3
        set s = #2
    }";
    let expected = ("#2", "Type error: expected Integer but got CellState");
    assert_compile_error(source_code, expected);
}

/// Tests 'become' vs. 'return'.
#[test]
fn test_return_vs_become_error() {
    let source_code = "@function Int test() { become 0 }";
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
    let expected = ("0", "Type error: expected CellState but got Integer");
    assert_compile_error(source_code, expected);

    let source_code = "@function CellState test(Int x) { return x }";
    let expected = ("x", "Type error: expected CellState but got Integer");
    assert_compile_error(source_code, expected);
    let source_code = "@function Cell test() { return 0 }";
    let expected = ("0", "Type error: expected CellState but got Integer");
    assert_compile_error(source_code, expected);

    let source_code = "@function Integer test(Cell x) { return x }";
    let expected = ("x", "Type error: expected Integer but got CellState");
    assert_compile_error(source_code, expected);
    let source_code = "@function Int test() { return #0 }";
    let expected = ("#0", "Type error: expected Integer but got CellState");
    assert_compile_error(source_code, expected);
}

/// Tests type checking on conditional statementts.
#[test]
fn test_condition_type_error() {
    let source_code = "@function Int test() { if #1 {} }";
    let expected = ("#1", "Type error: expected Integer but got CellState");
    assert_compile_error(source_code, expected);
}

/// Tests type checking between vectors.
#[test]
fn test_vector_types() {
    let source_code = "@function Int test() {
        set x = [1, 2, 3]
        set y = [4, 5, 6]
        set x = y
    }";
    compile_test_fn(source_code); // no error

    let source_code = "@function Int test() {
        set x = [1, 2, 3]
        set y = [1]
        set x = y
    }";
    let expected = ("y", "Type error: expected Vector3 but got Vector1");
    assert_compile_error(source_code, expected);

    let source_code = "@function Int test() {
        set x = [1, 2, 3]
        set x = [1, 2, 3, 4]
    }";
    let expected = (
        "[1, 2, 3, 4]",
        "Type error: expected Vector3 but got Vector4",
    );
    assert_compile_error(source_code, expected);

    let source_code = "@function Int test() {
        set x = [1]
        set x = 12
    }";
    let expected = ("12", "Type error: expected Vector1 but got Integer");
    assert_compile_error(source_code, expected);
}
