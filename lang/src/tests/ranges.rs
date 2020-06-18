use super::{assert_fn_result, compile_test_fn, ConstValue, LangInt};

fn args(args: &[LangInt]) -> Vec<ConstValue> {
    args.iter().copied().map(ConstValue::Int).collect()
}
fn expected_range<T>(start: LangInt, end: LangInt, step: LangInt) -> Result<ConstValue, T> {
    Ok(ConstValue::IntRange { start, end, step })
}

#[test]
fn test_range_construct() {
    let mut f = compile_test_fn("@function range test(int x, int y) { return x..y }");
    // Test with start < end.
    assert_fn_result(&mut f, &args(&[1, 10]), expected_range(1, 10, 1));
    assert_fn_result(&mut f, &args(&[-5, 10]), expected_range(-5, 10, 1));
    // Test with start > end.
    assert_fn_result(&mut f, &args(&[10, 1]), expected_range(10, 1, -1));
    assert_fn_result(&mut f, &args(&[5, -10]), expected_range(5, -10, -1));
    // Test with start == end.
    assert_fn_result(&mut f, &args(&[2, 2]), expected_range(2, 2, 1));
    assert_fn_result(&mut f, &args(&[-1, -1]), expected_range(-1, -1, 1));
    assert_fn_result(&mut f, &args(&[0, 0]), expected_range(0, 0, 1));

    // Test that expressions in parentheses are parsed correctly.
    let mut f = compile_test_fn("@function range test(int x, int y) { return (x)..(y) }");
    assert_fn_result(&mut f, &args(&[-3, 5]), expected_range(-3, 5, 1));

    // Test that negative numbers are parsed correctly.
    let mut f = compile_test_fn("@function range test() { return -5..-10 }");
    assert_fn_result(&mut f, &[], expected_range(-5, -10, -1));

    // Test that negated expressions are parsed correctly.
    let mut f = compile_test_fn("@function range test(int x, int y) { return -x..-y }");
    assert_fn_result(&mut f, &args(&[-3, 5]), expected_range(3, -5, -1));
}

#[test]
fn test_range_step_by() {
    let mut f =
        compile_test_fn("@function range test(int x, int y, int z) { return (x..y).by(z) }");
    assert_fn_result(&mut f, &args(&[1, 10, 2]), expected_range(1, 10, 2));
    assert_fn_result(&mut f, &args(&[-1, -10, 2]), expected_range(-1, -10, 2));
    assert_fn_result(&mut f, &args(&[1, 10, -2]), expected_range(1, 10, -2));
    assert_fn_result(&mut f, &args(&[-1, -10, -2]), expected_range(-1, -10, -2));
}

#[test]
fn test_range_attributes() {
    todo!("test all range attributes");
}

#[test]
fn test_range_ops() {
    todo!("test negation and addition/subtraction/multiplication by an integer");
}
