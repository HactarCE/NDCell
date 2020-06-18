use super::{assert_fn_result, compile_test_fn, ConstValue, LangInt};

fn int_args(args: &[LangInt]) -> Vec<ConstValue> {
    args.iter().copied().map(ConstValue::Int).collect()
}
fn ok_range<T>(start: LangInt, end: LangInt, step: LangInt) -> Result<ConstValue, T> {
    Ok(ConstValue::IntRange { start, end, step })
}

#[test]
fn test_range_construct() {
    let mut f = compile_test_fn("@function range test(int x, int y) { return x..y }");
    // Test with start < end.
    assert_fn_result(&mut f, &int_args(&[1, 10]), ok_range(1, 10, 1));
    assert_fn_result(&mut f, &int_args(&[-5, 10]), ok_range(-5, 10, 1));
    // Test with start > end.
    assert_fn_result(&mut f, &int_args(&[10, 1]), ok_range(10, 1, -1));
    assert_fn_result(&mut f, &int_args(&[5, -10]), ok_range(5, -10, -1));
    // Test with start == end.
    assert_fn_result(&mut f, &int_args(&[2, 2]), ok_range(2, 2, 1));
    assert_fn_result(&mut f, &int_args(&[-1, -1]), ok_range(-1, -1, 1));
    assert_fn_result(&mut f, &int_args(&[0, 0]), ok_range(0, 0, 1));

    // Test that expressions in parentheses are parsed correctly.
    let mut f = compile_test_fn("@function range test(int x, int y) { return (x)..(y) }");
    assert_fn_result(&mut f, &int_args(&[-3, 5]), ok_range(-3, 5, 1));

    // Test that negative numbers are parsed correctly.
    let mut f = compile_test_fn("@function range test() { return -5..-10 }");
    assert_fn_result(&mut f, &[], ok_range(-5, -10, -1));

    // Test that negated expressions are parsed correctly.
    let mut f = compile_test_fn("@function range test(int x, int y) { return -x..-y }");
    assert_fn_result(&mut f, &int_args(&[-3, 5]), ok_range(3, -5, -1));
}

#[test]
fn test_range_step_by() {
    let mut f =
        compile_test_fn("@function range test(int x, int y, int z) { return (x..y).by(z) }");
    assert_fn_result(&mut f, &int_args(&[1, 10, 2]), ok_range(1, 10, 2));
    assert_fn_result(&mut f, &int_args(&[-1, -10, 2]), ok_range(-1, -10, 2));
    assert_fn_result(&mut f, &int_args(&[1, 10, -2]), ok_range(1, 10, -2));
    assert_fn_result(&mut f, &int_args(&[-1, -10, -2]), ok_range(-1, -10, -2));
}

#[test]
fn test_range_properties() {
    // Test start, end, and step.
    let mut f = compile_test_fn(
        "@function vec3 test(int x, int y, int z) {
            set r = (x..y).by(z)
            set r.start += 5
            set r.end += 10
            set r.step += 2
            return [r.start, r.end, r.step]
        }",
    );
    let (x, y, z) = (10, 60, 5);
    assert_fn_result(
        &mut f,
        &int_args(&[x, y, z]),
        Ok(ConstValue::Vector(vec![x + 5, y + 10, z + 2])),
    );
}

#[test]
fn test_range_ops() {
    let mut f = compile_test_fn(
        "@function int test() {
            set r = (-5..10).by(3)
            assert r == +r != -5..10
            set r += 5
            assert r == (0..15).by(3)
            set r -= 2
            assert r == (-2..13).by(3)
            set r *= 6
            assert r == ((-2 * 6)..(13 * 6)).by(3 * 6)
            set r = -r
            assert r == ((2 * 6)..(-13 * 6)).by(-3 * 6)
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}
