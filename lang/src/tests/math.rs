use proptest::prelude::*;
use std::convert::TryInto;

use super::{
    assert_fn_result, assert_threadlocal_fn_result, compile_test_fn, test_values, CompiledFunction,
    ConstValue, LangInt,
};

#[test]
fn test_integer_new() {
    let mut f = compile_test_fn("@function Int test() { return Integer.new() }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

thread_local! {
    static ADD_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x + y }");
    static SUB_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x - y }");
    static MUL_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x * y }");
    static DIV_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x / y }");
    static MOD_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x % y }");
    static PLUS_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return +x }");
    static NEG_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return -x }");

    static BIT_OR_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x | y }");
    static BIT_XOR_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x ^ y }");
    static BIT_AND_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x & y }");
    static BIT_NOT_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return ~x }");

    static LSH_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x << y }");
    static RSH_ARITH_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x >> y }");
    static RSH_LOGIC_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return x >>> y }");

    static MIN_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return min(x, y) }");
    static MAX_FN: CompiledFunction =
        compile_test_fn("@function Int test(Int x, Int y) { return max(x, y) }");
}

// Test with random inputs.
proptest! {
    #[test]
    fn proptest_arithmetic(x: LangInt, y: LangInt) {
        test_arithmetic(x, y);
    }
}

// And make sure to cover several corner cases (e.g. division by zero).
#[test]
fn test_arithmetic_corner_cases() {
    for &x in test_values() {
        for &y in test_values() {
            test_arithmetic(x, y);
        }
    }
}

#[test]
fn test_bitshift_corner_cases() {
    for &x in test_values() {
        for &y in &[-65, -64, -63, -62, 62, 63, 64, 65] {
            test_arithmetic(x, y);
        }
    }
}

fn test_arithmetic(x: LangInt, y: LangInt) {
    println!("Testing arithmetic with inputs {:?}", (x, y));
    let args = [ConstValue::Int(x), ConstValue::Int(y)];

    let overflow_err_msg = "Integer overflow";
    let div_err_msg = if y == 0 {
        "Divide by zero"
    } else {
        overflow_err_msg
    };

    // Addition
    let expected = x
        .checked_add(y)
        .map(ConstValue::Int)
        .ok_or(("x + y", overflow_err_msg));
    assert_threadlocal_fn_result(&ADD_FN, &args, expected);
    // Subtraction
    let expected = x
        .checked_sub(y)
        .map(ConstValue::Int)
        .ok_or(("x - y", overflow_err_msg));
    assert_threadlocal_fn_result(&SUB_FN, &args, expected);
    // Multiplication
    let expected = x
        .checked_mul(y)
        .map(ConstValue::Int)
        .ok_or(("x * y", overflow_err_msg));
    assert_threadlocal_fn_result(&MUL_FN, &args, expected);
    // Division
    let expected = x
        .checked_div(y)
        .map(ConstValue::Int)
        .ok_or(("x / y", div_err_msg));
    assert_threadlocal_fn_result(&DIV_FN, &args, expected);
    // Remainder
    let expected = x
        .checked_rem(y)
        .map(ConstValue::Int)
        .ok_or(("x % y", div_err_msg));
    assert_threadlocal_fn_result(&MOD_FN, &args, expected);
    // Unary plus (no-op)
    let expected = Ok(ConstValue::Int(x));
    assert_threadlocal_fn_result(&PLUS_FN, &args, expected);
    // Negation
    let expected = x
        .checked_neg()
        .map(ConstValue::Int)
        .ok_or(("-x", overflow_err_msg));
    assert_threadlocal_fn_result(&NEG_FN, &args, expected);

    // Bitwise OR
    let expected = Ok(ConstValue::Int(x | y));
    assert_threadlocal_fn_result(&BIT_OR_FN, &args, expected);
    // Bitwise XOR
    let expected = Ok(ConstValue::Int(x ^ y));
    assert_threadlocal_fn_result(&BIT_XOR_FN, &args, expected);
    // Bitwise AND
    let expected = Ok(ConstValue::Int(x & y));
    assert_threadlocal_fn_result(&BIT_AND_FN, &args, expected);
    // Bitwise NOT
    let expected = Ok(ConstValue::Int(!x));
    assert_threadlocal_fn_result(&BIT_NOT_FN, &args, expected);

    // Shift left
    let expected = y
        .try_into()
        .ok()
        .and_then(|y| x.checked_shl(y))
        .map(ConstValue::Int)
        .ok_or(("x << y", div_err_msg));
    assert_threadlocal_fn_result(&LSH_FN, &args, expected);
    // Shift right arithmetic
    let expected = y
        .try_into()
        .ok()
        .and_then(|y| x.checked_shr(y))
        .map(ConstValue::Int)
        .ok_or(("x >> y", div_err_msg));
    assert_threadlocal_fn_result(&RSH_ARITH_FN, &args, expected);
    // Shift right logical
    let expected = y
        .try_into()
        .ok()
        .and_then(|y| (x as u64).checked_shr(y))
        .map(|i| i as LangInt)
        .map(ConstValue::Int)
        .ok_or(("x >>> y", div_err_msg));
    assert_threadlocal_fn_result(&RSH_LOGIC_FN, &args, expected);

    // Maximum
    let expected = Ok(ConstValue::Int(std::cmp::max(x, y)));
    assert_threadlocal_fn_result(&MAX_FN, &args, expected);
    // Minimum
    let expected = Ok(ConstValue::Int(std::cmp::min(x, y)));
    assert_threadlocal_fn_result(&MIN_FN, &args, expected);
}

#[test]
fn test_abs() {
    let mut f = compile_test_fn(
        "@function Void test(Int x) {
            if x < 0 {
                assert abs(x) == x.abs == -x
            } else {
                assert abs(x) == x.abs == x
            }
        }",
    );
    for &x in test_values() {
        let expected;
        if x == LangInt::MIN {
            expected = Err(("abs(x)", "Integer overflow"))
        } else {
            expected = Ok(ConstValue::Void);
        }
        assert_fn_result(&mut f, &[ConstValue::Int(x)], expected);
    }
}

#[test]
fn test_min_and_max() {
    // Test with four arguments.
    let mut f = compile_test_fn(
        "@function Vec2 test(Vec4 values) {
            x = values.x
            y = values.y
            z = values.z
            w = values.w
            return [max(x, y, z, w), min(x, y, z, w)]
        }",
    );
    for (&w, &x, &y, &z) in iproduct!(test_values(), test_values(), test_values(), test_values()) {
        use std::cmp::{max, min};
        let max = max(max(x, y), max(z, w));
        let min = min(min(x, y), min(z, w));
        assert_fn_result(
            &mut f,
            &[ConstValue::Vector(vec![x, y, z, w])],
            Ok(ConstValue::Vector(vec![max, min])),
        )
    }

    // Test with vector/mixed arguments.
    let mut f = compile_test_fn(
        "@function Vec6 test(Vec2 a, Int b, Vec3 c) {
            return [max(a, b, c), min(a, b, c)]
        }",
    );
    let test_values = [-10, -5, 0, 5, 10];
    for (&ax, &ay, &b, &cx, &cy, &cz) in iproduct!(
        &test_values,
        &test_values,
        &test_values,
        &test_values,
        &test_values,
        &test_values
    ) {
        use std::cmp::{max, min};
        let max_x = max(max(ax, b), cx);
        let max_y = max(max(ay, b), cy);
        let max_z = max(max(0, b), cz);
        let min_x = min(min(ax, b), cx);
        let min_y = min(min(ay, b), cy);
        let min_z = min(min(0, b), cz);
        let a = ConstValue::Vector(vec![ax, ay]);
        let b = ConstValue::Int(b);
        let c = ConstValue::Vector(vec![cx, cy, cz]);
        assert_fn_result(
            &mut f,
            &[a, b, c],
            Ok(ConstValue::Vector(vec![
                max_x, max_y, max_z, min_x, min_y, min_z,
            ])),
        )
    }
}
