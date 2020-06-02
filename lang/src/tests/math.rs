use proptest::prelude::*;
use std::convert::TryInto;

use super::{
    assert_fn_result, assert_threadlocal_fn_result, compile_test_fn, test_values, CompiledFunction,
    ConstValue, LangInt,
};

thread_local! {
    static ADD_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x + y }");
    static SUB_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x - y }");
    static MUL_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x * y }");
    static DIV_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x / y }");
    static MOD_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x % y }");
    static PLUS_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return +x }");
    static NEG_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return -x }");

    static BIT_OR_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x | y }");
    static BIT_XOR_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x ^ y }");
    static BIT_AND_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x & y }");

    static LSH_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x << y }");
    static RSH_ARITH_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x >> y }");
    static RSH_LOGIC_FN: CompiledFunction =
        compile_test_fn("@function int test(int x, int y) { return x >>> y }");
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
            println!("Testing arithmetic with inputs {:?}", (x, y));
            test_arithmetic(x, y);
        }
    }
}

fn test_arithmetic(x: LangInt, y: LangInt) {
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
}

#[test]
fn test_abs() {
    let mut f = compile_test_fn(
        "@function int test(int x) {
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
            expected = Ok(ConstValue::Int(0));
        }
        assert_fn_result(&mut f, &[ConstValue::Int(x)], expected);
    }
}
