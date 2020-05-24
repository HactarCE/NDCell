use proptest::prelude::*;

use super::{assert_fn_result, compile, CompiledFunction, ConstValue, LangInt, INT_TEST_VALUES};

thread_local! {
    static ADD_FN: CompiledFunction =
        compile(Some("test"), "@function int test(int x, int y) { return x + y }").unwrap();
    static SUB_FN: CompiledFunction =
        compile(Some("test"), "@function int test(int x, int y) { return x - y }").unwrap();
    static MUL_FN: CompiledFunction =
        compile(Some("test"), "@function int test(int x, int y) { return x * y }").unwrap();
    static DIV_FN: CompiledFunction =
        compile(Some("test"), "@function int test(int x, int y) { return x / y }").unwrap();
    static MOD_FN: CompiledFunction =
        compile(Some("test"), "@function int test(int x, int y) { return x % y }").unwrap();
    static NEG_FN: CompiledFunction =
        compile(Some("test"), "@function int test(int x, int y) { return -x }").unwrap();
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
    for &x in INT_TEST_VALUES {
        for &y in INT_TEST_VALUES {
            println!("Testing arithmetic with inputs {:?}", (x, y));
            test_arithmetic(x, y);
        }
    }
}

fn test_arithmetic(x: LangInt, y: LangInt) {
    let args = [ConstValue::Int(x), ConstValue::Int(y)];

    let div_err_msg = if y == 0 {
        "Divide by zero"
    } else {
        "Integer overflow"
    };

    // Addition
    let expected = x
        .checked_add(y)
        .map(ConstValue::Int)
        .ok_or(("x + y", "Integer overflow"));
    ADD_FN.with(|f| assert_fn_result(f, &args, expected));

    // Subtraction
    let expected = x
        .checked_sub(y)
        .map(ConstValue::Int)
        .ok_or(("x - y", "Integer overflow"));
    SUB_FN.with(|f| assert_fn_result(f, &args, expected));

    // Multiplication
    let expected = x
        .checked_mul(y)
        .map(ConstValue::Int)
        .ok_or(("x * y", "Integer overflow"));
    MUL_FN.with(|f| assert_fn_result(f, &args, expected));

    // Division
    let expected = x
        .checked_div(y)
        .map(ConstValue::Int)
        .ok_or(("x / y", div_err_msg));
    DIV_FN.with(|f| assert_fn_result(f, &args, expected));

    // Remainder
    let expected = x
        .checked_rem(y)
        .map(ConstValue::Int)
        .ok_or(("x % y", div_err_msg));
    MOD_FN.with(|f| assert_fn_result(f, &args, expected));

    // Negation
    let expected = x
        .checked_neg()
        .map(ConstValue::Int)
        .ok_or(("-x", "Integer overflow"));
    NEG_FN.with(|f| assert_fn_result(f, &args, expected));
}
