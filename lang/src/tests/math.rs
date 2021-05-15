use std::convert::TryFrom;

use super::*;
use RtVal::Integer;

/// Generates a test for a binary integer operator.
macro_rules! integer_expr_test_fn {
    {
        fn $fn_name:ident() { $expr_code_str:tt == $expected:expr }
        from $test_values_expr:expr
    } => {
        #[test]
        fn $fn_name() {
            test_expr(
                $expr_code_str,
                &[Type::Integer, Type::Integer, Type::Integer],
                &iproduct!($test_values_expr, $test_values_expr)
                    .map(|(&a, &b)| {
                        let inputs = vec![Integer(a), Integer(b)];
                        let expected_output = $expected(a, b);
                        (inputs, expected_output)
                    })
                    .collect_vec(),
            );
        }
    };
}
/// Generates tests for binary integer operators that may cause an overflow
/// error.
macro_rules! integer_binop_test_fn_with_overflow {
    {
        $(fn $fn_name:ident() { x0 $op_expr:tt x1 == $checked_op_func:expr })+
    } => {
        $(integer_expr_test_fn!{
            fn $fn_name() {
                (stringify!(x0 $op_expr x1))
                    == |a, b| match $checked_op_func(a, b) {
                        Some(result) => Ok(Integer(result)),
                        None => Err(&[(stringify!($op_expr), "integer overflow")][..]),
                    }
            } from test_values::<LangInt>()
        })+
    };
}
/// Generates tests for binary integer operators that may cause overflow or
/// division-by-zero errors.
macro_rules! integer_binop_test_fn_with_division {
    (
        $(fn $fn_name:ident() { x0 $op_expr:tt x1 == $checked_op_func:expr })+
    ) => {
        $(integer_expr_test_fn!{
            fn $fn_name() {
                (stringify!(x0 $op_expr x1))
                    == |a, b| match $checked_op_func(a, b) {
                        Some(result) => Ok(Integer(result)),
                        None if b == 0 => Err(&[(stringify!($op_expr), "division by zero")][..]),
                        None => Err(&[(stringify!($op_expr), "integer overflow")][..]),
                    }
            } from test_values::<LangInt>()
        })+
    };

    ($($t:tt)+) => {
        integer_binop_test_fn!($($t)+ divcheck 0);
    };
}

integer_binop_test_fn_with_overflow! {
    fn test_integer_add() { x0 + x1 == LangInt::checked_add }
    fn test_integer_sub() { x0 - x1 == LangInt::checked_sub }
    fn test_integer_mul() { x0 * x1 == LangInt::checked_mul }
}

integer_binop_test_fn_with_division! {
    fn test_integer_div() { x0 / x1 == LangInt::checked_div_euclid }
    fn test_integer_rem() { x0 % x1 == LangInt::checked_rem_euclid }
}

integer_expr_test_fn! {
    fn test_integer_pow() {
        "x0 ** x1"
            == |a: LangInt, b: LangInt| {
                match crate::utils::checked_pow_i64(a, b) {
                    Some(result) => Ok(Integer(result)),
                    None if b < 0 => Err(&[("**", "negative exponent")][..]),
                    None => Err(&[("**", "integer overflow")][..])
                }
            }
    } from &[
        // `test_values()` isn't great for testing exponentiation, so use these
        // values instead.
        LangInt::MIN,
        LangInt::MIN + 1,
        -100,
        -55,
        -10,
        -5,
        -2,
        -1,
        0,
        1,
        2,
        5,
        10,
        55,
        100,
        LangInt::MAX - 1,
        LangInt::MAX,
    ]
}
