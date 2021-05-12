use super::*;

use RtVal::Integer;

/// Generates a test for a binary integer operator.
macro_rules! integer_binop_test_fn {
    (fn $fn_name:ident() { x0 $op:tt x1 == $checked_fn_name:ident } $(with_check_for_div_by $zero:tt)? ) => {
        #[test]
        fn $fn_name() {
            test_expr(
                stringify!(x0 $op x1),
                &[Type::Integer, Type::Integer, Type::Integer],
                &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
                    .map(|(&a, &b)| {
                        let expected = match a.$checked_fn_name(b) {
                            Some(sum) => Ok(Integer(sum)),
                            $(
                                None if b == $zero => Err(&[(stringify!($op), "division by zero")][..]),
                            )?
                            None => Err(&[(stringify!($op), "integer overflow")][..]),
                        };

                        (vec![Integer(a), Integer(b)], expected)
                    })
                    .collect_vec(),
            );
        }
    };
}
/// Generates a test for a binary integer operator that causes a
/// division-by-zero error when the second operand is zero.
macro_rules! integer_division_binop_test_fn {
    ($($t:tt)+) => {
        integer_binop_test_fn!($($t)+ with_check_for_div_by 0);
    };
}

integer_binop_test_fn!(
    fn test_integer_add() {
        x0 + x1 == checked_add
    }
);
integer_binop_test_fn!(
    fn test_integer_subtract() {
        x0 - x1 == checked_sub
    }
);
integer_binop_test_fn!(
    fn test_integer_multiply() {
        x0 * x1 == checked_mul
    }
);
integer_division_binop_test_fn!(
    fn test_integer_divide() {
        x0 / x1 == checked_div_euclid
    }
);
integer_division_binop_test_fn!(
    fn test_integer_modulo() {
        x0 % x1 == checked_rem_euclid
    }
);
