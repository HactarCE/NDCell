use super::*;

use RtVal::Integer;

#[test]
fn test_integer_add() {
    test_expr(
        "x0 + x1",
        &[Type::Integer, Type::Integer, Type::Integer],
        &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
            .map(|(&a, &b)| {
                let expected = match a.checked_add(b) {
                    Some(sum) => Ok(Integer(sum)),
                    None => Err(&[("+", "integer overflow")][..]),
                };

                (vec![Integer(a), Integer(b)], expected)
            })
            .collect_vec(),
    );
}

#[test]
fn test_integer_subtract() {
    test_expr(
        "x0 - x1",
        &[Type::Integer, Type::Integer, Type::Integer],
        &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
            .map(|(&a, &b)| {
                let expected = match a.checked_sub(b) {
                    Some(sum) => Ok(Integer(sum)),
                    None => Err(&[("-", "integer overflow")][..]),
                };

                (vec![Integer(a), Integer(b)], expected)
            })
            .collect_vec(),
    );
}

#[test]

fn test_integer_multiply() {
    test_expr(
        "x0 * x1",
        &[Type::Integer, Type::Integer, Type::Integer],
        &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
            .map(|(&a, &b)| {
                let expected = match a.checked_mul(b) {
                    Some(sum) => Ok(Integer(sum)),
                    None => Err(&[("*", "integer overflow")][..]),
                };

                (vec![Integer(a), Integer(b)], expected)
            })
            .collect_vec(),
    );
}

#[test]
fn test_integer_divide() {
    test_expr(
        "x0 / x1",
        &[Type::Integer, Type::Integer, Type::Integer],
        &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
            .map(|(&a, &b)| {
                let expected = match a.checked_div_euclid(b) {
                    Some(sum) => Ok(Integer(sum)),
                    None if b == 0 => Err(&[("/", "division by zero")][..]),
                    None => Err(&[("/", "integer overflow")][..]),
                };

                (vec![Integer(a), Integer(b)], expected)
            })
            .collect_vec(),
    );
}

#[test]
fn test_integer_remainder() {
    test_expr(
        "x0 % x1",
        &[Type::Integer, Type::Integer, Type::Integer],
        &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
            .map(|(&a, &b)| {
                let expected = match a.checked_rem_euclid(b) {
                    Some(sum) => Ok(Integer(sum)),
                    None if b == 0 => Err(&[("%", "division by zero")][..]),
                    None => Err(&[("%", "integer overflow")][..]),
                };

                (vec![Integer(a), Integer(b)], expected)
            })
            .collect_vec(),
    );
}
