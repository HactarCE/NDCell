use super::*;

#[test]
fn test_integer_plus() {
    use RtVal::Integer;

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
