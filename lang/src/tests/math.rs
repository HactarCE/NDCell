use super::*;

#[test]
fn test_integer_plus() {
    use Value::Integer;

    test_expr(
        "x0 + x1",
        &iproduct!(test_values::<LangInt>(), test_values::<LangInt>())
            .map(|(&a, &b)| {
                let expected = match a.checked_add(b) {
                    Some(sum) => Ok(Integer(sum)),
                    None => Err(("+", "Integer overflow")),
                };

                (vec![Integer(a), Integer(b)], expected)
            })
            .collect_vec(),
    );
}
