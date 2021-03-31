use super::*;

#[test]
fn test_integer_plus() {
    use Value::Integer as Int;

    test_expr(
        "x0 + x1",
        &[
            (&[Int(2), Int(3)], Ok(Int(5))),
            (&[Int(-1), Int(-5)], Ok(Int(-6))),
        ],
    );
}
