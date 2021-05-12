use super::*;

#[test]
fn test_eager_eval_div_by_zero() {
    assert_expr_compile_error(
        "1 / 0",
        &[Type::Integer, Type::Integer],
        &[(&"/", "division by zero")],
    );
}
