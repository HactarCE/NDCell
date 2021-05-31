use super::*;

#[test]
fn test_const_eval_div_by_zero() {
    TestProgram::new()
        .with_result_expressions(&[(Type::Integer, "1 / 0")])
        .assert_compile_errors(vec![(&"/", "division by zero")]);
}
