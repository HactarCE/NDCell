use super::*;
use crate::data::RtVal::Integer;

#[test]
fn test_vector_set_construct() {
    TestProgram::new()
        .with_result_expressions(&[(Type::IntegerSet, "x0..x1")])
        .assert_interpreted_test_cases(test_cases![(Integer(-3), Integer(99)) => Ok("-3..99")]);

    TestProgram::new()
        .with_result_expressions(&[(Type::IntegerSet, "{1, 9, 6, -12}")])
        .assert_interpreted_test_cases(test_cases![() => Ok("{-12, 1, 6, 9}")]);

    TestProgram::new()
        .with_result_expressions(&[(Type::IntegerSet, "intset()")])
        .assert_interpreted_test_cases(test_cases![() => Ok("intset()")]);
}
