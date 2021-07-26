use super::*;

#[test]
fn test_empty_set() {
    // Test empty set constructor.
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "{}")])
        .assert_interpreted_test_cases::<&str>(test_cases![
            () => Ok("{}"),
        ]);

    // Test empty set operations.
    TestProgram::new()
        .with_result_expressions(&[
            (Type::VectorSet(None), "{} | {}"),
            (Type::VectorSet(None), "{} & {}"),
            (Type::VectorSet(None), "{} - {}"),
            (Type::VectorSet(None), "{} ^ {}"),
        ])
        .assert_interpreted_test_cases::<&str>(test_cases![
            () => Ok("{}", "{}", "{}", "{}"),
        ]);

    // TODO: superset/subset operators (like in Python)
}
