use super::*;
use RtVal::{Cell, Integer};

#[test]
fn test_convert_to_bool() {
    test_convert_type_to_bool(Type::Integer, test_values().iter().map(|&i| Integer(i)));
    test_convert_type_to_bool(Type::Cell, test_values().iter().map(|&i| Cell(i)));
}
fn test_convert_type_to_bool(ty: Type, test_values: impl Iterator<Item = RtVal>) {
    TestProgram::new()
        .with_setup("@states 256")
        .with_input_types(&[ty])
        .with_result_expressions(&[(Type::Integer, "bool(x0)")])
        .assert_test_cases(test_values.map_collect_vec(|v| TestCase {
            expected_result: test_ok![v.to_bool().unwrap() as LangInt],
            inputs: vec![v],
        }));
}

#[test]
fn test_logical_or() {
    // Test with variable values.
    TestProgram::new()
        .with_input_types(&[Type::Integer, Type::Integer])
        .with_result_expressions(&[(Type::Integer, "x0 or x1")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0), Integer(0)], false),
            (&[Integer(2), Integer(1)], true),
            (&[Integer(0), Integer(6)], true),
            (&[Integer(-5), Integer(0)], true),
        ]));

    // Test short-circuiting with constant `false`.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "0 or x0")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0)], false),
            (&[Integer(-99)], true),
        ]));

    // Test short-circuiting with constant `true`.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "42 or semantic['nonsense']")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0)], true),
            (&[Integer(-99)], true),
        ]));
}

#[test]
fn test_logical_and() {
    // Test with variable values.
    TestProgram::new()
        .with_input_types(&[Type::Integer, Type::Integer])
        .with_result_expressions(&[(Type::Integer, "x0 and x1")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0), Integer(0)], false),
            (&[Integer(2), Integer(1)], true),
            (&[Integer(0), Integer(6)], false),
            (&[Integer(-5), Integer(0)], false),
        ]));

    // Test short-circuiting with constant `false`.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "0 and semantic['nonsense']")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0)], false),
            (&[Integer(-99)], false),
        ]));

    // Test short-circuiting with constant `true`.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "42 and x0")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0)], false),
            (&[Integer(-99)], true),
        ]));
}

#[test]
fn test_logical_xor() {
    TestProgram::new()
        .with_input_types(&[Type::Integer, Type::Integer])
        .with_result_expressions(&[(Type::Integer, "x0 xor x1")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0), Integer(0)], false),
            (&[Integer(2), Integer(1)], false),
            (&[Integer(0), Integer(6)], true),
            (&[Integer(-5), Integer(0)], true),
        ]));
}

#[test]
fn test_logical_not() {
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "not x0")])
        .assert_test_cases(boolean_test_cases(&[
            (&[Integer(0)], true),
            (&[Integer(2)], false),
            (&[Integer(1)], false),
            (&[Integer(6)], false),
            (&[Integer(-5)], false),
        ]));
}

fn boolean_test_cases<'a>(cases: &'a [(&'a [RtVal], bool)]) -> Vec<TestCase<'static, LangInt>> {
    cases.map_collect_vec(|(inputs, output)| TestCase {
        inputs: inputs.to_vec(),
        expected_result: test_ok![*output as LangInt],
    })
}
