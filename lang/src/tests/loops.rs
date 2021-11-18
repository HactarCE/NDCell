use super::*;

use RtVal::Integer;

#[test]
fn test_while_loop() {
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_exec("while x0 { x0 -= 1 }")
        .with_result_expressions(&[(Type::Integer, "x0")])
        .assert_test_cases(
            (0..=20)
                .map(|i| TestCase {
                    inputs: vec![Integer(i)],
                    expected_result: test_ok![0],
                })
                .collect(),
        );
}

#[test]
fn test_while_continue() {
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_exec(
            "
            x1 = 0
            while x0 {
                x0 -= 1
                if x0 % 3 { continue }
                x1 += 1
            }
            ",
        )
        .with_result_expressions(&[(Type::Integer, "x0"), (Type::Integer, "x1")])
        .assert_test_cases(
            (0..=20)
                .map(|i| TestCase {
                    inputs: vec![Integer(i)],
                    expected_result: test_ok![0, (i + 2) / 3],
                })
                .collect(),
        );
}
