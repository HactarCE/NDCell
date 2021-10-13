use super::*;

use RtVal::Integer;

#[test]
fn test_condition_values() {
    for prgm in [
        "            if     x0 { ret = 10 } else { ret = 20 }",
        "ret =  0    if     x0 { ret = 10 } else { ret = 20 }",
        "ret =  0    if     x0 { ret = 10 } else { ret = 20 }",
        "ret = 10    if     x0 {          } else { ret = 20 }",
        "ret = 20    if     x0 { ret = 10 } else {          }",
        "ret = 20    if     x0 { ret = 10 }",
        "ret = 10    unless x0 { ret = 20 }",
    ] {
        TestProgram::new()
            .with_input_types(&[Type::Integer])
            .with_exec(prgm)
            .with_result_expressions(&[(Type::Integer, "ret")])
            .assert_test_cases(
                test_values()
                    .iter()
                    .map(|&i| TestCase {
                        inputs: vec![Integer(i)],
                        expected_result: Ok(vec![if i != 0 { "10" } else { "20" }]),
                    })
                    .collect(),
            );
    }
}

#[test]
fn test_condition_else_if() {
    for prgm in [
        "            if x0 { ret = 10 } else if x1 { ret = 15 } else { ret = 20 }",
        "ret =  0    if x0 { ret = 10 } else if x1 { ret = 15 } else { ret = 20 }",
        "ret = 10    if x0 {          } else if x1 { ret = 15 } else { ret = 20 }",
        "ret = 15    if x0 { ret = 10 } else if x1 {          } else { ret = 20 }",
        "ret = 20    if x0 { ret = 10 } else if x1 { ret = 15 } else {          }",
        "ret = 20    if x0 { ret = 10 } else if x1 { ret = 15 }",
    ] {
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(prgm)
            .with_result_expressions(&[(Type::Integer, "ret")])
            .assert_test_cases(
                iproduct!(test_values(), test_values())
                    .map(|(&i, &j)| TestCase {
                        inputs: vec![Integer(i), Integer(j)],
                        expected_result: Ok(vec![if i != 0 {
                            "10"
                        } else if j != 0 {
                            "15"
                        } else {
                            "20"
                        }]),
                    })
                    .collect(),
            );
    }
}

#[test]
fn test_condition_uninit() {
    for prgm in [
        "if     x0 { ret = 10 }",
        "if     x0 { ret = 10 } else {          }",
        "if     x0 {          } else { ret = 20 }",
        "unless x0 { ret = 20 }",
        "if     x0 { ret = 10 } else if x1 { ret = 15 }",
        "if     x0 { ret = 10 } else if x1 { ret = 15 } else {          }",
        "if     x0 { ret = 10 } else if x1 {          } else { ret = 20 }",
        "if     x0 {          } else if x1 { ret = 15 } else { ret = 20 }",
    ] {
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(prgm)
            .with_result_expressions(&[(Type::Integer, "ret")])
            .assert_compile_errors(
                test_errors!["this variable might not have been assigned a value" @ "ret"],
            );
    }

    for prgm in [
        "if     x0 {          }",
        "if     x0 {          } else {          }",
        "unless x0 {          }",
        "if     x0 {          } else if x1 {          }",
        "if     x0 {          } else if x1 {          } else {          }",
    ] {
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(prgm)
            .with_result_expressions(&[(Type::Integer, "ret")])
            .assert_compile_errors(
                test_errors!["this variable doesn't exist or hasn't been assigned a value" @ "ret"],
            );
    }
}
