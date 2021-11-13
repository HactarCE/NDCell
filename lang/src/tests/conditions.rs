use super::*;

use RtVal::Integer;

fn test_error_when_using_ret_after_condition<'a>(
    prgms: impl IntoIterator<Item = &'a str>,
    error_msg: &str,
) {
    for prgm in prgms {
        // Don't use `ret`.
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(prgm)
            .assert_test_cases::<&str>(
                iproduct!(test_values(), test_values())
                    .map(|(&i, &j)| TestCase {
                        inputs: vec![Integer(i), Integer(j)],
                        expected_result: Ok(vec![]),
                    })
                    .collect(),
            );

        // Do use `ret`.
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(prgm)
            .with_result_expressions(&[(Type::Integer, "ret")])
            .assert_compile_errors(test_errors![error_msg @ "ret"]);
    }
}

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
    test_error_when_using_ret_after_condition(
        [
            "if     x0 { ret = 10 }",
            "if     x0 { ret = 10 } else {          }",
            "if     x0 {          } else { ret = 20 }",
            "unless x0 { ret = 20 }",
            "if     x0 { ret = 10 } else if x1 { ret = 15 }",
            "if     x0 { ret = 10 } else if x1 { ret = 15 } else {          }",
            "if     x0 { ret = 10 } else if x1 {          } else { ret = 20 }",
            "if     x0 {          } else if x1 { ret = 15 } else { ret = 20 }",
        ],
        "this variable might not have been assigned a value",
    );

    test_error_when_using_ret_after_condition(
        [
            "if     x0 {          }",
            "if     x0 {          } else {          }",
            "unless x0 {          }",
            "if     x0 {          } else if x1 {          }",
            "if     x0 {          } else if x1 {          } else {          }",
        ],
        "this variable doesn't exist or hasn't been assigned a value",
    );
}

#[test]
fn test_condition_type_mismatch() {
    test_error_when_using_ret_after_condition(
        [
            "            if x0 { ret = #0 } else if x1 { ret =  0 } else { ret =  0 }",
            "            if x0 { ret =  0 } else if x1 { ret = #0 } else { ret =  0 }",
            "            if x0 { ret =  0 } else if x1 { ret =  0 } else { ret = #0 }",
            "            if x0 { ret = #0 } else { ret =  0 }",
            "            if x0 { ret =  0 } else { ret = #0 }",
            "ret =  0    if x0 { ret = #0 } else if x1 {          } else {          }",
            "ret =  0    if x0 {          } else if x1 { ret = #0 } else {          }",
            "ret =  0    if x0 {          } else if x1 {          } else { ret = #0 }",
            "ret =  0    if x0 { ret = #0 } else if x1 { ret = #0 } else {          }",
            "ret =  0    if x0 {          } else if x1 { ret = #0 } else { ret = #0 }",
            "ret =  0    if x0 { ret = #0 } else if x1 {          } else { ret = #0 }",
            "ret =  0    if x0 { ret = #0 } else if x1 {          }",
            "ret =  0    if x0 {          } else if x1 { ret = #0 }",
            "ret =  0    if x0 { ret = #0 } else if x1 { ret = #0 }",
            "ret =  0    if x0 { ret = #0 } else {          }",
            "ret =  0    if x0 {          } else { ret = #0 }",
        ],
        "this variable's type cannot be determined",
    );
}

#[test]
fn test_compile_time_condition() {
    for prgm in [
        "if     1 { ret = 10 }",
        "if     1 { ret = 10 } else {          }",
        "if     0 {          } else { ret = 10 }",
        "unless 0 { ret = 10 }",
        "if     0 {          } else if 1 { ret = 10 }",
    ] {
        TestProgram::new()
            .with_exec(prgm)
            .with_result_expressions(&[(Type::Integer, "ret")])
            .assert_test_cases(test_cases![() => Ok("10")]);
    }
}
