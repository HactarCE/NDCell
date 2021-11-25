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

#[test]
fn test_while_maybe_uninit() {
    for (pre, inner, post) in [
        ("", "x = 1", "_ = x"),
        ("if x0 { x = 1 }", "x = 1", "_ = x"),
    ] {
        let prgm = format!("{}  while x1 {{  x1-=1 {}  }}  {}", pre, inner, post);
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(&prgm)
            .assert_compile_errors(
                test_errors!["this variable might not have been assigned a value" @ "x"],
            );
    }
}

#[test]
fn test_while_ambiguous_type() {
    for (pre, inner, post) in [
        ("if x0 { x=1 } else { x=#1 }", "", ""),
        ("x=1", "x=#1", ""),
        ("x=1", "x=#1 _=x", ""),
        ("x=1", "if x0 { x=#1 }", ""),
        ("x=1", "x=#1 x=1", "_=x"),
        ("x=1", "if x0 { x=#1 _=x x=1 _=x }", "_=x"),
        ("x=1", "if x0 { x=#1 _=x }", ""),
        ("x=1", "if x0 { x=#1 continue x=1 }", ""),
        ("x=1", "if x0 { x=#1 break x=1 }", ""),
        ("x=1", "_=x x=1 continue x=#1", "_=x"),
        ("x=1", "_=x x=1 break x=#1", "_=x"),
        ("x=1", "_=x if x0 { x=1 continue x=#1 }", "_=x"),
        ("x=1", "_=x if x0 { x=1 break x=#1 }", "_=x"),
        ("x=1", "_=x==1 x=#1 _=x==#1 x=1", "_=x"),
        ("x=1", "x=#1 _=x==#1", ""),
        ("x=1", "if x0 { x=#1 break x=1 } _=x", ""),
    ] {
        let prgm = format!("{}  while x1 {{  x1-=1 {}  }}  {}", pre, inner, post);
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(&prgm)
            .assert_compile_ok();
    }

    for (pre, inner, post) in [
        ("if x0 { x=1 } else { x=#1 }", "", "_=x"),
        ("if x0 { x=1 } else { x=#1 }", "_=x", ""),
        ("x=1", "x=#1", "_=x"),
        ("x=1", "_=x x=#1", ""),
        ("x=1", "if x0 { x=#1 }", "_=x"),
        ("x=1", "if x0 { x=#1 } _=x", ""),
        ("x=1", "_=x if x0 { x=#1 }", ""),
        ("x=1", "if x0 { x=#1 } _=x", ""),
        ("x=1", "_=x if x0 { x=#1 continue x=1 }", ""),
        ("x=1", "_=x if x0 { x=#1 break x=1 }", "_=x"),
        ("x=1", "if x0 { x=#1 break x=1 }", "_=x"),
        ("x=1", "if x0 { x=#1 continue x=1 } _=x", ""),
    ] {
        let prgm = format!("{}  while x1 {{  x1-=1 {}  }}  {}", pre, inner, post);
        TestProgram::new()
            .with_input_types(&[Type::Integer, Type::Integer])
            .with_exec(&prgm)
            .assert_compile_errors(test_errors!["this variable's type cannot be determined" @ "x"]);
    }
}

#[test]
fn test_while_const_condition() {
    TestProgram::new()
        .with_exec("while 0 { this = is_an_error }")
        .assert_compile_ok();
    TestProgram::new()
        .with_exec("x=1 while 0 { x=#1 } _=x")
        .assert_compile_ok();
    TestProgram::new()
        .with_exec("while 1 { x=1 } _=x")
        .assert_compile_ok();
    TestProgram::new()
        .with_exec("x=1 while 1 { x=#1 } _=x")
        .assert_compile_ok();
}
