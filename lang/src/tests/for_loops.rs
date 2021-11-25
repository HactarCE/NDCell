use super::*;

use RtVal::{Integer, Vector};

#[test]
fn test_for_loop_empty_set() {
    test_integer_for_loop("{}");
}

#[test]
fn test_for_loop_range() {
    test_integer_for_loop("-5..12");
    test_integer_for_loop("3..3");
    test_integer_for_loop("{-1, 9999, 4}");
    test_integer_for_loop("intset()");
}

fn test_integer_for_loop(iter_expr: &str) {
    let exec = format!(
        "
        i = -9999
        count = 0
        sum = 0
        product = 1
        for i in {} {{
            count += 1
            sum += i
            if i {{ product *= i }}
            else {{ i = -9999 }} // this should have no effect
        }}
        ",
        iter_expr,
    );
    TestProgram::new()
        .with_exec(&exec)
        .with_result_expressions(&[
            (Type::Integer, "i"),
            (Type::Integer, "count"),
            (Type::Integer, "sum"),
            (Type::Integer, "product"),
        ])
        .assert_compiled_and_interpreted_agree(vec![vec![]]);
}

#[test]
fn test_for_loop_vector_set() {
    let exec = "
        i = -9999
        count = vec(0)
        sum = vec(0)
        product = vec(1)
        for v in circular(r=3) {
            count += 1
            sum += v
            if v.x and v.y { product *= v }
        }
        ";
    TestProgram::new()
        .with_exec(&exec)
        .with_result_expressions(&[
            (Type::Integer, "i"),
            (Type::Vector(Some(2)), "count"),
            (Type::Vector(Some(2)), "sum"),
            (Type::Vector(Some(2)), "product"),
        ])
        .assert_compiled_and_interpreted_agree(vec![vec![]]);
}

#[test]
fn test_for_loop_vector() {
    // Test with constant vector.
    let exec = "
        i = 4
        ret = vec4()
        a = 0
        for a in [10, 20, 30, 40] {
            i -= 1
            ret[i] = a
        }
        ";
    TestProgram::new()
        .with_exec(exec)
        .with_result_expressions(&[
            (Type::Vector(Some(4)), "ret"),
            (Type::Integer, "a"),
            (Type::Integer, "i"),
        ])
        .assert_test_cases(test_cases![
            () => Ok("[40, 30, 20, 10]", "40", "0")
        ]);

    // Test with non-constant vector.
    let exec = "
        i = 4
        ret = vec4()
        a = 0
        for a in x0 {
            i -= 1
            ret[i] = a
        }
        ";
    TestProgram::new()
        .with_input_types(&[Type::Vector(Some(4))])
        .with_exec(exec)
        .with_result_expressions(&[
            (Type::Vector(Some(4)), "ret"),
            (Type::Integer, "a"),
            (Type::Integer, "i"),
        ])
        .assert_test_cases(test_cases![
            (Vector(vec![10, 20, 30, 40])) => Ok("[40, 30, 20, 10]", "40", "0")
        ]);
}
