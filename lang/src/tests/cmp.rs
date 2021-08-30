use super::*;
use RtVal::{Cell, Integer};

#[test]
fn test_comparisons() {
    test_comparison(Type::Integer, Integer, "x0 == x1", Ok(|x0, x1| x0 == x1));
    test_comparison(Type::Integer, Integer, "x0 != x1", Ok(|x0, x1| x0 != x1));
    test_comparison(Type::Integer, Integer, "x0 < x1", Ok(|x0, x1| x0 < x1));
    test_comparison(Type::Integer, Integer, "x0 > x1", Ok(|x0, x1| x0 > x1));
    test_comparison(Type::Integer, Integer, "x0 <= x1", Ok(|x0, x1| x0 <= x1));
    test_comparison(Type::Integer, Integer, "x0 >= x1", Ok(|x0, x1| x0 >= x1));

    test_comparison(Type::Cell, Cell, "x0 == x1", Ok(|x0, x1| x0 == x1));
    test_comparison(Type::Cell, Cell, "x0 != x1", Ok(|x0, x1| x0 != x1));
    test_comparison(Type::Cell, Cell, "x0 < x1", Err("type error: cannot compare Cell to Cell using '<'; convert them to integers first using the '#id' tag"));
    test_comparison(Type::Cell, Cell, "x0 > x1", Err("type error: cannot compare Cell to Cell using '>'; convert them to integers first using the '#id' tag"));
    test_comparison(Type::Cell, Cell, "x0 <= x1", Err("type error: cannot compare Cell to Cell using '<='; convert them to integers first using the '#id' tag"));
    test_comparison(Type::Cell, Cell, "x0 >= x1", Err("type error: cannot compare Cell to Cell using '>='; convert them to integers first using the '#id' tag"));
}

#[test]
fn test_multi_comparisons() {
    test_multi_comparison("x0 == x1 == x2", |x0, x1, x2| x0 == x1 && x1 == x2);
    test_multi_comparison("x0 == x1 != x2", |x0, x1, x2| x0 == x1 && x1 != x2);
    test_multi_comparison("x0 != x1 != x2", |x0, x1, x2| x0 != x1 && x1 != x2);
    test_multi_comparison("x0 >= x1 > x2", |x0, x1, x2| x0 >= x1 && x1 > x2);
    test_multi_comparison("x0 >= x1 < x2", |x0, x1, x2| x0 >= x1 && x1 < x2);
    test_multi_comparison("x0 < x1 == x2", |x0, x1, x2| x0 < x1 && x1 == x2);
    test_multi_comparison("x0 < x1 != x2", |x0, x1, x2| x0 < x1 && x1 != x2);
}

fn test_comparison<T: 'static + Copy + TestValues>(
    ty: Type,
    value_constructor: fn(T) -> RtVal,
    expr_str: &str,
    cmp_reference_impl: Result<fn(T, T) -> bool, &str>,
) {
    let input_types = [ty.clone(), ty];
    let result_expr = [(Type::Integer, expr_str)];
    let prgm = TestProgram::new()
        .with_input_types(&input_types)
        .with_result_expressions(&result_expr);

    let test_cases = iproduct!(test_values(), test_values()).map_collect_vec(|(&a, &b)| TestCase {
        inputs: vec![value_constructor(a), value_constructor(b)],
        expected_result: match cmp_reference_impl {
            Ok(f) => test_ok!(f(a, b) as LangInt),
            Err(msg) => test_err!(msg @ expr_str),
        },
    });

    prgm.clone()
        .assert_interpreted_test_cases(test_cases.clone());
    match cmp_reference_impl {
        Ok(_) => prgm.assert_compiled_test_cases(test_cases.clone()),
        Err(msg) => prgm.assert_compile_errors(test_errors!(msg @ expr_str)),
    }
}

fn test_multi_comparison(
    expr_str: &str,
    cmp_reference_impl: fn(LangInt, LangInt, LangInt) -> bool,
) {
    TestProgram::new()
        .with_input_types(&[Type::Integer, Type::Integer, Type::Integer])
        .with_result_expressions(&[(Type::Integer, expr_str)])
        .assert_test_cases(
            iproduct!(test_values(), test_values(), test_values()).map_collect_vec(
                |(&a, &b, &c)| TestCase {
                    inputs: vec![Integer(a), Integer(b), Integer(c)],
                    expected_result: Ok(vec![cmp_reference_impl(a, b, c) as LangInt]),
                },
            ),
        )
}
