use super::*;

use crate::data::VectorSet;
use RtVal::Vector;

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
        x = -9999
        count = 0
        sum = 0
        product = 1
        for x in {0} {{
            count += 1
            sum += x
            if x {{ product *= x }}
            else {{ x = -9999 }} // this should have no effect
        }}
        ",
        iter_expr,
    );
    TestProgram::new()
        .with_exec(&exec)
        .with_result_expressions(&[
            (Type::Integer, "x"),
            (Type::Integer, "count"),
            (Type::Integer, "sum"),
            (Type::Integer, "product"),
        ])
        .assert_compiled_and_interpreted_agree(vec![vec![]]);
}

#[test]
fn test_for_loop_vector_set() {
    let exec = "
        v = vec()
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
            (Type::Vector(Some(2)), "v"),
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

        for j, a in [10, 20, 30, 40] {
            assert j == a / 10 - 1
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

        for j, a in x0 {
            assert j == a / 10 - 1
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

#[test]
fn test_for_loop_array() {
    let span = crate::utils::dummy_span();
    let shape = Arc::new(VectorSet::moore(span, 2, 1, span).unwrap());
    assert_eq!(shape.len(), 9);
    let ty = Type::CellArrayMut(Some(Arc::clone(&shape)));
    let array = CellArray::from_cells(span, shape, &(0..9).map(|i| i * 3).collect_vec()).unwrap();
    let input_types = [ty];

    let exec_template = "
        i = 0
        for cell in a {
            assert cell == #(i * 3)
            i += 1
        }
        assert i == 9

        i = 0
        for pos in a.shape {
            assert a[pos] == #(i * 3)
            i += 1
        }
        assert i == 9

        i = 0
        for pos, cell in a {
            assert cell == a[pos] == #(i * 3)
            i += 1
        }
        assert i == 9
        ";

    let test_prgm_template = TestProgram::new().with_setup("@states 200");

    let exec = format!("a = {}\n{}", array, exec_template);
    test_prgm_template
        .with_exec(&exec)
        .assert_test_cases::<&str>(test_cases![() => Ok()]);

    let exec = format!("a = ({}).as_immut\n{}", array, exec_template);
    test_prgm_template
        .with_exec(&exec)
        .assert_test_cases::<&str>(test_cases![() => Ok()]);

    let exec = format!("a = x0.as_immut\n{}", exec_template);
    test_prgm_template
        .with_input_types(&input_types)
        .with_exec(&exec)
        .assert_test_cases(test_cases![(RtVal::CellArray(Arc::new(array.clone()))) => Ok(&array)]);

    let exec = format!("a = x0\n{}", exec_template);
    test_prgm_template
        .with_input_types(&input_types)
        .with_exec(&exec)
        .assert_test_cases(test_cases![(RtVal::CellArray(Arc::new(array.clone()))) => Ok(&array)]);
}

#[test]
fn test_for_loop_array_with_mask() {
    let span = crate::utils::dummy_span();
    let mut mask_iter = "###...###.........###....##".chars().map(|ch| ch == '#');
    let rectangle = VectorSet::moore(span, 3, 1, span).unwrap();
    let shape = Arc::new(
        rectangle
            .filter(span, |_| mask_iter.next().unwrap())
            .unwrap(),
    );
    assert_eq!(shape.len(), 11);
    let ty = Type::CellArrayMut(Some(Arc::clone(&shape)));
    let array = CellArray::from_cells(span, shape, &(0..11).map(|i| i * 3).collect_vec()).unwrap();
    let input_types = [ty];

    let exec_template = "
        i = 0
        for cell in a {
            assert cell == #(i * 3)
            i += 1
        }
        assert i == 11

        i = 0
        for pos in a.shape {
            assert a[pos] == #(i * 3)
            i += 1
        }
        assert i == 11

        i = 0
        for pos, cell in a {
            assert cell == a[pos] == #(i * 3)
            i += 1
        }
        assert i == 11
        ";

    let test_prgm_template = TestProgram::new().with_setup("@states 200");

    let exec = format!("a = {}\n{}", array, exec_template);
    test_prgm_template
        .with_exec(&exec)
        .assert_test_cases::<&str>(test_cases![() => Ok()]);

    let exec = format!("a = ({}).as_immut\n{}", array, exec_template);
    test_prgm_template
        .with_exec(&exec)
        .assert_test_cases::<&str>(test_cases![() => Ok()]);

    let exec = format!("a = x0\n{}", exec_template);
    test_prgm_template
        .with_input_types(&input_types)
        .with_exec(&exec)
        .assert_test_cases(test_cases![(RtVal::CellArray(Arc::new(array.clone()))) => Ok(&array)]);

    let exec = format!("a = x0.as_immut\n{}", exec_template);
    test_prgm_template
        .with_input_types(&input_types)
        .with_exec(&exec)
        .assert_test_cases(test_cases![(RtVal::CellArray(Arc::new(array.clone()))) => Ok(&array)]);
}
