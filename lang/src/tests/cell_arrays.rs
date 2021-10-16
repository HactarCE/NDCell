use crate::data::{CellArray, VectorSet};

use super::*;
use RtVal::{Integer, Vector};

#[test]
fn test_cell_array_construction() {
    let span = crate::utils::dummy_span();
    let r2_moore = Arc::new(VectorSet::moore(span, 2, 1, span).unwrap());
    let r2_moore_zeros = CellArray::from_cell(Arc::clone(&r2_moore), 0_u8);
    let exprs = [(
        Type::CellArrayMut(Some(r2_moore.clone())),
        "moore().fill('1 2 3 4 . 0 # 7 9')",
    )];

    TestProgram::new()
        .with_setup("@states 7")
        .with_result_expressions(&exprs)
        .assert_compile_or_interpreted_errors(
            vec![],
            test_errors!["invalid cell symbol: \"7\"" @ "'1 2 3 4 . 0 # 7 9'"],
        );

    TestProgram::new()
        .with_setup("@states 10")
        .with_result_expressions(&exprs)
        .assert_interpreted_test_cases(test_cases![
            () => Ok("([-1, -1]..[1, 1]).fill(#1, #2, #3, #4, #0, #0, #1, #7, #9)"),
        ]);
}

#[test]
fn test_cell_array_mutation() {
    let span = crate::utils::dummy_span();
    let mut mask_iter = "###...###.........###....##".chars().map(|ch| ch == '#');
    let rectangle = VectorSet::moore(span, 3, 1, span).unwrap();
    let shape = Arc::new(
        rectangle
            .filter(span, |_| mask_iter.next().unwrap())
            .unwrap(),
    );
    let zeros = CellArray::from_cell(Arc::clone(&shape), 3_u8);
    let ty = Type::CellArrayMut(Some(Arc::clone(&shape)));

    assert_eq!(shape.len(), 11);

    let input_types = [ty.clone(), Type::Vector(Some(2)), Type::Integer];
    let test_prgm_template = TestProgram::new().with_input_types(&input_types);

    let gen_expected = |pos: &[LangInt]| {
        let mut expected = zeros.clone();
        *expected.get_cell_mut(&[-1, -1, -1]).unwrap() = 1_u8;
        expected
    };

    test_prgm_template
        .with_exec("x0[x1, x2] = #1")
        .assert_compiled_test_cases(
            iproduct!(-2..=2, -2..=2, -2..=2)
                .map(|(x, y, z)| {
                    let inputs = vec![
                        RtVal::CellArray(Arc::new(zeros.clone())),
                        Vector(vec![x, y]),
                        Integer(z),
                    ];

                    let mut expected = zeros.clone();
                    let expected_result = if let Some(c) = expected.get_cell_mut(&[x, y, z]) {
                        *c = 1_u8;
                        Ok(vec![expected])
                    } else if rectangle.contains_vector(&[x, y, z]) {
                        test_err!("position excluded by array mask" @ "[x1, x2]")
                    } else {
                        test_err!("position out of bounds" @ "[x1, x2]")
                    };

                    TestCase {
                        inputs,
                        expected_result,
                    }
                })
                .collect_vec(),
        );

    TestProgram::new()
        .with_input_types(&[Type::CellArrayMut(Some(shape))])
        .with_exec("a = x0.as_immut    a[0] = #1")
        .assert_compile_or_interpreted_errors(
            vec![RtVal::CellArray(Arc::new(zeros))],
            test_errors![
                "cannot assign to this expression" @ "a[0]",
            ],
        );
}
