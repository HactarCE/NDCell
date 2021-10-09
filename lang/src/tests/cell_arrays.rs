use crate::data::{CellArray, VectorSet};

use super::*;
use RtVal::{Cell, Integer};

#[test]
fn test_cell_array_construction() {
    let span = crate::utils::dummy_span();
    let r2_moore = Arc::new(VectorSet::moore(span, 2, 1, span).unwrap());
    let r2_moore_zeros = CellArray::from_cell(Arc::clone(&r2_moore), 0_u8);
    let exprs = [(
        Type::CellArray(Some(r2_moore)),
        "moore().fill('1 2 3 4 . 0 # 7 9')",
    )];

    TestProgram::new()
        .with_setup("@states 7")
        .with_result_expressions(&exprs)
        .assert_interpreted_test_cases::<&str>(test_cases![
            () => Err("invalid cell symbol: \"7\"" @ "'1 2 3 4 . 0 # 7 9'"),
        ]);
    TestProgram::new()
        .with_setup("@states 7")
        .with_result_expressions(&exprs)
        .assert_compile_errors(test_errors![
            "invalid cell symbol: \"7\"" @ "'1 2 3 4 . 0 # 7 9'",
        ]);

    TestProgram::new()
        .with_setup("@states 10")
        .with_result_expressions(&exprs)
        .assert_interpreted_test_cases(test_cases![
            () => Ok("([-1, -1]..[1, 1]).fill(#1, #2, #3, #4, #0, #0, #1, #7, #9)"),
        ]);

    // TestProgram::new()
    //     .with_setup("@states 10")
    //     .with_result_expressions(&exprs)
    //     .assert_compiled_test_cases(test_cases![
    //         () => Ok(RtVal::CellArray(r2_moore_zeros)),
    //     ]);

    // TODO syntax for defining a cell array literal?
    // TODO syntax for defining cell array with a particular shape as parameter?
    // TODO syntax for indexing a cell array?
    // TODO assign to index of a cell array?
}

#[test]
fn test_cell_array_mutation() {
    let span = crate::utils::dummy_span();
    let mut mask_iter = "###...###.........###....##".chars().map(|ch| ch == '#');
    let shape = Arc::new(
        VectorSet::moore(span, 3, 1, span)
            .unwrap()
            .filter(span, |_| mask_iter.next().unwrap())
            .unwrap(),
    );
    let zeros = CellArray::from_cell(Arc::clone(&shape), 0_u8);
    let ty = Type::CellArray(Some(Arc::clone(&shape)));

    assert_eq!(shape.len(), 11);

    let input_types = [ty.clone()];
    let test_prgm_template = TestProgram::new().with_input_types(&input_types);

    let mut expected = zeros.clone();
    *expected.get_cell_mut(&[-1, -1, -1]).unwrap() = 1_u8;
    test_prgm_template
        .with_exec("x0[-1] = #1")
        .assert_test_cases(test_cases![
            (RtVal::CellArray(zeros)) => Ok(expected.to_string()),
        ]);

    // TestProgram::new().with_exec(
    //     "
    //         p = __compiled_arg__[0]

    //     ",
    // );
}
