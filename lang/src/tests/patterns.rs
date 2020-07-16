use super::{assert_fn_result, compile_transition_fn, ConstValue, Pattern, PatternShape};

#[test]
fn test_empty_transition_fn() {
    let mut f = compile_transition_fn("@states 10 @transition {}");
    assert_fn_result(
        &mut f,
        &mut [ConstValue::Pattern(Pattern {
            cells: vec![0, 0, 0, 0, 5, 0, 0, 0, 0],
            shape: PatternShape::moore(2, 1),
            lut: None,
        })],
        Ok(ConstValue::CellState(5)),
    );
}
