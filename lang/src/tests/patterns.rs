use super::{assert_fn_result, compile_transition_fn, ConstValue, Pattern, PatternShape};

#[test]
fn test_empty_transition_fn() {
    let mut f = compile_transition_fn("@states 10 @transition {}");
    assert_fn_result(
        &mut f,
        &mut [demo_pattern_3x3()],
        Ok(ConstValue::CellState(5)),
    );
}

#[test]
fn test_square_pattern_indexing() {
    let mut expected = 1;
    for (&y, &x) in iproduct!(&[-1, 0, 1], &[-1, 0, 1]) {
        let mut f = compile_transition_fn(&format!(
            "@states 10
            @transition {{
                become nbhd[{}, {}]
            }}",
            x, y,
        ));
        assert_fn_result(
            &mut f,
            &mut [demo_pattern_3x3()],
            Ok(ConstValue::CellState(expected)),
        );
        expected += 1;
    }
    for (&y, &x) in iproduct!(&[-2, 0, 2], &[-2, 0, 2]) {
        if x == 0 && y == 0 {
            continue;
        }
        let mut f = compile_transition_fn(&format!(
            "@states 10
            @transition {{
                become nbhd[{}, {}]
            }}",
            x, y,
        ));

        assert_fn_result(
            &mut f,
            &mut [demo_pattern_3x3()],
            Err((&format!("nbhd[{}, {}]", x, y), "Index out of bounds")),
        )
    }
}

#[test]
fn test_pattern_iter() {
    let mut f = compile_transition_fn(
        "@states 10
        @transition {
            i = 1
            for cell in nbhd {
                assert cell != #0
                if cell != #(i) {
                    become cell
                }
                i += 1
            }
            assert i == 10
            become #0
        }",
    );
    assert_fn_result(
        &mut f,
        &mut [demo_pattern_3x3()],
        Ok(ConstValue::CellState(0)),
    );
}

fn demo_pattern_3x3() -> ConstValue {
    ConstValue::Pattern(Pattern {
        cells: (1..10).collect(),
        shape: PatternShape::moore(2, 1),
        lut: None,
    })
}
