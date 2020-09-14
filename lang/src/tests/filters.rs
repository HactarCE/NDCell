use super::{
    assert_fn_result, compile_test_fn, test_values, CellStateFilter, ConstValue,
    STATE_COUNT_TEST_VALUES,
};

#[test]
fn test_cell_state_filter_ops() {
    for &state_count in STATE_COUNT_TEST_VALUES {
        let mut f = compile_test_fn(&format!(
            "@states {}
            @function CellFilter test(CellFilter l, CellFilter r, Int op) {{
                if op == 0 {{ return l & r }}
                if op == 1 {{ return l | r }}
                if op == 2 {{ return l ^ r }}
                if op == 3 {{ return ~l }}
            }}",
            state_count,
        ));
        for (&cell_state1, &cell_state2) in iproduct!(test_values(), test_values()) {
            if cell_state1 as usize >= state_count || cell_state2 as usize >= state_count {
                continue;
            }
            let f1 = CellStateFilter::single_cell_state(state_count, cell_state1);
            let f2 = CellStateFilter::single_cell_state(state_count, cell_state2);
            let arg1 = ConstValue::CellStateFilter(f1.clone());
            let arg2 = ConstValue::CellStateFilter(f2.clone());
            assert_fn_result(
                &mut f,
                &mut [arg1.clone(), arg2.clone(), ConstValue::Int(0)],
                Ok(ConstValue::CellStateFilter(f1.clone() & f2.clone())),
            );
            assert_fn_result(
                &mut f,
                &mut [arg1.clone(), arg2.clone(), ConstValue::Int(1)],
                Ok(ConstValue::CellStateFilter(f1.clone() | f2.clone())),
            );
            assert_fn_result(
                &mut f,
                &mut [arg1.clone(), arg2.clone(), ConstValue::Int(2)],
                Ok(ConstValue::CellStateFilter(f1.clone() ^ f2.clone())),
            );
            assert_fn_result(
                &mut f,
                &mut [arg1.clone(), arg2.clone(), ConstValue::Int(3)],
                Ok(ConstValue::CellStateFilter(!f1.clone())),
            );
        }
    }
}

#[test]
fn test_cell_state_filter_construct_from_cell_state() {
    // Construct from single cell states
    for &state_count in STATE_COUNT_TEST_VALUES {
        let mut f = compile_test_fn(&format!(
            "@states {}
            @function CellFilter test(Cell state) {{
                return ~state
            }}",
            state_count,
        ));
        for &cell_state in test_values() {
            if cell_state as usize >= state_count {
                continue;
            }
            assert_fn_result(
                &mut f,
                &mut [ConstValue::CellState(cell_state)],
                Ok(ConstValue::CellStateFilter(
                    !CellStateFilter::single_cell_state(state_count, cell_state),
                )),
            );
        }
    }

    // Construct from range of cell states
    for &state_count in STATE_COUNT_TEST_VALUES {
        let mut f = compile_test_fn(&format!(
            "@states {}
            @function CellFilter test(Cell state1, Cell state2) {{
                return state1..state2
            }}
            ",
            state_count,
        ));
        for (&cell_state1, &cell_state2) in iproduct!(test_values(), test_values()) {
            if cell_state1 as usize >= state_count || cell_state2 as usize >= state_count {
                continue;
            }
            let mut expected = CellStateFilter::none(state_count);
            let min_cell_state = std::cmp::min(cell_state1, cell_state2);
            let max_cell_state = std::cmp::max(cell_state1, cell_state2);
            for cell_state in min_cell_state..=max_cell_state {
                expected.set_bit(cell_state, true);
            }
            assert_fn_result(
                &mut f,
                &mut [
                    ConstValue::CellState(cell_state1),
                    ConstValue::CellState(cell_state2),
                ],
                Ok(ConstValue::CellStateFilter(expected)),
            );
        }
    }
}
