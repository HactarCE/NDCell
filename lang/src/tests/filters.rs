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
            state_count
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
                &[arg1.clone(), arg2.clone(), ConstValue::Int(0)],
                Ok(ConstValue::CellStateFilter(f1.clone() & f2.clone())),
            );
            assert_fn_result(
                &mut f,
                &[arg1.clone(), arg2.clone(), ConstValue::Int(1)],
                Ok(ConstValue::CellStateFilter(f1.clone() | f2.clone())),
            );
            assert_fn_result(
                &mut f,
                &[arg1.clone(), arg2.clone(), ConstValue::Int(2)],
                Ok(ConstValue::CellStateFilter(f1.clone() ^ f2.clone())),
            );
            assert_fn_result(
                &mut f,
                &[arg1.clone(), arg2.clone(), ConstValue::Int(3)],
                Ok(ConstValue::CellStateFilter(!f1.clone())),
            );
        }
    }
}
