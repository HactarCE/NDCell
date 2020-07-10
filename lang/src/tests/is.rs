use itertools::Itertools;

use super::{assert_fn_result, compile_test_fn, CellStateFilter, ConstValue, LangInt};

#[test]
fn test_int_is_range() {
    let mut f = compile_test_fn("@function Int test(Int l, Range r) { return l is r }");
    let test_values = vec![-2, -1, 0, 1, 2];
    for (&i, &start, &end, &step) in
        iproduct!(&test_values, &test_values, &test_values, &test_values)
    {
        // println!(
        //     "Testing whether {:?} is in range ({:?}..{:?}).by({:?})",
        //     i, start, end, step,
        // );
        assert_fn_result(
            &mut f,
            &[
                ConstValue::Int(i),
                ConstValue::IntRange { start, end, step },
            ],
            // Note that the step doesn't actually matter.
            Ok(ConstValue::Int(
                ((start..=end).contains(&i) || (end..=start).contains(&i)) as LangInt,
            )),
        )
    }
}

#[test]
fn test_int_is_rectangle() {
    let mut f = compile_test_fn("@function Int test(Int l, Rect2 r) { return l is r }");
    let test_ints = vec![-2, 0, 2];
    let test_vecs = iproduct!(&test_ints, &test_ints)
        .map(|(&x, &y)| vec![x, y])
        .collect_vec();
    for (&i, start, end) in iproduct!(&test_ints, &test_vecs, &test_vecs) {
        // println!(
        //     "Testing whether {:?} is in range {:?}..{:?}",
        //     x, start, end,
        // );
        assert_fn_result(
            &mut f,
            &[
                ConstValue::Int(i),
                ConstValue::Rectangle(start.clone(), end.clone()),
            ],
            // Note that the step doesn't actually matter.
            Ok(ConstValue::Int(
                (
                    // Check X axis.
                    ((start[0]..=end[0]).contains(&i) || (end[0]..=start[0]).contains(&i))
                    // Check Y axis.
                    && ((start[1]..=end[1]).contains(&i) || (end[1]..=start[1]).contains(&i))
                ) as LangInt,
            )),
        )
    }
}

#[test]
fn test_vec_is_range() {
    let mut f = compile_test_fn("@function Int test(Vec3 l, Range r) { return l is r }");
    let test_ints = vec![-2, 0, 2];
    for (&x, &y, &z, &start, &end, &step) in
        iproduct!(&test_ints, &test_ints, &test_ints, &test_ints, &test_ints, &test_ints)
    {
        // println!(
        //     "Testing whether {:?} is in range ({:?}..{:?}).by({:?})",
        //     [x, y, z], start, end, step,
        // );
        assert_fn_result(
            &mut f,
            &[
                ConstValue::Vector(vec![x, y, z]),
                ConstValue::IntRange { start, end, step },
            ],
            // Note that the step doesn't actually matter.
            Ok(ConstValue::Int(
                (
                    // Check X axis.
                    ((start..=end).contains(&x) || (end..=start).contains(&x))
                    // Check Y axis.
                    && ((start..=end).contains(&y) || (end..=start).contains(&y))
                    // Check Z axis.
                    && ((start..=end).contains(&z) || (end..=start).contains(&z))
                ) as LangInt,
            )),
        )
    }
}

#[test]
fn test_vec_is_rectangle() {
    let test_ints = vec![-2, 0, 2];
    let test_vec2s = iproduct!(&test_ints, &test_ints)
        .map(|(&x, &y)| vec![x, y])
        .collect_vec();
    let test_vec3s = iproduct!(&test_ints, &test_ints, &test_ints)
        .map(|(&x, &y, &z)| vec![x, y, z])
        .collect_vec();

    // Same length
    let mut f = compile_test_fn("@function Int test(Vec2 l, Rect2 r) { return l is r }");
    for (v, start, end) in iproduct!(&test_vec2s, &test_vec2s, &test_vec2s) {
        assert_fn_result(
            &mut f,
            &[
                ConstValue::Vector(v.clone()),
                ConstValue::Rectangle(start.clone(), end.clone()),
            ],
            Ok(ConstValue::Int(
                (
                    // Check X axis.
                    ((start[0]..=end[0]).contains(&v[0]) || (end[0]..=start[0]).contains(&v[0]))
                    // Check Y axis.
                    && ((start[1]..=end[1]).contains(&v[1]) || (end[1]..=start[1]).contains(&v[1]))
                ) as LangInt,
            )),
        )
    }

    // LHS (vector) is longer
    let mut f = compile_test_fn("@function Int test(Vec3 l, Rect2 r) { return l is r }");
    for (v, start, end) in iproduct!(&test_vec3s, &test_vec2s, &test_vec2s) {
        assert_fn_result(
            &mut f,
            &[
                ConstValue::Vector(v.clone()),
                ConstValue::Rectangle(start.clone(), end.clone()),
            ],
            Ok(ConstValue::Int(
                (
                    // Check X axis.
                    ((start[0]..=end[0]).contains(&v[0]) || (end[0]..=start[0]).contains(&v[0]))
                    // Check Y axis.
                    && ((start[1]..=end[1]).contains(&v[1]) || (end[1]..=start[1]).contains(&v[1]))
                    // Check Z axis.
                    && v[2] == 0
                ) as LangInt,
            )),
        )
    }

    // RHS (rectangle) is longer
    let mut f = compile_test_fn("@function Int test(Vec2 l, Rect3 r) { return l is r }");
    for (v, start, end) in iproduct!(&test_vec2s, &test_vec3s, &test_vec3s) {
        assert_fn_result(
            &mut f,
            &[
                ConstValue::Vector(v.clone()),
                ConstValue::Rectangle(start.clone(), end.clone()),
            ],
            Ok(ConstValue::Int(
                (
                    // Check X axis.
                    ((start[0]..=end[0]).contains(&v[0]) || (end[0]..=start[0]).contains(&v[0]))
                    // Check Y axis.
                    && ((start[1]..=end[1]).contains(&v[1]) || (end[1]..=start[1]).contains(&v[1]))
                    // Check Z axis.
                    && ((start[2]..=end[2]).contains(&0) || (end[2]..=start[2]).contains(&0))
                ) as LangInt,
            )),
        )
    }
}

#[test]
fn test_cell_state_filter_membership() {
    let numbers_to_test = vec![
        0_usize, 1, 2, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256,
    ];
    for &state_count in &numbers_to_test[1..] {
        let mut f = compile_test_fn(&format!(
            "@states {}
            @function Int test(Cell l, CellFilter r) {{
                return l is r
            }}",
            state_count
        ));
        for &cell_state in &numbers_to_test {
            if cell_state >= state_count {
                continue;
            }
            assert_fn_result(
                &mut f,
                &[
                    ConstValue::CellState(cell_state as u8),
                    ConstValue::CellStateFilter(CellStateFilter::none(state_count)),
                ],
                Ok(ConstValue::Int(0)),
            );
            assert_fn_result(
                &mut f,
                &[
                    ConstValue::CellState(cell_state as u8),
                    ConstValue::CellStateFilter(CellStateFilter::single_cell_state(
                        state_count,
                        cell_state as u8,
                    )),
                ],
                Ok(ConstValue::Int(1)),
            );
        }
    }
}
