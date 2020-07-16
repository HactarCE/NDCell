//! Tests for passing values between Rust and LLVM.

use super::{assert_fn_result, compile_test_fn, CellStateFilter, ConstValue};

#[test]
fn test_ints_to_bytes() {
    let mut f = compile_test_fn(
        "@function Void test(Int a, Int b, Int c) {
            assert a == 10
            assert b == -20
            assert c == 30
        }",
    );
    assert_fn_result(
        &mut f,
        &mut [
            ConstValue::Int(10),
            ConstValue::Int(-20),
            ConstValue::Int(30),
        ],
        Ok(ConstValue::Void),
    );
}

#[test]
fn test_mixed_to_bytes() {
    let mut f = compile_test_fn(
        "@states 100
        @function Void test(Int a, Cell b, Vec7 c, Range d, Rect3 e, CellFilter f) {
            assert a == 10
            assert b == #20
            assert c == [-7, -6, -5, -4, -3, -2, -1]
            assert d == (30..40).by(2)
            assert e == [50, -60, 70]..[80, -90, 100]
            assert f == #0 | #3 | #10 | #99
        }",
    );
    assert_fn_result(
        &mut f,
        &mut [
            ConstValue::Int(10),
            ConstValue::CellState(20),
            ConstValue::Vector((-7..=-1).collect()),
            ConstValue::IntRange {
                start: 30,
                end: 40,
                step: 2,
            },
            ConstValue::Rectangle(vec![50, -60, 70], vec![80, -90, 100]),
            ConstValue::CellStateFilter(CellStateFilter::from_cell_states(100, &[0, 3, 10, 99])),
        ],
        Ok(ConstValue::Void),
    );
}
