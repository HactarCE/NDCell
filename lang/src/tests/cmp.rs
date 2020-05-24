use proptest::prelude::*;

use super::{
    assert_threadlocal_fn_result, compile_test, test_values, CompiledFunction, ConstValue, LangInt,
};

thread_local! {
    static EQL_FN: CompiledFunction =
        compile_test("@function int test(int x, int y) { return x == y }");
    static NEQ_FN: CompiledFunction =
        compile_test("@function int test(int x, int y) { return x != y }");
    static LT_FN: CompiledFunction =
        compile_test("@function int test(int x, int y) { return x < y }");
    static GT_FN: CompiledFunction =
        compile_test("@function int test(int x, int y) { return x > y }");
    static LTE_FN: CompiledFunction =
        compile_test("@function int test(int x, int y) { return x <= y }");
    static GTE_FN: CompiledFunction =
        compile_test("@function int test(int x, int y) { return x >= y }");
}

// Test with random inputs.
proptest! {
    #[test]
    fn proptest_comparisons(x: LangInt, y: LangInt) {
        test_comparisons(x, y);
    }
}

// And make sure to cover several corner cases (e.g. division by zero).
#[test]
fn test_comparisons_corner_cases() {
    for &x in test_values() {
        for &y in test_values() {
            println!("Testing comparisons with inputs {:?}", (x, y));
            test_comparisons(x, y);
        }
    }
}

fn test_comparisons(x: LangInt, y: LangInt) {
    let args = [ConstValue::Int(x), ConstValue::Int(y)];

    // Equal
    let expected = Ok(ConstValue::Int((x == y).into()));
    assert_threadlocal_fn_result(&EQL_FN, &args, expected);

    // Not equal
    let expected = Ok(ConstValue::Int((x != y).into()));
    assert_threadlocal_fn_result(&NEQ_FN, &args, expected);

    // Less than
    let expected = Ok(ConstValue::Int((x < y).into()));
    assert_threadlocal_fn_result(&LT_FN, &args, expected);

    // Greater than
    let expected = Ok(ConstValue::Int((x > y).into()));
    assert_threadlocal_fn_result(&GT_FN, &args, expected);

    // Less than or equal
    let expected = Ok(ConstValue::Int((x <= y).into()));
    assert_threadlocal_fn_result(&LTE_FN, &args, expected);

    // Greater than or equal
    let expected = Ok(ConstValue::Int((x >= y).into()));
    assert_threadlocal_fn_result(&GTE_FN, &args, expected);
}

// #[test]
// fn test_branch_nonzero() {
//     assert_output(
//         Ok(ConstValue::CellState(10)),
//         "
//         @transition {
//             if 0 {
//             } else {
//                 if 1 {
//                     if 2 {
//                         if -1 {
//                             become #10
//                         }
//                     }
//                 }
//             }
//             become #0
//         }
//         @states 11",
//     );
// }

// #[test]
// fn test_comparisons() {
//     assert_output(
//         Ok(ConstValue::CellState(1)),
//         "
//         @transition {
//             set x = 3
//             set y = 4
//             set z = 4
//             if y == z {
//                 if x == y {
//                 } else {
//                     if x != y {
//                         if y != z {
//                         } else {
//                             if x < y < z {
//                             } else if x < y <= z {
//                                 if x <= z {
//                                     become #(y >= x)
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//             become #0
//         }",
//     );
// }
