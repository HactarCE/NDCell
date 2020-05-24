use proptest::prelude::*;

use super::{assert_output, ConstValue, LangInt};

proptest! {
    #[test]
    fn proptest_arithmetic(x: LangInt, y: LangInt) {
        // Addition
        let source_code = "@function int test(int x, int y) { return x + y }";
        let expected = x
            .checked_add(y)
            .map(ConstValue::Int)
            .ok_or(("x + y", "Integer overflow"));
        assert_output(
            &[ConstValue::Int(x), ConstValue::Int(y)],
            expected,
            source_code,
            Some("test"),
        );
    }
}

// #[test]
// fn test_arithmetic() {
//     // Addition
//     assert_output(
//         Ok(ConstValue::CellState(9)),
//         "
//         @transition {
//             become #(5 + 4)
//         }
//         @states 10",
//     );

//     // Subtraction
//     assert_output(
//         Ok(ConstValue::CellState(1)),
//         "
//         @transition {
//             become #(5 - 4)
//         }",
//     );

//     // Multiplication
//     assert_output(
//         Ok(ConstValue::CellState(20)),
//         "
//         @transition {
//             become #(5 * 4)
//         }
//         @states 21",
//     );

//     // Division
//     assert_output(
//         Ok(ConstValue::CellState(3)),
//         "
//         @transition {
//             become #(10 / 3)
//         }
//         @states 4",
//     );

//     // Remainder
//     assert_output(
//         Ok(ConstValue::CellState(1)),
//         "
//         @transition {
//             become #(10 % 3)
//         }",
//     );

//     // Negation
//     assert_output(
//         Ok(ConstValue::CellState(12)),
//         "
//         @transition {
//             become #-(-12)
//         }
//         @states 13",
//     );
// }

// #[test]
// fn test_overflow() {
//     // Addition
//     assert_output(
//         Err("Error at line 3; column 22
// become #(9223372036854775805 + 3)
//          ^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
//         "
//         @transition {
//             become #(9223372036854775805 + 3)
//         }",
//     );

//     // Subtraction (negative end)
//     assert_output(
//         Err("Error at line 3; column 22
// become #(-9223372036854775805 - 4)
//          ^^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
//         "
//         @transition {
//             become #(-9223372036854775805 - 4)
//         }",
//     );

//     // Subtraction (positive end)
//     assert_output(
//         Err("Error at line 3; column 22
// become #(9223372036854775805 - -3)
//          ^^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
//         "
//         @transition {
//             become #(9223372036854775805 - -3)
//         }",
//     );

//     // Multiplication
//     assert_output(
//         Err("Error at line 3; column 22
// become #(8589934592 * 8589934592)
//          ^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
//         "
//         @transition {
//             become #(8589934592 * 8589934592)
//         }",
//     );

//     // Division
//     assert_output(
//         Err("Error at line 3; column 22
// become #(-9223372036854775808 / -1)
//          ^^^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
//         "
//         @transition {
//             become #(-9223372036854775808 / -1)
//         }",
//     );

//     // Negation
//     assert_output(
//         Err("Error at line 3; column 22
// become #(--9223372036854775808)
//          ^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
//         "
//         @transition {
//             become #(--9223372036854775808)
//         }",
//     );

//     // Divide by zero
//     assert_output(
//         Err("Error at line 3; column 22
// become #(12 / 0)
//          ^^^^^^   Divide by zero"),
//         "
//         @transition {
//             become #(12 / 0)
//         }",
//     );

//     // Divide by zero with remainder
//     assert_output(
//         Err("Error at line 3; column 22
// become #(12 % 0)
//          ^^^^^^   Divide by zero"),
//         "
//         @transition {
//             become #(12 % 0)
//         }",
//     );
// }
