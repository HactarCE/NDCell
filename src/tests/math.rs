use super::assert_output;

#[test]
fn test_arithmetic() {
    // Addition
    assert_output(
        Ok(9),
        "
        @transition {
            become #(5 + 4)
        }
        @states 10",
    );

    // Subtraction
    assert_output(
        Ok(1),
        "
        @transition {
            become #(5 - 4)
        }",
    );

    // Multiplication
    assert_output(
        Ok(20),
        "
        @transition {
            become #(5 * 4)
        }
        @states 21",
    );

    // Division
    assert_output(
        Ok(3),
        "
        @transition {
            become #(10 / 3)
        }
        @states 4",
    );

    // Remainder
    assert_output(
        Ok(1),
        "
        @transition {
            become #(10 % 3)
        }",
    );

    // Negation
    assert_output(
        Ok(12),
        "
        @transition {
            become #-(-12)
        }
        @states 13",
    );
}

#[test]
fn test_overflow() {
    // Addition
    assert_output(
        Err("Error at line 3; column 22
become #(9223372036854775805 + 3)
         ^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
        "
        @transition {
            become #(9223372036854775805 + 3)
        }",
    );

    // Subtraction (negative end)
    assert_output(
        Err("Error at line 3; column 22
become #(-9223372036854775805 - 4)
         ^^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
        "
        @transition {
            become #(-9223372036854775805 - 4)
        }",
    );

    // Subtraction (positive end)
    assert_output(
        Err("Error at line 3; column 22
become #(9223372036854775805 - -3)
         ^^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
        "
        @transition {
            become #(9223372036854775805 - -3)
        }",
    );

    // Multiplication
    assert_output(
        Err("Error at line 3; column 22
become #(8589934592 * 8589934592)
         ^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
        "
        @transition {
            become #(8589934592 * 8589934592)
        }",
    );

    // Division
    assert_output(
        Err("Error at line 3; column 22
become #(-9223372036854775808 / -1)
         ^^^^^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
        "
        @transition {
            become #(-9223372036854775808 / -1)
        }",
    );

    // Negation
    assert_output(
        Err("Error at line 3; column 22
become #(--9223372036854775808)
         ^^^^^^^^^^^^^^^^^^^^^   Integer overflow"),
        "
        @transition {
            become #(--9223372036854775808)
        }",
    );

    // Divide by zero
    assert_output(
        Err("Error at line 3; column 22
become #(12 / 0)
         ^^^^^^   Divide by zero"),
        "
        @transition {
            become #(12 / 0)
        }",
    );

    // Divide by zero with remainder
    assert_output(
        Err("Error at line 3; column 22
become #(12 % 0)
         ^^^^^^   Divide by zero"),
        "
        @transition {
            become #(12 % 0)
        }",
    );
}
