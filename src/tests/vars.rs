use super::assert_output;

#[test]
fn test_variable_init() {
    assert_output(
        Ok(0),
        "
        @transition {
            if 0 {
                // never executed, but declares x as an integer
                set x = 5
            }
            become #(x)
        }",
    )
}

#[test]
fn test_variable_undeclared() {
    assert_output(
        Err("Error at line 3; column 21
become #(x)
        ^^^   This variable must be initialized before it is used"),
        "
        @transition {
            become #(x)
        }",
    )
}

#[test]
fn test_variables() {
    assert_output(
        Ok(10),
        "
        @transition {
            set x = 3
            set another_variable = 9
            set x = another_variable - 2
            if 1 {
                become #(x + 3)
            }
        }",
    )
}
