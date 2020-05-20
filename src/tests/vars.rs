use super::{assert_output, ConstValue};

#[test]
fn test_variable_init() {
    assert_output(
        Ok(ConstValue::CellState(0)),
        "
        @transition {
            if 0 {
                // never executed, but declares x as an integer
                set x = 5
            }
            become #(x)
        }",
    );
}

#[test]
fn test_variable_undeclared() {
    assert_output(
        Err("Error at line 3; column 22
become #(x)
         ^   This variable must be initialized before it is used"),
        "
        @transition {
            become #(x)
        }",
    );
}

#[test]
fn test_variables() {
    assert_output(
        Ok(ConstValue::CellState(10)),
        "
        @transition {
            set x = 3
            set another_variable = 9
            set x = another_variable - 2
            if 1 {
                become #(x + 3)
            }
        }
        @states 11",
    );
}

#[test]
fn test_variable_types() {
    // Cell state -> integer
    assert_output(
        Err("Error at line 4; column 21
set s = 3
        ^   Type error: expected cell state but got integer"),
        "
        @transition {
            set s = #2
            set s = 3
        }",
    );

    // Integer -> cell state
    assert_output(
        Err("Error at line 4; column 21
set s = #2
        ^^   Type error: expected integer but got cell state"),
        "
        @transition {
            set s = 3
            set s = #2
        }",
    );

    // Return an integer
    assert_output(
        Err("TODO"),
        "
        @transition {
            return 0
        }",
    )
}
