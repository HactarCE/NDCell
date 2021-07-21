use super::*;
use RtVal::{Cell, Integer};

#[test]
fn test_ambiguous_octothorpe_syntax_error() {
    let ambiguous_msg = "this is ambiguous; if it is a tag name, remove the space after '#'; if it is a variable name, wrap it in parentheses";
    TestProgram::new()
        .with_exec("a = # x0")
        .assert_syntax_error(test_error!(ambiguous_msg @ "x0"));
}

#[test]
fn test_convert_int_to_cell() {
    // Test with no `@states`.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Cell, "#(x0)")])
        .assert_test_cases(int_to_cell_test_cases(2));

    // Test with 5 states.
    TestProgram::new()
        .with_setup("@states 5")
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Cell, "#(x0)")])
        .assert_test_cases(int_to_cell_test_cases(5));

    // Test with 256 states.
    TestProgram::new()
        .with_setup("@states 256")
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Cell, "#(x0)")])
        .assert_test_cases(int_to_cell_test_cases(256));

    // Test before `@states`.
    TestProgram::new()
        .with_setup(
            "
                @init { x = #(2) }
                @states 5
            ",
        )
        .assert_init_errors(test_errors!("not yet reached '@states' directive" @ "#"));
}

fn int_to_cell_test_cases(state_count: LangInt) -> Vec<TestCase<'static>> {
    (-10..300).map_collect_vec(move |i| TestCase {
        inputs: vec![Integer(i)],
        expected_result: if 0 <= i && i < state_count {
            test_ok![Cell(i as u8)]
        } else {
            test_err!["invalid cell state ID" @ "(x0)"]
        },
    })
}
