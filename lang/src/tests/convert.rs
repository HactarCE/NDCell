use super::*;
use RtVal::{Cell, Integer};

#[test]
fn test_ambiguous_octothorpe_syntax_error() {
    let ambiguous_msg = "this is ambiguous; if it is a tag name, remove the space after '#'; if it is a variable name, wrap it in parentheses";
    TestProgram::new()
        .with_exec("# x0")
        .assert_syntax_error(("x0", ambiguous_msg));
}
