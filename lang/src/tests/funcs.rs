use super::{assert_compile_error, assert_fn_compile_error};

// Test errors related to calling functions.
#[test]
fn test_function_errors() {
    // Function lookup error
    let source_code = "
        @function Int test(Int y) {
            return func_doesnt_exist(3, y)
        }
        @function Int other_func(Int x, Int y) {
            return x + y
        }
    ";
    let expected = ("func_doesnt_exist", "There is no function with this name");
    assert_compile_error(source_code, expected);

    // Bad arguments
    let source_code = "
        @function Int test(Int y) {
            return other_func([1, 2], y)
        }
        @function Int other_func(Int x, Int y) {
            return x + y
        }
    ";
    let expected = (
        "other_func([1, 2], y)",
        r#"Invalid arguments [Vec2, Int] for user-defined function other_func"#,
    );
    assert_compile_error(source_code, expected);
}

// Calling user functions is not supported (yet), but make sure that it doesn't
// panic or anything.
#[test]
fn test_userfunc() {
    let source_code = "
        @function Int test(Int y) {
            return other_func(3, y)
        }
        @function Int other_func(Int x, Int y) {
            return x + y
        }
    ";
    // let mut args = [ConstValue::Int(10)];
    // let expected = Ok(ConstValue::Int(13));
    // assert_threadlocal_fn_result(&USERFUNC_FN,&mut args, expected);
    let expected = ("other_func(3, y)", "This feature is unimplemented");
    assert_fn_compile_error(Some("test"), source_code, expected);
}
