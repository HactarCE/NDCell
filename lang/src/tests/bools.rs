use super::{assert_fn_result, compile_test_fn, test_values, ConstValue, LangInt};

#[test]
fn test_bool_convert() {
    let mut f = compile_test_fn(
        "@function int test(int x) {
            return bool(x)
        }",
    );
    for &x in test_values() {
        let args = [ConstValue::Int(x)];
        let expected = Ok(ConstValue::Int((x != 0) as LangInt));
        assert_fn_result(&mut f, &args, expected);
    }
}

#[test]
fn test_bool_ops() {
    let mut f = compile_test_fn(
        "@function vec8 test(int x, int y) {
            return [
                not x,
                not not x,
                x and y,
                x or  y,
                x xor y,
                not x and y,
                not x or  y,
                not x xor y,
            ]
        }",
    );
    for &x in test_values() {
        for &y in test_values() {
            let args = [ConstValue::Int(x), ConstValue::Int(y)];
            let expected = Ok(ConstValue::Vector(vec![
                (x == 0) as LangInt,               // not x
                (x != 0) as LangInt,               // not not x
                (x != 0 && y != 0) as LangInt,     // x and y
                (x != 0 || y != 0) as LangInt,     // x or  y
                ((x != 0) != (y != 0)) as LangInt, // x xor y
                (x == 0 && y != 0) as LangInt,     // not x and y
                (x == 0 || y != 0) as LangInt,     // not x or  y
                ((x == 0) != (y != 0)) as LangInt, // not x xor y
            ]));
            assert_fn_result(&mut f, &args, expected);
        }
    }
}
