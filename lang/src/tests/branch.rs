use super::{
    assert_fn_result, assert_threadlocal_fn_result, compile_test_fn, test_values, CompiledFunction,
    ConstValue, LangInt,
};

thread_local! {
    static IF_FN: CompiledFunction =
        compile_test_fn("@function int test(int cond) { if cond { return 10 } else { return 20 } }");
}

#[test]
fn test_condition_values() {
    for &cond in test_values() {
        let args = [ConstValue::Int(cond)];
        let expected = Ok(ConstValue::Int(if cond != 0 { 10 } else { 20 }));
        assert_threadlocal_fn_result(&IF_FN, &args, expected);
    }
}

#[test]
fn test_branching() {
    for (&cond1, &cond2, &add_else, &ret_val1, &ret_val2, &ret_val3, &ret_val4) in iproduct!(
        &[None, Some(0), Some(1)], // cond1
        &[None, Some(0), Some(1)], // cond2
        &[false, true],            // add_else
        &[None, Some(10)],         // ret_val1
        &[None, Some(20)],         // ret_val2
        &[None, Some(30)],         // ret_val3
        &[None, Some(40)]          // ret_val4
    ) {
        let mut expected_ret = None;
        let mut skip_to_end = false;
        let mut source_code = String::new();
        source_code.push_str("@function int test() {");
        if let Some(cond1) = cond1 {
            source_code.push_str(" if ");
            source_code.push_str(&cond1.to_string());
            source_code.push_str("{");
            push_ret(&mut source_code, ret_val1, &mut expected_ret, cond1 != 0);
            if cond1 != 0 {
                skip_to_end = true;
            }
            source_code.push_str("}");

            if let Some(cond2) = cond2 {
                source_code.push_str("else if ");
                source_code.push_str(&cond2.to_string());
                source_code.push_str("{");
                push_ret(
                    &mut source_code,
                    ret_val2,
                    &mut expected_ret,
                    !skip_to_end && cond2 != 0,
                );
                if cond2 != 0 {
                    skip_to_end = true;
                }
                source_code.push_str("}");
            }

            if add_else {
                source_code.push_str("else {");
                push_ret(&mut source_code, ret_val3, &mut expected_ret, !skip_to_end);
                source_code.push_str("}");
            }

            push_ret(&mut source_code, ret_val4, &mut expected_ret, true);
        }
        source_code.push_str("}");
        let expected_ret = ConstValue::Int(expected_ret.unwrap_or(0));

        let mut function = compile_test_fn(&source_code);
        assert_fn_result(&mut function, &[], Ok(expected_ret));
    }
}

pub fn push_ret(
    s: &mut String,
    ret_val: Option<LangInt>,
    expected_ret: &mut Option<LangInt>,
    expect_this_to_exec: bool,
) {
    if let Some(ret) = ret_val {
        s.push_str("return ");
        s.push_str(&ret.to_string());
        if expect_this_to_exec {
            expected_ret.get_or_insert(ret);
        }
    }
}
