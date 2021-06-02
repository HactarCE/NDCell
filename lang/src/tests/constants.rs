use super::*;
use RtVal::Integer;

#[test]
fn test_bool_constants() {
    TestProgram::new()
        .with_result_expressions(&[(Type::Integer, "FALSE"), (Type::Integer, "TRUE")])
        .assert_test_cases(&[(vec![], Ok(vec![Integer(0), Integer(1)]))]);
}

#[test]
fn test_ndim_constant() {
    test_ndim_constant_with_ndim(None);
    for ndim in 1..6 {
        test_ndim_constant_with_ndim(Some(ndim));
    }
}
fn test_ndim_constant_with_ndim(ndim: Option<LangInt>) {
    let setup_str = match ndim {
        Some(n) => format!("@ndim {}", n),
        None => "/* default ndim */".to_owned(),
    };
    TestProgram::new()
        .with_setup(&setup_str)
        .with_result_expressions(&[(Type::Integer, "NDIM")])
        .assert_test_cases(&[(vec![], Ok(vec![Integer(ndim.unwrap_or(2))]))]);
}

#[test]
fn test_statecount_constant() {
    test_statecount_constant_with_statecount(None);
    for &statecount in &[1, 2, 10, 256] {
        test_statecount_constant_with_statecount(Some(statecount));
    }
}
fn test_statecount_constant_with_statecount(statecount: Option<LangInt>) {
    let setup_str = match statecount {
        Some(n) => format!("@states {}", n),
        None => "/* default states */".to_owned(),
    };
    TestProgram::new()
        .with_setup(&setup_str)
        .with_result_expressions(&[(Type::Integer, "STATECOUNT")])
        .assert_test_cases(&[(vec![], Ok(vec![Integer(statecount.unwrap_or(2))]))]);
}
