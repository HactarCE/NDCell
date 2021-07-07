use super::*;
use RtVal::Integer;

const POW_TEST_VALUES: &[LangInt] = &[
    LangInt::MIN,
    LangInt::MIN + 1,
    -100,
    -55,
    -10,
    -5,
    -2,
    -1,
    0,
    1,
    2,
    5,
    10,
    55,
    100,
    LangInt::MAX - 1,
    LangInt::MAX,
];

const BITSHIFT_TEST_VALUES: &[LangInt] = &[
    LangInt::MIN,
    LangInt::MIN + 1,
    -65,
    -64,
    -63,
    -5,
    -2,
    -1,
    0,
    1,
    2,
    5,
    63,
    64,
    65,
    LangInt::MAX - 1,
    LangInt::MAX,
];

#[test]
fn test_integer_math_binops() {
    // Test addition.
    let ref_impl = op_with_overflow_reference_impl("+", LangInt::checked_add);
    test_integer_binop("+", ref_impl);
    // Test subtraction.
    let ref_impl = op_with_overflow_reference_impl("-", LangInt::checked_sub);
    test_integer_binop("-", ref_impl);
    // Test multiplication.
    let ref_impl = op_with_overflow_reference_impl("*", LangInt::checked_mul);
    test_integer_binop("*", ref_impl);

    // Test Euclidean division.
    let ref_impl = op_with_div_by_zero_or_overflow_reference_impl("/", LangInt::checked_div_euclid);
    test_integer_binop("/", ref_impl);
    // Test Euclidean modulus.
    let ref_impl = op_with_div_by_zero_or_overflow_reference_impl("%", LangInt::checked_rem_euclid);
    test_integer_binop("%", ref_impl);

    // Test exponentiation.
    let ref_impl = pow_op_reference_impl;
    test_integer_binop_with_values("**", ref_impl, POW_TEST_VALUES);
}

#[test]
fn test_integer_bitwise_binops() {
    // Test bitwise AND.
    test_integer_binop("&", |a, b| Ok(vec![a & b]));
    // Test bitwise OR.
    test_integer_binop("|", |a, b| Ok(vec![a | b]));
    // Test bitwise XOR.
    test_integer_binop("^", |a, b| Ok(vec![a ^ b]));

    // Test left shift.
    let ref_impl = bitshift_op_reference_impl("<<", LangInt::checked_shl);
    test_integer_binop_with_values("<<", ref_impl, BITSHIFT_TEST_VALUES);
    // Test signed (arithmetic) right shift.
    let ref_impl = bitshift_op_reference_impl(">>", LangInt::checked_shr);
    test_integer_binop_with_values(">>", ref_impl, BITSHIFT_TEST_VALUES);
    // Test unsigned (logical) right shift.
    let ref_impl = bitshift_op_reference_impl(">>>", |a, b| {
        Some((a as LangUint).checked_shr(b)? as LangInt)
    });
    test_integer_binop_with_values(">>>", ref_impl, BITSHIFT_TEST_VALUES);
}

fn op_with_overflow_reference_impl<'s>(
    op_str: &'s str,
    checked_fn: fn(LangInt, LangInt) -> Option<LangInt>,
) -> impl Fn(LangInt, LangInt) -> TestResult<'s, LangInt> {
    move |a, b| match checked_fn(a, b) {
        Some(result) => Ok(vec![result]),
        None => Err(vec![(op_str, "integer overflow")]),
    }
}
fn op_with_div_by_zero_or_overflow_reference_impl<'s>(
    op_str: &'s str,
    checked_fn: fn(LangInt, LangInt) -> Option<LangInt>,
) -> impl Fn(LangInt, LangInt) -> TestResult<'s, LangInt> {
    move |a, b| match checked_fn(a, b) {
        Some(result) => Ok(vec![result]),
        None if b == 0 => Err(vec![(op_str, "division by zero")]),
        None => Err(vec![(op_str, "integer overflow")]),
    }
}
fn pow_op_reference_impl(a: LangInt, b: LangInt) -> TestResult<'static, LangInt> {
    match crate::utils::checked_pow_i64(a, b) {
        Some(result) => Ok(vec![result]),
        None if b < 0 => Err(vec![("**", "negative exponent")]),
        None => Err(vec![("**", "integer overflow")]),
    }
}
fn bitshift_op_reference_impl<'s>(
    op_str: &'s str,
    f: fn(LangInt, u32) -> Option<LangInt>,
) -> impl Fn(LangInt, LangInt) -> TestResult<'s, LangInt> {
    move |a, b| match b.try_into().ok().and_then(|b| f(a, b)) {
        Some(result) => Ok(vec![result]),
        None => Err(vec![(op_str, "bitshift amount out of range")]),
    }
}

fn test_integer_binop<'s>(
    op_str: &'s str,
    op_reference_impl: impl Fn(LangInt, LangInt) -> TestResult<'s, LangInt>,
) {
    test_integer_binop_with_values(op_str, op_reference_impl, test_values());
}
fn test_integer_binop_with_values<'s>(
    op_str: &'s str,
    op_reference_impl: impl Fn(LangInt, LangInt) -> TestResult<'s, LangInt>,
    values: &'s [LangInt],
) {
    TestProgram::new()
        .with_input_types(&[Type::Integer, Type::Integer])
        .with_result_expressions(&[(Type::Integer, &format!("x0 {} x1", op_str))])
        .assert_test_cases(iproduct!(values, values).map(|(&a, &b)| {
            let inputs = vec![Integer(a), Integer(b)];
            let output = op_reference_impl(a, b);
            (inputs, output)
        }));
}

#[test]
fn test_integer_math_unary_ops() {
    // Test unary plus.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "+x0")])
        .assert_test_cases(test_values().iter().map(|&i| {
            let input = vec![Integer(i)];
            let output = Ok(vec![i]);
            (input, output)
        }));

    // Test unary negation.
    TestProgram::new()
        .with_input_types(&[Type::Integer])
        .with_result_expressions(&[(Type::Integer, "-x0")])
        .assert_test_cases(test_values().iter().map(|&i| {
            let input = vec![Integer(i)];
            let output = match i.checked_neg() {
                Some(result) => Ok(vec![result]),
                None => Err(vec![("-", "integer overflow")]),
            };
            (input, output)
        }));
}
