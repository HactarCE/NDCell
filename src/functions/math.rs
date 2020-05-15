//! Math functions.

use std::convert::TryInto;

use super::super::ast::{
    ArgValues, ErrorPointRef, FnSignature, Function, FunctionKind, UserFunction,
};
use super::super::compiler::{Compiler, Value};
use super::super::errors::*;
use super::super::lexer::OperatorToken;
use super::super::types::LangInt;
use super::super::{ConstValue, Span, Type};
use LangErrorMsg::{DivideByZero, IntegerOverflow, InternalError, NegativeExponent};

/// Built-in function that negates an integer.
#[derive(Debug)]
pub struct NegInt {
    /// Error returned if overflow occurs.
    overflow_error: ErrorPointRef,
}
impl NegInt {
    /// Returns a new NegInt instance.
    pub fn try_new(userfunc: &mut UserFunction, span: Span) -> LangResult<Self> {
        Ok(Self {
            overflow_error: userfunc.add_error_point(IntegerOverflow.with_span(span)),
        })
    }
}
impl Function for NegInt {
    fn name(&self) -> String {
        format!("<unary {:?} operator>", OperatorToken::Minus)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::new(vec![Type::Int], Type::Int)]
    }
    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let arg = args.compile(compiler, 0)?.as_int()?;
        // To negate an integer, subtract it from zero.
        Ok(Value::Int(compiler.build_checked_int_arithmetic(
            compiler.int_type().const_zero(),
            arg,
            "ssub",
            |c| Ok(self.overflow_error.compile(c)),
        )?))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        match args.const_eval(0)?.as_int()?.checked_neg() {
            Some(result) => Ok(Some(ConstValue::Int(result))),
            None => self.overflow_error.err(),
        }
    }
}

/// Built-in function that performs a fixed two-input integer math operation.
#[derive(Debug)]
pub struct BinaryIntOp {
    /// Token signifying what operation to perform.
    op: OperatorToken,
    /// Error returned if overflow occurs.
    overflow_error: Option<ErrorPointRef>,
    /// Error returned if the divisor of an operation is negative.
    div_by_zero_error: Option<ErrorPointRef>,
    /// Error returned if the exponent is negative.
    negative_exponent_error: Option<ErrorPointRef>,
}
impl BinaryIntOp {
    /// Constructs a new BinaryIntOp instance that performs the given operation.
    pub fn try_new(userfunc: &mut UserFunction, span: Span, op: OperatorToken) -> LangResult<Self> {
        use OperatorToken::*;
        let overflow_error = if matches!(op, Plus | Minus | Asterisk | Slash | Percent) {
            Some(userfunc.add_error_point(IntegerOverflow.with_span(span)))
        } else {
            None
        };
        let div_by_zero_error = if matches!(op, Slash | Percent) {
            Some(userfunc.add_error_point(DivideByZero.with_span(span)))
        } else {
            None
        };
        let negative_exponent_error = if matches!(op, DoubleAsterisk) {
            Some(userfunc.add_error_point(NegativeExponent.with_span(span)))
        } else {
            None
        };
        Ok(Self {
            op,
            overflow_error,
            div_by_zero_error,
            negative_exponent_error,
        })
    }
    /// Returns the OverflowError error point; panics if this function cannot
    /// return an Err(OverflowError).
    fn overflow_error(&self) -> &ErrorPointRef {
        self.overflow_error.as_ref().unwrap()
    }
    /// Returns the DivideByZero error point; panics if this function cannot
    /// return an Err(DivideByZero).
    fn div_by_zero_error(&self) -> &ErrorPointRef {
        self.div_by_zero_error.as_ref().unwrap()
    }
    /// Returns the NegativeExponent error point; panics if this function cannot
    /// return an Err(NegativeExponent).
    fn negative_exponent_error(&self) -> &ErrorPointRef {
        self.negative_exponent_error.as_ref().unwrap()
    }
}
impl Function for BinaryIntOp {
    fn name(&self) -> String {
        format!("<binary {:?} operator>", self.op)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }
    fn signatures(&self) -> Vec<FnSignature> {
        vec![FnSignature::new(vec![Type::Int], Type::Int)]
    }
    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let lhs = args.compile(compiler, 0)?.as_int()?;
        let rhs = args.compile(compiler, 1)?.as_int()?;
        let b = compiler.builder();
        use OperatorToken::*;
        // Perform the operation.
        Ok(Value::Int(match self.op {
            // Addition, subtraction, and multiplication
            Plus | Minus | Asterisk => {
                let intrinsic = match self.op {
                    Plus => "sadd",     // Addition
                    Minus => "ssub",    // Subtraction
                    Asterisk => "smul", // Multiplication
                    _ => unreachable!(),
                };
                compiler.build_checked_int_arithmetic(lhs, rhs, intrinsic, |c| {
                    Ok(self.overflow_error().compile(c))
                })?
            }
            // Division and remainder (TODO: use euclidean div and modulo)
            Slash | Percent => {
                // Check for overflow and division by zero.
                compiler.build_div_check(
                    lhs,
                    rhs,
                    |c| Ok(self.overflow_error().compile(c)),
                    |c| Ok(self.div_by_zero_error().compile(c)),
                )?;
                match self.op {
                    // Division
                    Slash => compiler.builder().build_int_signed_div(lhs, rhs, "tmp_div"),
                    // Remainder
                    Percent => compiler.builder().build_int_signed_rem(lhs, rhs, "tmp_rem"),
                    _ => unreachable!(),
                }
            }
            // Exponentiation
            DoubleAsterisk => todo!("Exponentiation"),
            // Bitshift left
            DoubleLessThan => b.build_left_shift(lhs, rhs, "tmp_shl"),
            // Bitshift right (arithmetic)
            DoubleGreaterThan => b.build_right_shift(lhs, rhs, true, "tmp_ashr"),
            // Bitshift right (logical)
            TripleGreaterThan => b.build_right_shift(lhs, rhs, false, "tmp_lshr"),
            // Bitwise AND
            Ampersand => b.build_and(lhs, rhs, "tmp_and"),
            // Bitwise OR
            Pipe => b.build_or(lhs, rhs, "tmp_or"),
            // Bitwise XOR
            Caret => b.build_xor(lhs, rhs, "tmp_xor"),
            // Anything else
            _ => Err(InternalError("Uncaught invalid operator".into()).without_span())?,
        }))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let lhs = args.const_eval(0)?.as_int()?;
        let rhs = args.const_eval(1)?.as_int()?;
        use OperatorToken::*;
        // Perform the operation.
        match self.op {
            // Addition
            Plus => lhs.checked_add(rhs),
            // Subtraction
            Minus => lhs.checked_sub(rhs),
            // Multiplication
            Asterisk => lhs.checked_mul(rhs),
            // Division or remainder (TODO: use euclidean div and modulo)
            Slash | Percent => {
                if rhs == 0 {
                    self.div_by_zero_error().err()?
                } else {
                    match self.op {
                        Slash => lhs.checked_div(rhs),
                        Percent => lhs.checked_rem(rhs),
                        _ => unreachable!(),
                    }
                }
            }
            // Exponentiation
            DoubleAsterisk => {
                if rhs < 0 {
                    self.negative_exponent_error().err()?
                } else {
                    rhs.try_into().ok().map(|exp| lhs.pow(exp))
                }
            }
            // Bitshift left
            DoubleLessThan => Some(lhs << rhs),
            // Bitshift right (arithmetic)
            DoubleGreaterThan => Some(lhs >> rhs),
            // Bitshift right (logical)
            TripleGreaterThan => Some((lhs as u64 >> rhs) as LangInt),
            // Bitwise AND
            Ampersand => Some(lhs & rhs),
            // Bitwise OR
            Pipe => Some(lhs | rhs),
            // Bitwise XOR
            Caret => Some(lhs ^ rhs),
            // Anything else
            _ => Err(InternalError("Uncaught invalid operator".into()).without_span())?,
        }
        // IntegerOverflow error by default
        .ok_or_else(|| self.overflow_error().error())
        .map(ConstValue::Int)
        .map(Some)
    }
}
