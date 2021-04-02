//! Math functions.

use codemap::{Span, Spanned};
use inkwell::types::VectorType;
use inkwell::values::{BasicValueEnum, IntMathValue};
use inkwell::IntPredicate;
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;

// pub use super::enums::{MinMaxMode, NegOrAbsMode};

// use super::{FuncConstructor, FuncResult};
// use crate::ast::{ErrorPointRef, FuncCallInfo, FuncCallInfoMut, Function};
// use crate::compiler::{const_int, const_uint, Compiler, Value};
// use crate::errors::*;
// use crate::lexer::OperatorToken;
// use crate::types::{CellStateFilter, LangInt, LangUint};
// use crate::{ConstValue, Type};
// use ErrorKind::{DivideByZero, IntegerOverflow, NegativeExponent};

use super::{CompileResult, FuncCall, Function};
use crate::ast;
use crate::compiler::{CompileValue, Compiler};
use crate::data::{LangInt, LangUint, SpannedTypeExt, SpannedValueExt, Value};
use crate::errors::{Error, Result};
use crate::runtime::{Runtime, RuntimeResult};

/// Built-in function that performs a fixed two-input integer math operation.
#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Shl,
    ShrSigned,
    ShrUnsigned,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Pow => write!(f, "**"),
            Self::Shl => write!(f, "<<"),
            Self::ShrSigned => write!(f, ">>"),
            Self::ShrUnsigned => write!(f, ">>>"),
            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseXor => write!(f, "^"),
        }
    }
}
impl From<ast::AssignOp> for Option<BinaryOp> {
    fn from(op: ast::AssignOp) -> Self {
        match op {
            ast::AssignOp::NoOp => None,
            ast::AssignOp::Add => Some(BinaryOp::Add),
            ast::AssignOp::Sub => Some(BinaryOp::Sub),
            ast::AssignOp::Mul => Some(BinaryOp::Mul),
            ast::AssignOp::Div => Some(BinaryOp::Div),
            ast::AssignOp::Mod => Some(BinaryOp::Mod),
            ast::AssignOp::Pow => Some(BinaryOp::Pow),
            ast::AssignOp::Shl => Some(BinaryOp::Shl),
            ast::AssignOp::ShrSigned => Some(BinaryOp::ShrSigned),
            ast::AssignOp::ShrUnsigned => Some(BinaryOp::ShrUnsigned),
            ast::AssignOp::And => Some(BinaryOp::BitwiseAnd),
            ast::AssignOp::Or => Some(BinaryOp::BitwiseOr),
            ast::AssignOp::Xor => Some(BinaryOp::BitwiseXor),
        }
    }
}
impl BinaryOp {
    /// Evaluates this operation for two integers.
    fn eval_on_integers(&self, lhs: LangInt, rhs: LangInt, span: Span) -> Result<LangInt> {
        // Perform the operation.
        match self {
            Self::Add => lhs.checked_add(rhs),
            Self::Sub => lhs.checked_sub(rhs),
            Self::Mul => lhs.checked_mul(rhs),
            Self::Div => {
                if rhs == 0 {
                    Err(Error::division_by_zero(span))?
                } else {
                    lhs.checked_div(rhs)
                }
            }
            Self::Mod => {
                if rhs == 0 {
                    Err(Error::division_by_zero(span))?
                } else {
                    lhs.checked_rem(rhs)
                }
            }
            Self::Pow => {
                if rhs < 0 {
                    Err(Error::negative_exponent(span))?
                } else {
                    rhs.try_into().ok().map(|exp| lhs.pow(exp))
                }
            }

            Self::Shl => rhs.try_into().ok().and_then(|rhs| lhs.checked_shl(rhs)),
            Self::ShrSigned => rhs.try_into().ok().and_then(|rhs| lhs.checked_shr(rhs)),
            Self::ShrUnsigned => rhs
                .try_into()
                .ok()
                .and_then(|rhs| (lhs as LangUint).checked_shr(rhs))
                .map(|i| i as LangInt),

            Self::BitwiseAnd => Some(lhs & rhs),
            Self::BitwiseOr => Some(lhs | rhs),
            Self::BitwiseXor => Some(lhs ^ rhs),
        }
        // If the operation returned None, assume an integer overflow error.
        .ok_or_else(|| Error::integer_overflow(span))
    }

    /// Evaluates this operation for two values.
    ///
    /// `span` is the span of the operator, not the entire expression.
    pub fn eval_on_values(
        &self,
        span: Span,
        lhs: Spanned<Value>,
        rhs: Spanned<Value>,
    ) -> Result<Value> {
        if let (Ok(l), Ok(r)) = (lhs.clone().as_integer(), rhs.clone().as_integer()) {
            self.eval_on_integers(l, r, span).map(Value::Integer)
        } else {
            Err(Error::invalid_arguments(span, self, &[lhs.ty(), rhs.ty()]))
        }
    }
}
impl Function for BinaryOp {
    fn eval(&self, runtime: &mut Runtime, call: FuncCall<'_>) -> RuntimeResult<Value> {
        let args = call.eval_args(runtime)?;

        if call.args.len() == 2 {
            let result = self.eval_on_values(call.span, args[0].clone(), args[1].clone());
            return Ok(result?); // `?` casts from `Error` to `RuntimeError`
        }

        Err(
            Error::invalid_arguments(call.span, self, &args.iter().map(|v| v.ty()).collect_vec())
                .into(),
        )
    }
}
