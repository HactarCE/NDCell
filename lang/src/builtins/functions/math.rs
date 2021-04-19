//! Math functions.

use codemap::{Span, Spanned};
use inkwell::types::VectorType;
use inkwell::values::{BasicValueEnum, IntMathValue};
use inkwell::IntPredicate;
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;

use super::{CallInfo, Function};
use crate::ast;
use crate::compiler::Compiler;
use crate::data::{
    CpVal, LangInt, LangUint, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt,
    SpannedTypeExt, Type, Val,
};
use crate::errors::{Error, Fallible, ReportError, Result};
use crate::llvm;
use crate::runtime::Runtime;

/// Built-in function that performs a fixed two-input integer math operation.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    fn eval_on_integers(self, span: Span, lhs: LangInt, rhs: LangInt) -> Result<LangInt> {
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
        self,
        runtime: &mut Runtime,
        span: Span,
        lhs: Spanned<RtVal>,
        rhs: Spanned<RtVal>,
    ) -> Fallible<RtVal> {
        if let (Ok(l), Ok(r)) = (lhs.clone().as_integer(), rhs.clone().as_integer()) {
            self.eval_on_integers(span, l, r)
                .map(RtVal::Integer)
                .map_err(|e| runtime.error(e))
        } else {
            Err(runtime.error(Error::invalid_arguments(span, self, &[lhs.ty(), rhs.ty()])))
        }
    }

    pub fn compile_for_int_math_values<M: llvm::IntMathValue<'static>>(
        self,
        compiler: &mut Compiler,
        span: Span,
        lhs: M,
        rhs: M,
    ) -> Fallible<llvm::BasicValueEnum> {
        match self {
            Self::Add => compiler.build_checked_int_arithmetic(
                lhs,
                rhs,
                "sadd",
                Error::integer_overflow(span),
            ),
            Self::Sub => todo!("compile op Sub"),
            Self::Mul => todo!("compile op Mul"),
            Self::Div => todo!("compile op Div"),
            Self::Mod => todo!("compile op Mod"),
            Self::Pow => todo!("compile op Pow"),
            Self::Shl => todo!("compile op Shl"),
            Self::ShrSigned => todo!("compile op ShrSigned"),
            Self::ShrUnsigned => todo!("compile op ShrUnsigned"),
            Self::BitwiseAnd => todo!("compile op BitwiseAnd"),
            Self::BitwiseOr => todo!("compile op BitwiseOr"),
            Self::BitwiseXor => todo!("compile op BitwiseXor"),
        }
    }

    pub fn compile_for_values(
        self,
        compiler: &mut Compiler,
        span: Span,
        lhs: Spanned<Val>,
        rhs: Spanned<Val>,
    ) -> Fallible<Val> {
        let l = compiler
            .get_cp_val(lhs)?
            .as_integer()
            .map_err(|e| compiler.error(e))?;
        let r = compiler
            .get_cp_val(rhs)?
            .as_integer()
            .map_err(|e| compiler.error(e))?;
        Ok(Val::Cp(CpVal::Integer(
            self.compile_for_int_math_values(compiler, span, l, r)?
                .into_int_value(),
        )))
    }
}
impl Function for BinaryOp {
    fn eval(&self, runtime: &mut Runtime, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(2, runtime, self);
        let lhs = call.args[0].clone();
        let rhs = call.args[1].clone();
        self.eval_on_values(runtime, call.span, lhs, rhs)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let lhs = call.args[0].clone();
        let rhs = call.args[1].clone();
        self.compile_for_values(compiler, call.span, lhs, rhs)
    }
}
