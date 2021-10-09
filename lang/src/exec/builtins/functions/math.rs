//! Math functions.

use codemap::{Span, Spanned};
use std::convert::TryInto;
use std::fmt;

use super::{CallInfo, Function};
use crate::ast;
use crate::data::{
    self, CpVal, LangInt, LangUint, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt, Type,
    Val,
};
use crate::errors::{Error, Fallible, Result};
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt};
use crate::llvm;

/// Built-in function that performs a fixed two-input integer math operation.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryMathOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Shl,
    ShrSigned,
    ShrUnsigned,

    And,
    Or,
    Xor,
}
impl fmt::Display for BinaryMathOp {
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
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Xor => write!(f, "^"),
        }
    }
}
impl From<ast::AssignOp> for BinaryMathOp {
    fn from(op: ast::AssignOp) -> Self {
        match op {
            ast::AssignOp::Add => BinaryMathOp::Add,
            ast::AssignOp::Sub => BinaryMathOp::Sub,
            ast::AssignOp::Mul => BinaryMathOp::Mul,
            ast::AssignOp::Div => BinaryMathOp::Div,
            ast::AssignOp::Mod => BinaryMathOp::Mod,
            ast::AssignOp::Pow => BinaryMathOp::Pow,
            ast::AssignOp::Shl => BinaryMathOp::Shl,
            ast::AssignOp::ShrSigned => BinaryMathOp::ShrSigned,
            ast::AssignOp::ShrUnsigned => BinaryMathOp::ShrUnsigned,
            ast::AssignOp::And => BinaryMathOp::And,
            ast::AssignOp::Or => BinaryMathOp::Or,
            ast::AssignOp::Xor => BinaryMathOp::Xor,
        }
    }
}
impl From<ast::BinaryOp> for Option<BinaryMathOp> {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => Some(BinaryMathOp::Add),
            ast::BinaryOp::Sub => Some(BinaryMathOp::Sub),
            ast::BinaryOp::Mul => Some(BinaryMathOp::Mul),
            ast::BinaryOp::Div => Some(BinaryMathOp::Div),
            ast::BinaryOp::Mod => Some(BinaryMathOp::Mod),
            ast::BinaryOp::Pow => Some(BinaryMathOp::Pow),
            ast::BinaryOp::Shl => Some(BinaryMathOp::Shl),
            ast::BinaryOp::ShrSigned => Some(BinaryMathOp::ShrSigned),
            ast::BinaryOp::ShrUnsigned => Some(BinaryMathOp::ShrUnsigned),
            ast::BinaryOp::And => Some(BinaryMathOp::And),
            ast::BinaryOp::Or => Some(BinaryMathOp::Or),
            ast::BinaryOp::Xor => Some(BinaryMathOp::Xor),
            ast::BinaryOp::LogicalAnd => None,
            ast::BinaryOp::LogicalOr => None,
            ast::BinaryOp::LogicalXor => None,
            ast::BinaryOp::Range => None,
            ast::BinaryOp::Is => None,
        }
    }
}
impl BinaryMathOp {
    /// Returns whether this operation is defined for sets.
    fn is_set_op(self) -> bool {
        matches!(self, Self::Or | Self::And | Self::Sub | Self::Xor)
    }
    /// Returns the function used to merge two vector lengths for this
    /// operation.
    fn vec_len_merger(self) -> fn(usize, usize) -> usize {
        match self {
            BinaryMathOp::Add | BinaryMathOp::Sub => std::cmp::max,

            BinaryMathOp::Mul
            | BinaryMathOp::Div
            | BinaryMathOp::Mod
            | BinaryMathOp::Pow
            | BinaryMathOp::Shl
            | BinaryMathOp::ShrSigned
            | BinaryMathOp::ShrUnsigned
            | BinaryMathOp::And => std::cmp::min,

            BinaryMathOp::Or | BinaryMathOp::Xor => std::cmp::max,
        }
    }

    /// Evaluates this operation for two integers.
    fn eval_on_integers(self, span: Span, lhs: LangInt, rhs: LangInt) -> Result<LangInt> {
        // Perform the operation.
        match self {
            Self::Add => lhs.checked_add(rhs),
            Self::Sub => lhs.checked_sub(rhs),
            Self::Mul => lhs.checked_mul(rhs),
            Self::Div => lhs.checked_div_euclid(rhs),
            Self::Mod => lhs.checked_rem_euclid(rhs),
            Self::Pow => crate::utils::checked_pow_i64(lhs, rhs),

            Self::Shl => rhs.try_into().ok().and_then(|rhs| lhs.checked_shl(rhs)),
            Self::ShrSigned => rhs.try_into().ok().and_then(|rhs| lhs.checked_shr(rhs)),
            Self::ShrUnsigned => rhs
                .try_into()
                .ok()
                .and_then(|rhs| (lhs as LangUint).checked_shr(rhs))
                .map(|i| i as LangInt),

            Self::And => Some(lhs & rhs),
            Self::Or => Some(lhs | rhs),
            Self::Xor => Some(lhs ^ rhs),
        }
        // If the operation returned None, assume an integer overflow error.
        .ok_or_else(|| match self {
            Self::Div | Self::Mod if rhs == 0 => Error::division_by_zero(span),
            Self::Pow if rhs < 0 => Error::negative_exponent(span),
            Self::Shl | Self::ShrSigned | Self::ShrUnsigned => Error::bitshift_out_of_range(span),
            _ => Error::integer_overflow(span),
        })
    }
    /// Evaluates this operation for two values.
    ///
    /// `span` is the span of the operator, not the entire expression.
    pub fn eval_on_values(
        self,
        ctx: &mut Ctx,
        span: Span,
        lhs: &Spanned<RtVal>,
        rhs: &Spanned<RtVal>,
    ) -> Fallible<RtVal> {
        // Math operations on vectors (componentwise).
        if let Some((l, r)) = data::coerce_vectors_together(&lhs, &rhs, self.vec_len_merger()) {
            return Ok(RtVal::Vector(
                l.into_iter()
                    .zip(r)
                    .map(|(a, b)| self.eval_on_integers(span, a, b))
                    .collect::<Result<_>>()
                    .report_err(ctx)?,
            ));
        }

        match (&lhs.node, &rhs.node) {
            // Math operations on integers.
            (RtVal::Integer(l), RtVal::Integer(r)) => {
                self.eval_on_integers(span, *l, *r).map(RtVal::Integer)
            }

            // Set operations.
            (RtVal::EmptySet, RtVal::EmptySet) if self.is_set_op() => Ok(RtVal::EmptySet),
            (RtVal::IntegerSet(l), _) if self.is_set_op() => {
                let r = rhs.as_integer_set().report_err(ctx)?;
                match self {
                    Self::Or => Err(Error::unimplemented(span)),
                    Self::And => Err(Error::unimplemented(span)),
                    Self::Sub => Err(Error::unimplemented(span)),
                    Self::Xor => Err(Error::unimplemented(span)),
                    _ => Err(internal_error_value!("invalid set op")),
                }
            }
            (RtVal::CellSet(l), _) => {
                let r = rhs.as_cell_set().report_err(ctx)?;
                match self {
                    Self::Or => Err(Error::unimplemented(span)),
                    Self::And => Err(Error::unimplemented(span)),
                    Self::Sub => Err(Error::unimplemented(span)),
                    Self::Xor => Err(Error::unimplemented(span)),
                    _ => Err(internal_error_value!("invalid set op")),
                }
            }
            (RtVal::VectorSet(l), _) => {
                let r = rhs.as_vector_set(l.vec_len()).report_err(ctx)?;
                match self {
                    Self::Or => l.union(span, &*r),
                    Self::And => l.intersection(span, &*r),
                    Self::Sub => l.difference(span, &*r),
                    Self::Xor => l.symmetric_difference(span, &*r),
                    _ => Err(internal_error_value!("invalid set op")),
                }
                .map(RtVal::from)
            }

            _ => Err(Error::invalid_arguments(span, self, &[lhs.ty(), rhs.ty()])),
        }
        .report_err(ctx)
    }

    /// Compiles this operation for two LLVM math values.
    pub fn compile_for_int_math_values<M: llvm::IntMathValue>(
        self,
        compiler: &mut Compiler,
        span: Span,
        lhs: M,
        rhs: M,
    ) -> Fallible<llvm::BasicValueEnum> {
        let b = compiler.builder();
        match self {
            Self::Add => compiler.build_checked_int_arithmetic(span, "sadd", lhs, rhs),
            Self::Sub => compiler.build_checked_int_arithmetic(span, "ssub", lhs, rhs),
            Self::Mul => compiler.build_checked_int_arithmetic(span, "smul", lhs, rhs),
            Self::Div => compiler.build_checked_int_div_euclid(span, lhs, rhs),
            Self::Mod => compiler.build_checked_int_rem_euclid(span, lhs, rhs),
            Self::Pow => compiler.build_checked_int_pow(span, lhs, rhs),
            Self::Shl => compiler.build_checked_int_shl(span, lhs, rhs),
            Self::ShrSigned => compiler.build_checked_int_ashr(span, lhs, rhs),
            Self::ShrUnsigned => compiler.build_checked_int_lshr(span, lhs, rhs),
            Self::And => Ok(b.build_and(lhs, rhs, "").as_basic_value_enum()),
            Self::Or => Ok(b.build_or(lhs, rhs, "").as_basic_value_enum()),
            Self::Xor => Ok(b.build_xor(lhs, rhs, "").as_basic_value_enum()),
        }
    }

    /// Compiles this operation for two values.
    pub fn compile_for_values(
        self,
        compiler: &mut Compiler,
        span: Span,
        lhs: &Spanned<Val>,
        rhs: &Spanned<Val>,
    ) -> Fallible<Val> {
        let l = compiler
            .get_cp_val(lhs)?
            .as_integer()
            .report_err(compiler)?;
        let r = compiler
            .get_cp_val(rhs)?
            .as_integer()
            .report_err(compiler)?;
        Ok(Val::Cp(CpVal::Integer(
            self.compile_for_int_math_values(compiler, span, l, r)?
                .into_int_value(),
        )))
    }
}
impl Function for BinaryMathOp {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(2, ctx)?;
        let lhs = &call.args[0];
        let rhs = &call.args[1];

        self.eval_on_values(ctx, call.span, lhs, rhs)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(2, compiler)?;
        let lhs = &call.args[0];
        let rhs = &call.args[1];

        let lhs = compiler.get_cp_val(lhs)?;
        let rhs = compiler.get_cp_val(rhs)?;

        // Math operations on vectors (componentwise).
        if let Some((l, r)) =
            compiler.build_coerce_vectors_together(&lhs, &rhs, self.vec_len_merger())
        {
            return Ok(Val::Cp(CpVal::Vector(
                self.compile_for_int_math_values(compiler, call.span, l, r)?
                    .into_vector_value(),
            )));
        }

        match (lhs.ty(), rhs.ty()) {
            (Type::Integer, Type::Integer) => {
                let l = lhs.as_integer().report_err(compiler)?;
                let r = rhs.as_integer().report_err(compiler)?;
                let result = self.compile_for_int_math_values(compiler, call.span, l, r)?;
                Ok(Val::Cp(CpVal::Integer(result.into_int_value())))
            }

            _ => Err(call.invalid_args_error(compiler)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryMathOp {
    Pos,
    Neg,

    BitwiseNot,
}
impl fmt::Display for UnaryMathOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryMathOp::Pos => write!(f, "+"),
            UnaryMathOp::Neg => write!(f, "-"),
            UnaryMathOp::BitwiseNot => write!(f, "~"),
        }
    }
}
impl From<ast::PrefixOp> for Option<UnaryMathOp> {
    fn from(op: ast::PrefixOp) -> Self {
        match op {
            ast::PrefixOp::Pos => Some(UnaryMathOp::Pos),
            ast::PrefixOp::Neg => Some(UnaryMathOp::Neg),
            ast::PrefixOp::BitwiseNot => Some(UnaryMathOp::BitwiseNot),
            ast::PrefixOp::LogicalNot => None,
            ast::PrefixOp::IntToCell => None,
        }
    }
}
impl UnaryMathOp {
    /// Evaluates this operation for an integer.
    fn eval_on_integers(self, span: Span, arg: LangInt) -> Result<LangInt> {
        match self {
            Self::Pos => Ok(arg),
            Self::Neg => arg.checked_neg().ok_or(Error::integer_overflow(span)),
            Self::BitwiseNot => Ok(!arg),
        }
    }

    /// Evaluates this operation for a values.
    pub fn eval_on_value(self, ctx: &mut Ctx, span: Span, arg: &Spanned<RtVal>) -> Fallible<RtVal> {
        if let Ok(x) = arg.as_integer() {
            self.eval_on_integers(span, x)
                .map(RtVal::Integer)
                .report_err(ctx)
        } else {
            Err(ctx.error(Error::invalid_arguments(span, self, &[arg.ty()])))
        }
    }

    /// Compiles this operation for an LLVM math value.
    pub fn compile_for_int_math_values<M: llvm::IntMathValue>(
        self,
        compiler: &mut Compiler,
        span: Span,
        arg: M,
    ) -> Fallible<llvm::BasicValueEnum> {
        let b = compiler.builder();
        let zero = arg.same_type_const_zero();
        match self {
            Self::Pos => Ok(arg.as_basic_value_enum()),
            Self::Neg => compiler.build_checked_int_arithmetic(span, "ssub", zero, arg),
            Self::BitwiseNot => Ok(b.build_not(arg, "bitwise_not").as_basic_value_enum()),
        }
    }

    /// Compiles this operation for a value.
    pub fn compile_for_value(
        self,
        compiler: &mut Compiler,
        span: Span,
        arg: &Spanned<Val>,
    ) -> Fallible<Val> {
        let x = compiler
            .get_cp_val(arg)?
            .as_integer()
            .report_err(compiler)?;
        Ok(Val::Cp(CpVal::Integer(
            self.compile_for_int_math_values(compiler, span, x)?
                .into_int_value(),
        )))
    }
}
impl Function for UnaryMathOp {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx)?;
        let arg = &call.args[0];
        self.eval_on_value(ctx, call.span, arg)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(1, compiler)?;
        let arg = &call.args[0];
        self.compile_for_value(compiler, call.span, arg)
    }
}
