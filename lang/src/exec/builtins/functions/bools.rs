//! Non-short-circuiting boolean logic functions and operators.
//!
//! Logical AND and OR do not eagearly evaluate their operands, so they require
//! custom expression types.

use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{CpVal, LangInt, RtVal, SpannedRuntimeValueExt, Val};
use crate::errors::Fallible;
use crate::exec::{Compiler, Ctx, ErrorReportExt};
use crate::llvm;

/// Built-in function that converts a value to a boolean.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct ToBool;
impl fmt::Display for ToBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bool")
    }
}
impl Function for ToBool {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx, self)?;
        Ok(RtVal::Integer(
            call.args[0].to_bool().report_err(ctx)? as LangInt
        ))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(1, compiler, self)?;
        Ok(Val::Cp(CpVal::Integer(
            compiler.build_convert_to_bool(call.args[0].clone())?,
        )))
    }
}

/// Built-in function that performs a two-input logical XOR operation.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct LogicalXor;
impl fmt::Display for LogicalXor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "xor")
    }
}
impl Function for LogicalXor {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(2, ctx, self)?;
        let lhs = call.args[0].to_bool().report_err(ctx);
        let rhs = call.args[1].to_bool().report_err(ctx);
        Ok(RtVal::Integer((lhs != rhs) as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(2, compiler, self)?;
        let lhs = compiler.build_convert_to_bool(call.args[0].clone())?;
        let rhs = compiler.build_convert_to_bool(call.args[1].clone())?;
        Ok(Val::Cp(CpVal::Integer(
            compiler.builder().build_xor(lhs, rhs, "xor"),
        )))
    }
}

/// Built-in function that performs a unary logical NOT operation.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct LogicalNot;
impl fmt::Display for LogicalNot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "not")
    }
}
impl Function for LogicalNot {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx, self)?;
        let arg = call.args[0].to_bool().report_err(ctx)?;
        Ok(RtVal::Integer(!arg as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(1, compiler, self)?;
        let arg = compiler.build_convert_to_bool(call.args[0].clone())?;
        Ok(Val::Cp(CpVal::Integer(compiler.builder().build_xor(
            arg,
            llvm::const_int(1),
            "logical_not",
        ))))
    }
}
