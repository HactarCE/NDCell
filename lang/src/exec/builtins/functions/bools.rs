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
impl Function for ToBool {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx)?;
        let arg = &call.args[0];
        Ok(RtVal::Integer(arg.to_bool().report_err(ctx)? as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(1, compiler)?;
        let arg = &call.args[0];
        Ok(Val::Cp(CpVal::Integer(
            compiler.build_convert_to_bool(arg)?,
        )))
    }
}

/// Built-in function that performs a two-input logical XOR operation.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct LogicalXor;
impl Function for LogicalXor {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(2, ctx)?;
        let lhs = call.args[0].to_bool().report_err(ctx);
        let rhs = call.args[1].to_bool().report_err(ctx);
        Ok(RtVal::Integer((lhs != rhs) as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(2, compiler)?;
        let lhs = compiler.build_convert_to_bool(&call.args[0])?;
        let rhs = compiler.build_convert_to_bool(&call.args[1])?;
        Ok(Val::Cp(CpVal::Integer(
            compiler.builder().build_xor(lhs, rhs, "xor"),
        )))
    }
}

/// Built-in function that performs a unary logical NOT operation.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct LogicalNot;
impl Function for LogicalNot {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx)?;
        let arg = call.args[0].to_bool().report_err(ctx)?;
        Ok(RtVal::Integer(!arg as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(1, compiler)?;
        let arg = compiler.build_convert_to_bool(&call.args[0])?;
        Ok(Val::Cp(CpVal::Integer(compiler.builder().build_xor(
            arg,
            llvm::const_int(1),
            "logical_not",
        ))))
    }
}
