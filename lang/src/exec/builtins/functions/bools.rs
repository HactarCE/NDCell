//! Non-short-circuiting boolean logic functions and operators.
//!
//! Logical AND and OR do not eagearly evaluate their operands, so they require
//! custom expression types.

use codemap::Spanned;

use super::{CallInfo, Function};
use crate::data::{CpVal, LangInt, RtVal, SpannedRuntimeValueExt, Val};
use crate::errors::Result;
use crate::exec::{Compiler, Ctx};
use crate::llvm;

/// Built-in function that converts a value to a boolean.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct ToBool;
impl Function for ToBool {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(1)?;
        let arg = call.arg(0)?;
        Ok(RtVal::Integer(arg.to_bool()? as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(1)?;
        let arg = call.arg(0)?;
        Ok(Val::Cp(CpVal::Integer(
            compiler.build_convert_to_bool(arg)?.llvm_int_value(),
        )))
    }
}

/// Built-in function that performs a two-input logical XOR operation.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct LogicalXor;
impl Function for LogicalXor {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(2)?;
        let lhs = call.arg(0)?.to_bool()?;
        let rhs = call.arg(1)?.to_bool()?;
        Ok(RtVal::Integer((lhs != rhs) as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(2)?;
        let lhs = compiler
            .build_convert_to_bool(call.arg(0)?)?
            .llvm_int_value();
        let rhs = compiler
            .build_convert_to_bool(call.arg(1)?)?
            .llvm_int_value();
        Ok(Val::Cp(CpVal::Integer(
            compiler.builder().build_xor(lhs, rhs, "xor"),
        )))
    }
}

/// Built-in function that performs a unary logical NOT operation.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct LogicalNot;
impl Function for LogicalNot {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(1)?;
        let arg = call.arg(0)?.to_bool()?;
        Ok(RtVal::Integer(!arg as LangInt))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(1)?;
        let arg = compiler
            .build_convert_to_bool(call.arg(0)?)?
            .llvm_int_value();
        Ok(Val::Cp(CpVal::Integer(compiler.builder().build_xor(
            arg,
            llvm::const_int(1),
            "logical_not",
        ))))
    }
}
