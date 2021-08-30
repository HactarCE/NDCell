//! Functions and methods that construct or operate on cell states.

use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{
    CpVal, LangCell, LangInt, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt, Val,
};
use crate::errors::{Error, Fallible};
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt};
use crate::llvm;

/// Built-in function that converts an integer to a cell state.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct IntToCell;
impl fmt::Display for IntToCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#")
    }
}
impl Function for IntToCell {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx, self)?;
        let arg = call.args[0].clone();

        if let Ok(x) = arg.as_integer() {
            let state_count = ctx.get_states(call.span)? as LangInt;
            if 0 <= x && x < state_count {
                Ok(RtVal::Cell(x as LangCell))
            } else {
                Err(ctx.error(Error::cell_state_out_of_range(call.args[0].span)))
            }
        } else {
            Err(call.invalid_args_error(ctx, self))
        }
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        use llvm::IntPredicate::UGE;

        call.check_args_len(1, compiler, self)?;
        let arg = &call.args[0];
        let n = compiler
            .get_cp_val(arg)?
            .as_integer()
            .report_err(compiler)?;

        let cell_state_count = llvm::const_int(compiler.get_states(call.span)? as LangInt);
        let cell_state_id_is_invalid = compiler.builder().build_int_compare(
            UGE,
            n,
            cell_state_count,
            "cell_state_id_is_invalid",
        );

        let error_index = compiler.add_runtime_error(Error::cell_state_out_of_range(arg.span));
        compiler.build_return_err_if(cell_state_id_is_invalid, error_index)?;

        Ok(Val::Cp(CpVal::Cell(compiler.builder().build_int_truncate(
            n,
            llvm::cell_type(),
            "cell_state_from_int",
        ))))
    }
}
