//! Functions and methods that construct or operate on vectors.

use codemap::{Span, Spanned};
use std::fmt;

use ndcell_core::axis::Axis;

use super::{CallInfo, Function};
use crate::data::{
    self, CpVal, LangInt, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt, Type, Val,
};
use crate::errors::{Error, Fallible};
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt};
use crate::llvm;

/// Built-in function that constructs a vector from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VectorLiteral;
impl fmt::Display for VectorLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vector expression")
    }
}
impl Function for VectorLiteral {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let mut ret = vec![];
        for arg in call.args {
            match arg.node {
                RtVal::Integer(i) => Ok(ret.push(i)),
                RtVal::Vector(v) => Ok(ret.extend_from_slice(&v)),
                RtVal::IntegerSet(_) => Err(Error::unimplemented(arg.span)),
                RtVal::VectorSet(_) => Err(Error::unimplemented(arg.span)),
                _ => Err(Error::expected(arg.span, "integer or vector")),
            }
            .report_err(ctx)?;
        }
        data::check_vector_len(call.span, ret.len()).report_err(ctx)?;
        Ok(RtVal::Vector(ret))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let mut ret = vec![];
        for arg in &call.args {
            let arg = compiler.get_cp_val(arg)?;
            match arg.node {
                CpVal::Integer(i) => Ok(ret.push(i)),
                CpVal::Vector(v) => Ok(ret.extend_from_slice(&compiler.build_split_vector(v))),
                _ => Err(Error::expected(arg.span, "integer or vector")),
            }
            .report_err(compiler)?;
        }
        data::check_vector_len(call.span, ret.len()).report_err(compiler)?;
        Ok(Val::Cp(CpVal::Vector(llvm::VectorType::const_vector(&ret))))
    }
}

/// Built-in function that constructs a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecConstructor;
impl fmt::Display for VecConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec")
    }
}
impl Function for VecConstructor {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["len"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len = match call.kwarg("len") {
            Some(x) => x
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))
                .report_err(ctx)?,
            None => ctx.get_ndim(call.span)?,
        };
        eval_vec_construct(self, ctx, call, len)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let len = match call.kwarg("len") {
            Some(x) => compiler
                .get_rt_val(x)?
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))
                .report_err(compiler)?,
            None => compiler.get_ndim(call.span)?,
        };
        compile_vec_construct(self, compiler, call, len)
    }
}

/// Built-in function that constructs a vector with a specific length.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VecWithLen(pub usize);
impl fmt::Display for VecWithLen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec{}", self.0)
    }
}
impl Function for VecWithLen {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len = self.0;
        eval_vec_construct(self, ctx, call, len)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let len = self.0;
        compile_vec_construct(self, compiler, call, len)
    }
}

fn eval_vec_construct(
    f: &impl Function,
    ctx: &mut Ctx,
    call: CallInfo<Spanned<RtVal>>,
    len: usize,
) -> Fallible<RtVal> {
    match &call.args[..] {
        [] => Ok(RtVal::Vector(vec![0; len])),
        [arg] => Ok(RtVal::Vector(arg.to_vector(len).report_err(ctx)?)),
        _ => Err(call.invalid_args_error(ctx, f)),
    }
}
fn compile_vec_construct(
    f: &impl Function,
    compiler: &mut Compiler,
    call: CallInfo<Spanned<Val>>,
    len: usize,
) -> Fallible<Val> {
    match &call.args[..] {
        [] => Ok(Val::Rt(RtVal::Vector(vec![0; len]))),
        [arg] => Ok(Val::Cp(CpVal::Vector(
            compiler.build_convert_to_vector(arg, len)?,
        ))),
        _ => Err(call.invalid_args_error(compiler, f)),
    }
}
