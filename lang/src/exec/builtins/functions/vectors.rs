//! Functions and methods that construct or operate on vectors.

use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{self, CpVal, RtVal, SpannedRuntimeValueExt, Val};
use crate::errors::Fallible;
use crate::exec::{Compiler, Ctx, CtxTrait};

/// Built-in function that constructs a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VectorConstruct;
impl fmt::Display for VectorConstruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec")
    }
}
impl Function for VectorConstruct {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["len"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len = match call.kwarg("len") {
            Some(x) => x
                .clone()
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))
                .map_err(|e| ctx.error(e))?,
            None => ctx.get_ndim(call.span)?,
        };
        VectorConstructWithLen(len).eval(ctx, call)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let len = match call.kwarg("len") {
            Some(x) => compiler
                .get_rt_val(x.clone())?
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))
                .map_err(|e| compiler.error(e))?,
            None => compiler.get_ndim(call.span)?,
        };
        VectorConstructWithLen(len).compile(compiler, call)
    }
}

/// Built-in function that constructs a vector with a specific length.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VectorConstructWithLen(pub usize);
impl fmt::Display for VectorConstructWithLen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec{}", self.0)
    }
}
impl Function for VectorConstructWithLen {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len = self.0;
        match &call.args[..] {
            [] => Ok(RtVal::Vector(vec![0; len])),
            [arg] => Ok(RtVal::Vector(arg.to_vector(len).map_err(|e| ctx.error(e))?)),
            _ => Err(call.invalid_args_error(ctx, self)),
        }
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let len = self.0;
        match &call.args[..] {
            [] => Ok(Val::Rt(RtVal::Vector(vec![0; len]))),
            [arg] => Ok(Val::Cp(CpVal::Vector(
                compiler.build_convert_to_vector(arg.clone(), len)?,
            ))),
            _ => Err(call.invalid_args_error(compiler, self)),
        }
    }
}
