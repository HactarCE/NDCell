//! Functions and methods that construct or operate on vectors.

use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{self, check_vector_len, CpVal, RtVal, SpannedRuntimeValueExt, Val};
use crate::errors::{Error, Fallible};
use crate::exec::{Compiler, Ctx, CtxTrait};
use crate::llvm;

/// Built-in function that constructs a vector from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VectorConstruct;
impl fmt::Display for VectorConstruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vector expression")
    }
}
impl Function for VectorConstruct {
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
            .map_err(|e| ctx.error(e))?;
        }
        check_vector_len(call.span, ret.len()).map_err(|e| ctx.error(e))?;
        Ok(RtVal::Vector(ret))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let mut ret = vec![];
        for arg in call.args {
            let arg = compiler.get_cp_val(arg)?;
            match arg.node {
                CpVal::Integer(i) => Ok(ret.push(i)),
                CpVal::Vector(v) => Ok(ret.extend_from_slice(&compiler.build_split_vector(v))),
                _ => Err(Error::expected(arg.span, "integer or vector")),
            }
            .map_err(|e| compiler.error(e))?;
        }
        check_vector_len(call.span, ret.len()).map_err(|e| compiler.error(e))?;
        Ok(Val::Cp(CpVal::Vector(llvm::VectorType::const_vector(&ret))))
    }
}

/// Built-in function that constructs a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Vec;
impl fmt::Display for Vec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec")
    }
}
impl Function for Vec {
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
        VecWithLen(len).eval(ctx, call)
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
        VecWithLen(len).compile(compiler, call)
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
