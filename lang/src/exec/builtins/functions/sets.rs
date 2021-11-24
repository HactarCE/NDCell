//! Functions and methods that construct or operate on sets.

use codemap::{Span, Spanned};
use std::sync::Arc;

use super::{CallInfo, Function};
use crate::data::{
    self, CellSet, GetType, IntegerSet, LangInt, RtVal, SpannedRuntimeValueExt, VectorSet,
};
use crate::errors::{Error, Result};
use crate::exec::{Ctx, CtxTrait};

/// Built-in function that constructs an `EmptySet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptySet;
impl Function for EmptySet {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(0)?;
        Ok(RtVal::EmptySet)
    }
}

/// Built-in function that constructs an empty `IntegerSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyIntegerSet;
impl Function for EmptyIntegerSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(0)?;
        Ok(IntegerSet::empty().into())
    }
}

/// Built-in function that constructs an empty `CellSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyCellSet;
impl Function for EmptyCellSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(0)?;
        Err(Error::unimplemented(call.expr_span))
    }
}

/// Built-in function that constructs a vector set.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecSetConstructor;
impl Function for VecSetConstructor {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["len"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let len;
        let len_span;
        match call.kwarg("len") {
            Some(x) => {
                len = x
                    .as_integer()
                    .and_then(|n| data::check_vector_len(x.span, n))?;
                len_span = x.span;
            }
            None => {
                len = ctx.get_ndim(call.expr_span)?;
                len_span = call.span;
            }
        }

        eval_vec_set_construct(call, len_span, len)
    }
}

/// Built-in function that constructs a vector set with a specific vector
/// length.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecSetWithLen(pub usize);
impl Function for VecSetWithLen {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let len_span = call.span;
        eval_vec_set_construct(call, len_span, self.0)
    }
}

fn eval_vec_set_construct(
    call: CallInfo<Spanned<RtVal>>,
    len_span: Span,
    len: usize,
) -> Result<RtVal> {
    match &call.args[..] {
        [] => Ok(RtVal::VectorSet(Arc::new(VectorSet::empty(len_span, len)?))),
        [arg] => Ok(RtVal::VectorSet(arg.to_vector_set(len_span, len)?)),
        _ => Err(call.invalid_args_error())?,
    }
}

/// Built-in function that constructs a set from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SetLiteral;
impl Function for SetLiteral {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.args
            .into_iter()
            .try_fold(RtVal::EmptySet, |v1, v2| match (v1, set_of(v2)?) {
                (RtVal::EmptySet, s2) => Ok(s2.node),
                (RtVal::IntegerSet(s1), s2) => {
                    let s2 = s2.as_integer_set()?;
                    Ok(s1.union(call.span, &s2)?.into())
                }
                (RtVal::CellSet(s1), s2) => Err(Error::unimplemented(call.expr_span)),
                (RtVal::VectorSet(s1), s2) => {
                    let s2 = s2.as_vector_set(s1.vec_len())?;
                    Ok(s1.union(call.span, &s2)?.into())
                }
                _ => internal_error!("invalid fold type in set constructor"),
            })
    }
}

/// Returns a set of a value.
///
/// - If the value is already a set, return it.
/// - If the value can be a member of a set, wrap it in a set and return it.
/// - Otherwise, return an error.
fn set_of(x: Spanned<RtVal>) -> Result<Spanned<RtVal>> {
    // If the value is already a set, return it.
    if let RtVal::EmptySet | RtVal::IntegerSet(_) | RtVal::CellSet(_) | RtVal::VectorSet(_) =
        &x.node
    {
        return Ok(x);
    }

    let span = x.span;
    let ret: RtVal = match &x.node {
        // If the value can be a member of a set, wrap it in a set and return
        // it.
        RtVal::Integer(i) => IntegerSet::single_integer(*i).into(),
        RtVal::Cell(i) => CellSet::single_cell(*i).into(),
        RtVal::Vector(v) => VectorSet::single_vector(span, v, span)?.into(),

        // Otherwise, return an error.
        _ => Err(Error::invalid_set_type(span, &x.ty()))?,
    };
    Ok(Spanned { node: ret, span })
}

/// Built-in function that generates a pre-defined shape.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum VectorSetShape {
    Square,
    Diamond,
    Moore,
    Vn,
    Circular,
    L2,
    Checkerboard,
    Hash,
    Cross,
    Saltire,
    Star,
}
impl Function for VectorSetShape {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["r", "d"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(0)?;

        let (radius, radius_span) = match call.kwarg("r") {
            Some(arg) => (arg.as_integer()?, arg.span),
            None => (1, call.span),
        };

        let ndim = match call.kwarg("d") {
            Some(arg) => arg.as_integer()?,
            None => ctx.get_ndim(call.expr_span)? as LangInt,
        };

        Ok(VectorSet::moore(call.expr_span, ndim, radius, radius_span)?.into())
    }
}
