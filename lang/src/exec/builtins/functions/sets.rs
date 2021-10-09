//! Functions and methods that construct or operate on sets.

use codemap::{Span, Spanned};
use std::fmt;
use std::sync::Arc;

use super::{CallInfo, Function};
use crate::data::{
    self, CellArray, CellSet, LangInt, RtVal, SpannedRuntimeValueExt, Type, VectorSet,
};
use crate::errors::{Error, Fallible, Result};
use crate::exec::{Ctx, CtxTrait, ErrorReportExt};

/// Built-in function that constructs an `EmptySet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptySet;
impl Function for EmptySet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx)?;
        Ok(RtVal::EmptySet)
    }
}

/// Built-in function that constructs an empty `IntegerSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyIntegerSet;
impl Function for EmptyIntegerSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx)?;
        Err(ctx.error(Error::unimplemented(call.span)))
    }
}

/// Built-in function that constructs an empty `CellSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyCellSet;
impl Function for EmptyCellSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx)?;
        Err(ctx.error(Error::unimplemented(call.span)))
    }
}

/// Built-in function that constructs a vector set.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecSetConstructor;
impl Function for VecSetConstructor {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["len"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len;
        let len_span;
        match call.kwarg("len") {
            Some(x) => {
                len = x
                    .as_integer()
                    .and_then(|n| data::check_vector_len(x.span, n))
                    .report_err(ctx)?;
                len_span = x.span;
            }
            None => {
                len = ctx.get_ndim(call.span)?;
                len_span = call.span;
            }
        }

        eval_vec_set_construct(ctx, call, len_span, len)
    }
}

/// Built-in function that constructs a vector set with a specific vector
/// length.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecSetWithLen(pub usize);
impl Function for VecSetWithLen {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let span = call.span;
        eval_vec_set_construct(ctx, call, span, self.0)
    }
}

fn eval_vec_set_construct(
    ctx: &mut Ctx,
    call: CallInfo<Spanned<RtVal>>,
    len_span: Span,
    len: usize,
) -> Fallible<RtVal> {
    match &call.args[..] {
        [] => Ok(RtVal::VectorSet(Arc::new(
            VectorSet::empty(len_span, len).report_err(ctx)?,
        ))),
        [arg] => Ok(RtVal::VectorSet(
            arg.to_vector_set(len_span, len).report_err(ctx)?,
        )),
        _ => Err(call.invalid_args_error(ctx))?,
    }
}

/// Built-in function that constructs a set from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SetLiteral;
impl Function for SetLiteral {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let span = call.span;
        call.args
            .into_iter()
            .try_fold(RtVal::EmptySet, |v1, v2| match (v1, set_of(v2)?) {
                (RtVal::EmptySet, s2) => Ok(s2.node),
                (RtVal::IntegerSet(s1), s2) => Err(Error::unimplemented(span)),
                (RtVal::CellSet(s1), s2) => Err(Error::unimplemented(span)),
                (RtVal::VectorSet(s1), s2) => {
                    let s2 = s2.as_vector_set(s1.vec_len())?;
                    Ok(s1.union(span, &s2)?.into())
                }
                _ => internal_error!("invalid fold type in set constructor"),
            })
            .report_err(ctx)
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
        RtVal::Integer(i) => Err(Error::unimplemented(span))?,
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

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx)?;

        let (radius, radius_span) = match call.kwarg("r") {
            Some(arg) => (arg.as_integer().report_err(ctx)?, arg.span),
            None => (1, call.span),
        };

        let ndim = match call.kwarg("d") {
            Some(arg) => arg.as_integer().report_err(ctx)?,
            None => ctx.get_ndim(call.span)? as LangInt,
        };

        Ok(VectorSet::moore(call.span, ndim, radius, radius_span)
            .report_err(ctx)?
            .into())
    }
}
