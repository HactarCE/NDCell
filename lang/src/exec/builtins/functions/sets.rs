//! Functions and methods that construct or operate on sets.

use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{CellSet, RtVal, SpannedRuntimeValueExt, Type, VectorSet};
use crate::errors::{Error, Fallible, Result};
use crate::exec::{Ctx, CtxTrait, ErrorReportExt};

/// Built-in function that constructs an `EmptySet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptySet;
impl_display!(for EmptySet, "{}.empty", Type::EmptySet);
impl Function for EmptySet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx, self)?;
        Ok(RtVal::EmptySet)
    }
}

/// Built-in function that constructs an empty `IntegerSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyIntegerSet;
impl_display!(for EmptyIntegerSet, "{}.empty", Type::IntegerSet);
impl Function for EmptyIntegerSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx, self)?;
        Err(ctx.error(Error::unimplemented(call.span)))
    }
}

/// Built-in function that constructs an empty `CellSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyCellSet;
impl_display!(for EmptyCellSet, "{}.empty", Type::CellSet);
impl Function for EmptyCellSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(0, ctx, self)?;
        Err(ctx.error(Error::unimplemented(call.span)))
    }
}

/// Built-in function that constructs an empty `VectorSet`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct EmptyVectorSet(pub Option<usize>);
impl fmt::Display for EmptyVectorSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.empty", Type::VectorSet(self.0))
    }
}
impl Function for EmptyVectorSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let vec_len = self.0.unwrap_or(ctx.get_ndim(call.span)?);
        call.check_args_len(0, ctx, self)?;
        Ok(VectorSet::empty(call.span, vec_len).report_err(ctx)?.into())
    }
}

/// Built-in function that constructs a set from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SetConstruct;
impl fmt::Display for SetConstruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "set expression")
    }
}
impl Function for SetConstruct {
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
    Ok(Spanned { span, node: ret })
}
