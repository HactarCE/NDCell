//! Range operator.

use codemap::{Span, Spanned};
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use ndcell_core::prelude::{IRect6D, IVec6D};

use super::{CallInfo, Function};
use crate::data::{LangInt, RtVal, VectorSet};
use crate::errors::{Error, Fallible};
use crate::exec::{Ctx, CtxTrait};

/// Built-in function that constructs a range between two arguments.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Range;
impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "..")
    }
}
impl Function for Range {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(2, ctx, self)?;
        let arg_a = &call.args[0];
        let arg_b = &call.args[1];
        match [&arg_a.node, &arg_b.node] {
            [RtVal::Integer(a), RtVal::Integer(b)] => {
                Err(ctx.error(Error::unimplemented(call.span)))
            }
            [RtVal::Vector(a), RtVal::Vector(b)] => {
                make_vector_range(ctx, call.span, a, arg_a.span, b, arg_b.span)
            }
            [RtVal::Vector(a), b] | [b, RtVal::Vector(a)] => {
                let b = b
                    .to_vector(a.len())
                    .ok_or_else(|| call.invalid_args_error(ctx, self))?;
                make_vector_range(ctx, call.span, a, arg_a.span, &b, arg_b.span)
            }
            _ => Err(call.invalid_args_error(ctx, self)),
        }
    }
}

fn ivec6_from_vec_langint(ctx: &mut Ctx, v: &[LangInt], span: Span) -> Fallible<IVec6D> {
    IVec6D::try_from_fn(|ax| (*v.get(ax as usize).unwrap_or(&0)).try_into().ok())
        .ok_or_else(|| ctx.error(Error::invalid_vector_set_size(span)))
}
fn make_vector_range(
    ctx: &mut Ctx,
    call_span: Span,
    a: &[LangInt],
    a_span: Span,
    b: &[LangInt],
    b_span: Span,
) -> Fallible<RtVal> {
    let vec_len = std::cmp::max(a.len(), b.len());
    let a = ivec6_from_vec_langint(ctx, a, a_span)?;
    let b = ivec6_from_vec_langint(ctx, b, b_span)?;
    VectorSet::rect(call_span, vec_len, IRect6D::span(a, b))
        .map_err(|e| ctx.error(e))
        .map(|set| RtVal::VectorSet(Arc::new(set)))
}