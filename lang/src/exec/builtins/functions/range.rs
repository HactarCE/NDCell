//! Range operator.

use codemap::{Span, Spanned};
use std::sync::Arc;

use super::{CallInfo, Function};
use crate::data::{LangInt, RtVal, VectorSet};
use crate::errors::{Error, Result};
use crate::exec::Ctx;

/// Built-in function that constructs a range between two arguments.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Range;
impl Function for Range {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(2)?;
        let arg_a = &call.args[0];
        let arg_b = &call.args[1];
        match [&arg_a.node, &arg_b.node] {
            [RtVal::Integer(a), RtVal::Integer(b)] => Err(Error::unimplemented(call.span)),
            [RtVal::Vector(a), RtVal::Vector(b)] => {
                make_vector_range(call.span, a, arg_a.span, b, arg_b.span)
            }
            [RtVal::Vector(a), b] | [b, RtVal::Vector(a)] => {
                let b = b
                    .to_vector(a.len())
                    .ok_or_else(|| call.invalid_args_error())?;
                make_vector_range(call.span, a, arg_a.span, &b, arg_b.span)
            }
            _ => Err(call.invalid_args_error()),
        }
    }
}

fn make_vector_range(
    call_span: Span,
    a: &[LangInt],
    a_span: Span,
    b: &[LangInt],
    b_span: Span,
) -> Result<RtVal> {
    VectorSet::rect(call_span, a, a_span, b, b_span).map(|set| RtVal::VectorSet(Arc::new(set)))
}
