//! Functions and methods that construct or operate on sets.

use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{CellSet, RtVal, SpannedRuntimeValueExt, VectorSet};
use crate::errors::{Error, Fallible, Result};
use crate::exec::{Ctx, CtxTrait};

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
            .map_err(|e| ctx.error(e))
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
        RtVal::Vector(v) => {
            let vec = crate::utils::vec_to_ivec6d(v)
                .ok_or(Error::invalid_vector_length_for_set(span, v.len()))?;
            VectorSet::single_vector(v.len(), vec)?.into()
        }

        // Otherwise, return an error.
        _ => Err(Error::invalid_set_type(span, &x.ty()))?,
    };
    Ok(Spanned { span, node: ret })
}
