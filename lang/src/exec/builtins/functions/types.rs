use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{self, RtVal, SpannedRuntimeValueExt, Type};
use crate::errors::{Error, Fallible};
use crate::exec::{Ctx, CtxTrait, ErrorReportExt};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct TypeBrackets;
impl Function for TypeBrackets {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let obj = call.arg(0, ctx)?.clone();
        let obj_span = obj.span;
        match obj.as_type().report_err(ctx)? {
            Type::Vector(None) => {
                call.check_args_len(2, ctx)?;
                let x = call.arg(1, ctx)?;
                let len = x
                    .as_integer() // TODO: make as_integer_vector_len()
                    .and_then(|n| data::check_vector_len(x.span, n))
                    .report_err(ctx)?;
                Ok(RtVal::Type(Type::Vector(Some(len))))
            }
            Type::CellArray(None) => {
                call.check_args_len(2, ctx)?;
                let x = call.arg(1, ctx)?;
                let ndim = ctx.get_ndim(x.span)?;
                let shape = x.as_vector_set(ndim).report_err(ctx)?;
                Ok(RtVal::Type(Type::CellArray(Some(shape))))
            }
            Type::VectorSet(None) => {
                call.check_args_len(2, ctx)?;
                let x = call.arg(1, ctx)?;
                let vec_len = x
                    .as_integer()
                    .and_then(|n| data::check_vector_set_vec_len(x.span, n))
                    .report_err(ctx)?;
                Ok(RtVal::Type(Type::VectorSet(Some(vec_len))))
            }
            Type::PatternMatcher(_) => Err(ctx.error(Error::unimplemented(call.span))),

            _ => Err(ctx.error(Error::custom(obj_span, "type is not dependent"))),
        }
    }
}
