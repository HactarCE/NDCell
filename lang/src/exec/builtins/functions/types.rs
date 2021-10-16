use codemap::Spanned;

use super::{CallInfo, Function};
use crate::data::{self, RtVal, SpannedRuntimeValueExt, Type};
use crate::errors::{Error, Result};
use crate::exec::{Ctx, CtxTrait};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct TypeBrackets;
impl Function for TypeBrackets {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let obj = call.arg(0)?.clone();
        let obj_span = obj.span;
        match obj.as_type()? {
            Type::Vector(None) => {
                call.check_args_len(2)?;
                let x = call.arg(1)?;
                let len = x
                    .as_integer() // TODO: make as_integer_vector_len()
                    .and_then(|n| data::check_vector_len(x.span, n))?;
                Ok(RtVal::Type(Type::Vector(Some(len))))
            }
            Type::CellArray(None) => {
                call.check_args_len(2)?;
                let x = call.arg(1)?;
                let ndim = ctx.get_ndim(x.span)?;
                let shape = x.as_vector_set(ndim)?;
                Ok(RtVal::Type(Type::CellArray(Some(shape))))
            }
            Type::CellArrayMut(None) => {
                call.check_args_len(2)?;
                let x = call.arg(1)?;
                let ndim = ctx.get_ndim(x.span)?;
                let shape = x.as_vector_set(ndim)?;
                Ok(RtVal::Type(Type::CellArrayMut(Some(shape))))
            }
            Type::VectorSet(None) => {
                call.check_args_len(2)?;
                let x = call.arg(1)?;
                let vec_len = x
                    .as_integer()
                    .and_then(|n| data::check_vector_set_vec_len(x.span, n))?;
                Ok(RtVal::Type(Type::VectorSet(Some(vec_len))))
            }
            Type::PatternMatcher(_) => Err(Error::unimplemented(call.span)),

            _ => Err(Error::custom(obj_span, "type is not dependent")),
        }
    }
}
