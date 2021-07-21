use codemap::Spanned;
use std::fmt;

use super::{CallInfo, Function};
use crate::data::{check_vector_len, RtVal, SpannedRuntimeValueExt, Type};
use crate::errors::{Error, Fallible};
use crate::exec::{Ctx, CtxTrait};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct TypeBrackets;
impl fmt::Display for TypeBrackets {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type indexing")
    }
}
impl Function for TypeBrackets {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let obj = call.arg(0, ctx)?.clone();
        let obj_span = obj.span;
        match obj.as_type().map_err(|e| ctx.error(e))? {
            Type::Vector(None) => {
                call.check_args_len(2, ctx, self)?;
                let x = call.arg(1, ctx)?;
                let len = x
                    .clone()
                    .as_integer() // TODO: make as_integer_vector_len()
                    .and_then(|n| check_vector_len(x.span, n))
                    .map_err(|e| ctx.error(e))?;
                Ok(RtVal::Type(Type::Vector(Some(len))))
            }
            Type::Array(_) => Err(ctx.error(Error::unimplemented(call.span))),
            Type::VectorSet(_) => Err(ctx.error(Error::unimplemented(call.span))),
            Type::Pattern(_) => Err(ctx.error(Error::unimplemented(call.span))),

            _ => Err(ctx.error(Error::custom(obj_span, "type is not dependent"))),
        }
    }
}