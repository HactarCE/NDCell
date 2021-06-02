//! Built-in values and functions; the "standard library" of NDCA.

use codemap::Span;

mod expressions;
pub mod functions;

use super::{Ctx, CtxTrait};
use crate::data::{LangInt, RtVal, Type};
use crate::errors::Fallible;
pub use expressions::Expression;
pub use functions::Function;

/// Returns a built-in function.
pub fn resolve_function(name: &str) -> Option<Box<dyn Function>> {
    Some(match name {
        "bool" => Box::new(functions::convert::ToBool),
        _ => None?,
    })
}

/// Returns a built-in constant.
pub fn resolve_constant(name: &str, span: Span, ctx: &mut Ctx) -> Option<Fallible<RtVal>> {
    if let Some(ty) = resolve_type_keyword(name) {
        Some(Ok(RtVal::Type(ty)))
    } else {
        Some(match name {
            "TRUE" => Ok(RtVal::Integer(1)),
            "FALSE" => Ok(RtVal::Integer(0)),

            "NDIM" => ctx
                .get_ndim(span)
                .map(|ndim| ndim as LangInt)
                .map(RtVal::Integer),
            "STATECOUNT" => ctx
                .get_states(span)
                .map(|statecount| statecount as LangInt)
                .map(RtVal::Integer),
            _ => None?,
        })
    }
}
fn resolve_type_keyword(name: &str) -> Option<Type> {
    Some(match name {
        "Integer" => Type::Integer,
        "Cell" => Type::Cell,
        "Tag" => Type::Tag,
        "String" => Type::String,
        "Type" => Type::Type,
        "Null" => Type::Null,
        "Vector" => Type::Vector(None),
        "Array" => Type::Array(None),
        "IntegerSet" => Type::IntegerSet,
        "CellSet" => Type::CellSet,
        "VectorSet" => Type::VectorSet(None),
        "Pattern" => Type::Pattern(None),
        "Regex" => Type::Regex,
        _ => None?,
    })
}

/// Returns a built-in method.
pub fn resolve_method(receiving_type: &Type, name: &str) -> Option<Box<dyn Function>> {
    match receiving_type {
        Type::Integer => match name {
            _ => None,
        },
        Type::Cell => match name {
            _ => None,
        },
        Type::Tag => match name {
            _ => None,
        },
        Type::String => match name {
            _ => None,
        },
        Type::Type => match name {
            _ => None,
        },
        Type::Null => match name {
            _ => None,
        },

        Type::Vector(_) => match name {
            _ => None,
        },
        Type::Array(_) => match name {
            _ => None,
        },

        Type::IntegerSet => match name {
            _ => None,
        },
        Type::CellSet => match name {
            _ => None,
        },
        Type::VectorSet(_) => match name {
            _ => None,
        },
        Type::Pattern(_) => match name {
            _ => None,
        },
        Type::Regex => match name {
            _ => None,
        },
    }
}

fn resolve_index_method(obj_type: &Type) -> Option<Box<dyn Function>> {
    match obj_type {
        Type::Integer => None,
        Type::Cell => None,
        Type::Tag => None,
        Type::String => None,
        Type::Type => None,
        Type::Null => None,
        Type::Vector(_) => None,
        Type::Array(_) => None,
        Type::IntegerSet => None,
        Type::CellSet => None,
        Type::VectorSet(_) => None,
        Type::Pattern(_) => None,
        Type::Regex => None,
    }
}
