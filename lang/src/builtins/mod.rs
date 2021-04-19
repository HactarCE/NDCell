//! Built-in values and functions; the "standard library" of NDCA.

use codemap::{Span, Spanned};
use itertools::Itertools;
use std::fmt;
use std::sync::Arc;

mod expressions;
pub mod functions;

use crate::ast;
use crate::compiler::Compiler;
use crate::data::{CpVal, FallibleTypeOf, RtVal, Type, Val};
use crate::errors::{AlreadyReported, Error, Fallible};
use crate::runtime::Runtime;
pub use expressions::Expression;
pub use functions::Function;

/// Returns the built-in function with the given name.
pub fn resolve_function(name: &str) -> Option<Box<dyn 'static + Function>> {
    match name {
        _ => None,
    }
}

/// Returns the built-in constant with the given name.
pub fn resolve_constant(name: &str) -> Option<Val> {
    match name {
        // Type keywords
        "Integer" => Some(RtVal::Type(Type::Integer).into()),
        "Cell" => Some(RtVal::Type(Type::Cell).into()),
        "Tag" => Some(RtVal::Type(Type::Tag).into()),
        "String" => Some(RtVal::Type(Type::String).into()),
        "Type" => Some(RtVal::Type(Type::Type).into()),
        "Null" => Some(RtVal::Type(Type::Null).into()),
        "Vector" => Some(RtVal::Type(Type::Vector(None)).into()),
        "Array" => Some(RtVal::Type(Type::Array(None)).into()),
        "IntegerSet" => Some(RtVal::Type(Type::IntegerSet).into()),
        "CellSet" => Some(RtVal::Type(Type::CellSet).into()),
        "VectorSet" => Some(RtVal::Type(Type::VectorSet(None)).into()),
        "Pattern" => Some(RtVal::Type(Type::Pattern(None)).into()),
        "Regex" => Some(RtVal::Type(Type::Regex).into()),

        _ => None,
    }
}
