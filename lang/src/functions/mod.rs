//! Built-in functions, methods/properties, and operators.

pub mod cmp;
pub mod convert;
pub mod literals;
pub mod logic;
pub mod math;
pub mod misc;
pub mod vectors;

use crate::ast;
use crate::errors::*;
use crate::{Span, Type};

/// FnOnce that constructs a Box<dyn ast::Function>, given some standard
/// arguments.
pub type FuncConstructor =
    Box<dyn FnOnce(&mut ast::UserFunction, Span, ast::ArgTypes) -> FuncResult>;
/// Shorthand for LangResult<Box<dyn ast::Function>>, which is returned from a
/// FuncConstructor.
pub type FuncResult = LangResult<Box<dyn ast::Function>>;

pub fn lookup_function(name: &str) -> Option<FuncConstructor> {
    match name {
        "abs" => Some(math::NegOrAbs::with_mode(math::NegOrAbsMode::AbsFunc)),
        "bool" => Some(Box::new(convert::ToBool::construct)),
        _ => None,
    }
}

pub fn lookup_method(ty: Type, name: &str) -> Option<FuncConstructor> {
    match ty {
        Type::Int => match name {
            "abs" => Some(math::NegOrAbs::with_mode(math::NegOrAbsMode::AbsMethod)),
            _ => None,
        },
        Type::CellState => todo!("methods on cell states"),
        Type::Vector(_) => todo!("methods on vectors"),
    }
}
