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
use crate::lexer::OperatorToken;
use crate::{Span, Type};
use LangErrorMsg::InvalidArguments;

/// FnOnce that constructs a Box<dyn ast::Function>, given some standard
/// arguments.
pub type FuncConstructor =
    Box<dyn FnOnce(&mut ast::UserFunction, Span, ast::ArgTypes) -> FuncResult>;
/// Shorthand for LangResult<Box<dyn ast::Function>>, which is returned from a
/// FuncConstructor.
pub type FuncResult = LangResult<Box<dyn ast::Function>>;

/// Returns a constructor for the function with the given name.
pub fn lookup_function(name: &str) -> Option<FuncConstructor> {
    match name {
        "abs" => Some(math::NegOrAbs::with_mode(math::NegOrAbsMode::AbsFunc)),
        "bool" => Some(Box::new(convert::ToBool::construct)),
        _ => None,
    }
}

/// Returns a constructor for the method on the given type with the given name.
pub fn lookup_method(ty: Type, name: &str) -> Option<FuncConstructor> {
    match ty {
        Type::Int => match name {
            "abs" => Some(math::NegOrAbs::with_mode(math::NegOrAbsMode::AbsMethod)),
            _ => None,
        },
        Type::CellState => todo!("methods on cell states"),
        Type::Vector(_) => match name {
            // Reductions
            "max" => Some(vectors::MinMax::max()),
            "min" => Some(vectors::MinMax::min()),
            "sum" => Some(vectors::Reduce::sum()),
            "product" => Some(vectors::Reduce::product()),
            // Component access
            "x" => Some(vectors::Access::with_component_idx(Some(0))),
            "y" => Some(vectors::Access::with_component_idx(Some(1))),
            "z" => Some(vectors::Access::with_component_idx(Some(2))),
            "w" => Some(vectors::Access::with_component_idx(Some(3))),
            "u" => Some(vectors::Access::with_component_idx(Some(4))),
            "v" => Some(vectors::Access::with_component_idx(Some(5))),
            // Miscellaneous
            "len" => Some(Box::new(vectors::GetLen::construct)),
            _ => None,
        },
    }
}

pub fn lookup_unary_operator(
    op: OperatorToken,
    arg: Type,
    span: Span,
) -> LangResult<FuncConstructor> {
    use OperatorToken::*;
    use Type::*;
    let ret: Option<FuncConstructor> = match op {
        // Unary plus
        Plus => match arg {
            Int | Vector(_) => Some(Box::new(math::UnaryPlus::construct)),
            _ => None,
        },
        // Unary minus
        Minus => match arg {
            Int | Vector(_) => Some(math::NegOrAbs::with_mode(math::NegOrAbsMode::Negate)),
            _ => None,
        },
        // Tag
        Tag => match arg {
            Int => Some(Box::new(convert::IntToCellState::construct)),
            _ => None,
        },
        _ => None,
    };
    ret.ok_or_else(|| {
        InvalidArguments {
            name: format!("unary {:?} operator", op.to_string()),
            is_method: false,
            arg_types: vec![arg],
        }
        .with_span(span)
    })
}

pub fn lookup_binary_operator(
    lhs: Type,
    op: OperatorToken,
    rhs: Type,
    span: Span,
) -> LangResult<FuncConstructor> {
    use OperatorToken::*;
    use Type::*;
    let ret: Option<FuncConstructor> = match (lhs, op, rhs) {
        // Range
        (_, DotDot, _) => match (lhs, rhs) {
            (Int, Int) => todo!("Integer range"),
            _ => None,
        },
        // Basic math
        (Int, _, Int) | (Int, _, Vector(_)) | (Vector(_), _, Int) | (Vector(_), _, Vector(_)) => {
            Some(math::BinaryOp::with_op(op))
        }
        _ => None,
    };
    ret.ok_or_else(|| {
        InvalidArguments {
            name: format!("binary {:?} operator", op.to_string()),
            is_method: false,
            arg_types: vec![lhs, rhs],
        }
        .with_span(span)
    })
}
