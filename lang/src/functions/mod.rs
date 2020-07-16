//! Built-in functions, methods/properties, and operators.

use regex::Regex;

pub mod cmp;
pub mod convert;
pub mod enums;
pub mod literals;
pub mod logic;
pub mod math;
pub mod misc;
pub mod patterns;
pub mod ranges;
pub mod vectors;

use crate::ast;
use crate::errors::*;
use crate::lexer::{OperatorToken, TypeToken};
use crate::types::{LangInt, AXES, MAX_VECTOR_LEN};
use crate::Type;

lazy_static! {
    static ref VEC_FN_PATTERN: Regex = Regex::new(r#"vec(?:tor)?([1-9]\d*)"#).unwrap();
    static ref RECT_FN_PATTERN: Regex = Regex::new(r#"rect(?:angle)?([1-9]\d*)"#).unwrap();
}

/// FnOnce that constructs a Box<dyn ast::Function>, given some standard
/// arguments.
pub type FuncConstructor = Box<dyn FnOnce(&mut ast::FuncCallInfoMut) -> FuncResult>;
/// Shorthand for LangResult<Box<dyn ast::Function>>, which is returned from a
/// FuncConstructor.
pub type FuncResult = LangResult<Box<dyn ast::Function>>;

/// Returns a constructor for the function with the given name, if one exists.
pub fn lookup_function(name: &str) -> Option<FuncConstructor> {
    // vecN()
    if let Some(captures) = VEC_FN_PATTERN.captures(name) {
        match captures.get(1).unwrap().as_str().parse() {
            Ok(len @ 1..=MAX_VECTOR_LEN) => return Some(convert::ToVector::with_len(Some(len))),
            _ => (),
        }
    }
    // rectN()
    if let Some(captures) = RECT_FN_PATTERN.captures(name) {
        match captures.get(1).unwrap().as_str().parse() {
            Ok(ndim @ 1..=MAX_VECTOR_LEN) => {
                return Some(convert::ToRectangle::with_ndim(Some(ndim)))
            }
            _ => (),
        }
    }
    // Other functions
    match name {
        "abs" => Some(math::NegOrAbs::with_mode(math::NegOrAbsMode::AbsFunc)),
        "bool" => Some(Box::new(convert::ToBool::construct)),
        "max" => Some(math::MinMax::max()),
        "min" => Some(math::MinMax::min()),
        "rect" => Some(convert::ToRectangle::with_ndim(None)),
        "vec" => Some(convert::ToVector::with_len(None)),
        _ => None,
    }
}

/// Returns a constructor for the method on the given type with the given name,
/// if one exists.
pub fn lookup_method(ty: Type, name: &str) -> Option<FuncConstructor> {
    if ty.has_runtime_representation() {
        if name == "new" {
            return Some(misc::New::with_type(ty));
        }
    }
    match ty {
        Type::Void => None,
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
            "x" | "y" | "z" | "w" | "u" | "v" => Some(vectors::Access::with_component_idx(Some(
                AXES.find(&name).unwrap() as LangInt,
            ))),
            // Miscellaneous
            "len" => Some(Box::new(vectors::GetLen::construct)),
            _ => None,
        },
        Type::Pattern { .. } => match name {
            "count" => todo!("count cells in pattern"),
            "find" => todo!("find cell in pattern"),
            "outer" => todo!("exclude center cell from pattern"),
            "shape" => todo!("get pattern mask"),
            "size" => todo!("get pattern size"),
            "min" => todo!("get pattern lower corner"),
            "max" => todo!("get pattern upper corner"),
            _ => None,
        },
        Type::IntRange => match name {
            "by" => Some(Box::new(ranges::StepBy::construct)),
            "start" => Some(ranges::Access::with_prop(ranges::RangeProperty::Start)),
            "end" => Some(ranges::Access::with_prop(ranges::RangeProperty::End)),
            "step" => Some(ranges::Access::with_prop(ranges::RangeProperty::Step)),
            _ => None,
        },
        Type::Rectangle(_) => match name {
            "start" => Some(ranges::Access::with_prop(ranges::RangeProperty::Start)),
            "end" => Some(ranges::Access::with_prop(ranges::RangeProperty::End)),
            // Component access
            "x" | "y" | "z" | "w" | "u" | "v" => Some(vectors::Access::with_component_idx(Some(
                AXES.find(&name).unwrap() as LangInt,
            ))),
            // Miscellaneous
            "ndim" => Some(Box::new(vectors::GetLen::construct)),
            _ => None,
        },
        Type::CellStateFilter(_) => match name {
            "any" => todo!("return true if any cell state matches"),
            "count" => todo!("count cell states matching"),
            "first" => todo!("first cell state matching"),
            "last" => todo!("last cell state matching"),
            _ => None,
        },
        Type::Stencil => None,
    }
}

/// Returns a constructor for the function with the same name as the given type.
pub fn lookup_type_function(ty: TypeToken) -> Option<FuncConstructor> {
    match ty {
        TypeToken::Vector(maybe_len) => Some(convert::ToVector::with_len(maybe_len)),
        _ => None,
    }
}

/// Returns a constructor for the function that applies the given unary operator
/// to the given type, or returns an appropriate error if the operator cannot be
/// applied to the given type.
pub fn lookup_unary_operator(op: OperatorToken) -> LangResult<FuncConstructor> {
    use OperatorToken::*;
    match op {
        Plus => Ok(Box::new(math::UnaryPlus::construct)),
        Minus => Ok(math::NegOrAbs::with_mode(math::NegOrAbsMode::Negate)),
        Tilde => Ok(Box::new(math::BitwiseNot::construct)),
        Tag => Ok(Box::new(convert::IntToCellState::construct)),
        _ => internal_error!("Invalid unary operator {:?}", op.to_string()),
    }
}

/// Returns a constructor for the function that applies the given binary
/// operator to the given types, or returns an appropriate error if the operator
/// cannot be applied to the given types.
pub fn lookup_binary_operator(op: OperatorToken) -> LangResult<FuncConstructor> {
    use OperatorToken::*;
    match op {
        Plus | Minus | Asterisk | Slash | Percent | DoubleAsterisk | DoubleLessThan
        | DoubleGreaterThan | TripleGreaterThan | Ampersand | Pipe | Caret | Tilde => {
            Ok(math::BinaryOp::with_op(op))
        }
        DotDot => Ok(Box::new(literals::Range::construct)),
        _ => internal_error!("Invalid binary operator {:?}", op.to_string()),
    }
}
