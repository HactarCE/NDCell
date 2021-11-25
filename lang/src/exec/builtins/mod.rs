//! Built-in values and functions; the "standard library" of NDCA.

use codemap::Span;
use regex::Regex;

use ndcell_core::axis::Axis;

mod expressions;
pub mod functions;

use super::{Ctx, CtxTrait};
use crate::data::{self, LangInt, RtVal, Type};
use crate::errors::Result;
pub use expressions::Expression;
pub use functions::Function;

/// Returns a built-in function.
pub fn resolve_function(name: &str) -> Option<Box<dyn Function>> {
    lazy_static! {
        static ref VEC_FN_REGEX: Regex = Regex::new("^vec([1-9][0-9]*)$").unwrap();
        static ref VECSET_FN_REGEX: Regex = Regex::new("^vec([1-6])set$").unwrap();
    }

    // `vec1()` through `vec256()`
    if let Some(len) = VEC_FN_REGEX
        .captures(name)
        .and_then(|cap| cap.get(1)?.as_str().parse().ok())
        .filter(|&n| data::is_valid_vector_len(n))
    {
        return Some(functions::vectors::VecWithLen(len).boxed());
    }

    // `vec1set()` through `vec6set()`
    if let Some(len) = VECSET_FN_REGEX
        .captures(name)
        .and_then(|cap| cap.get(1)?.as_str().parse().ok())
    {
        return Some(functions::sets::VecSetWithLen(len).boxed());
    }

    Some(match name {
        "bool" => functions::bools::ToBool.boxed(),
        "vec" => functions::vectors::VecConstructor.boxed(),
        "vecset" => functions::sets::VecSetConstructor.boxed(),
        "intset" => functions::sets::EmptyIntegerSet.boxed(),
        "cellset" => functions::sets::EmptyIntegerSet.boxed(),

        // Shapes
        "square" => functions::sets::VectorSetShape::Square.boxed(),
        "diamond" => functions::sets::VectorSetShape::Diamond.boxed(),
        "moore" => functions::sets::VectorSetShape::Moore.boxed(),
        "vn" => functions::sets::VectorSetShape::Vn.boxed(),
        "circular" => functions::sets::VectorSetShape::Circular.boxed(),
        "l2" => functions::sets::VectorSetShape::L2.boxed(),
        "checkerboard" => functions::sets::VectorSetShape::Checkerboard.boxed(),
        "hash" => functions::sets::VectorSetShape::Hash.boxed(),
        "cross" => functions::sets::VectorSetShape::Cross.boxed(),
        "saltire" => functions::sets::VectorSetShape::Saltire.boxed(),
        "star" => functions::sets::VectorSetShape::Star.boxed(),

        _ => None?,
    })
}

/// Returns a built-in constant.
pub fn resolve_constant(name: &str, span: Span, ctx: &mut Ctx) -> Option<Result<RtVal>> {
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
        "Null" => Type::Null,
        "Integer" => Type::Integer,
        "Cell" => Type::Cell,
        "Tag" => Type::Tag,
        "String" => Type::String,
        "Type" => Type::Type,
        "Vector" => Type::Vector(None),
        "CellArray" => Type::CellArray(None),
        "CellArrayMut" => Type::CellArrayMut(None),
        "EmtpySet" => Type::EmptySet,
        "IntegerSet" => Type::IntegerSet,
        "CellSet" => Type::CellSet,
        "VectorSet" => Type::VectorSet(None),
        "PatternMatcher" => Type::PatternMatcher(None),
        "Regex" => Type::Regex,
        _ => None?,
    })
}

/// Returns a built-in method.
pub fn resolve_method(receiving_type: &Type, name: &str) -> Option<Box<dyn Function>> {
    match receiving_type {
        Type::Null => match name {
            _ => None,
        },
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
        Type::Type => None,

        Type::Vector(_) => match name {
            "x" => Some(functions::vectors::IndexVector(Some(Axis::X)).boxed()),
            "y" => Some(functions::vectors::IndexVector(Some(Axis::Y)).boxed()),
            "z" => Some(functions::vectors::IndexVector(Some(Axis::Z)).boxed()),
            "w" => Some(functions::vectors::IndexVector(Some(Axis::W)).boxed()),
            "u" => Some(functions::vectors::IndexVector(Some(Axis::U)).boxed()),
            "v" => Some(functions::vectors::IndexVector(Some(Axis::V)).boxed()),
            "len" => Some(functions::vectors::VectorLen.boxed()),
            _ => None,
        },
        Type::CellArray(_) | Type::CellArrayMut(_) => match name {
            "shape" => Some(functions::arrays::Shape.boxed()),
            "as_immut" => Some(functions::arrays::AsImmut.boxed()),
            // "as_mut" => Some(functions::arrays::AsMut.boxed()),
            _ => None,
        },

        Type::EmptySet => match name {
            _ => None,
        },
        Type::IntegerSet => match name {
            _ => None,
        },
        Type::CellSet => match name {
            _ => None,
        },
        Type::VectorSet(_) => match name {
            "fill" => Some(functions::arrays::FillVectorSet.boxed()),
            "new_buffer" => Some(functions::arrays::NewBuffer.boxed()),
            _ => None,
        },
        Type::PatternMatcher(_) => match name {
            _ => None,
        },
        Type::Regex => match name {
            _ => None,
        },
    }
}

fn resolve_index_method(obj_type: &Type) -> Option<Box<dyn Function>> {
    match obj_type {
        Type::Null => None,
        Type::Integer => None,
        Type::Cell => None,
        Type::Tag => None,
        Type::String => None,
        Type::Type => Some(Box::new(functions::types::TypeBrackets)),
        Type::Vector(_) => Some(Box::new(functions::vectors::IndexVector(None))),
        Type::CellArray(_) => Some(Box::new(functions::arrays::IndexCellArray)),
        Type::CellArrayMut(_) => Some(Box::new(functions::arrays::IndexCellArray)),
        Type::EmptySet => None,
        Type::IntegerSet => None,
        Type::CellSet => None,
        Type::VectorSet(_) => None,
        Type::PatternMatcher(_) => None,
        Type::Regex => None,
    }
}
