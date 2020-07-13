//! Values used by the JIT compiler.

use inkwell::values::{BasicValueEnum, IntValue, StructValue, VectorValue};
use std::rc::Rc;

use crate::errors::*;
use crate::types::PatternShape;
use crate::Type;

/// A value of any type.
#[derive(Debug, Clone)]
pub enum Value {
    /// Void.
    Void,
    /// Integer.
    Int(IntValue<'static>),
    /// Cell state.
    CellState(IntValue<'static>),
    /// Vector of a specific length from 1 to 256.
    Vector(VectorValue<'static>),
    /// Pattern of cells with an arbitrary shape.
    Pattern(PatternValue),
    /// Inclusive integer range, represented by an LLVM vector value with three
    /// integers: start, end, and step.
    IntRange(VectorValue<'static>),
    /// Inclusive hyperrectangle, represented by a struct containing two vectors
    /// of the same length.
    Rectangle(StructValue<'static>),
    /// Cell state filter for an automaton with a specific number of cell
    /// states, represented by raw bits stored in an LLVM vector value of
    /// integers.
    CellStateFilter(usize, VectorValue<'static>),
}
impl Value {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Void => Type::Void,
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(v) => Type::Vector(v.get_type().get_size() as usize),
            Self::Pattern(p) => Type::Pattern(p.shape.clone()),
            Self::IntRange(_) => Type::IntRange,
            Self::Rectangle(r) => Type::Rectangle({
                let field1 = r.get_type().get_field_type_at_index(0).unwrap();
                let field2 = r.get_type().get_field_type_at_index(1).unwrap();
                assert_eq!(field1, field2, "LLVM rect fields mismatch");
                field1.into_vector_type().get_size() as usize
            }),
            Self::CellStateFilter(state_count, _) => Type::CellStateFilter(*state_count),
        }
    }
    /// Constructs a value of the given type from an LLVM basic value.
    pub fn from_basic_value(ty: &Type, basic_value: BasicValueEnum<'static>) -> Self {
        match ty {
            Type::Void => Self::Void,
            Type::Int => Self::Int(basic_value.into_int_value()),
            Type::CellState => Self::CellState(basic_value.into_int_value()),
            Type::Vector(len) => {
                let ret = Self::Vector(basic_value.into_vector_value());
                assert_eq!(Type::Vector(*len), ret.ty(), "LLVM vector length mismatch");
                ret
            }
            Type::Pattern(shape) => Self::Pattern(PatternValue {
                value: basic_value.into_struct_value(),
                shape: shape.clone(),
            }),
            Type::IntRange => Self::IntRange(basic_value.into_vector_value()),
            Type::Rectangle(ndim) => {
                let ret = Self::Rectangle(basic_value.into_struct_value());
                assert_eq!(Type::Rectangle(*ndim), ret.ty(), "LLVM rect ndim mismatch");
                ret
            }
            Type::CellStateFilter(state_count) => {
                assert_eq!(
                    basic_value.into_vector_value().get_type(),
                    super::types::cell_state_filter(*state_count),
                    "LLVM cell state filter vector length mismatch",
                );
                Value::CellStateFilter(*state_count, basic_value.into_vector_value())
            }
            Type::Stencil => panic!(NO_RUNTIME_REPRESENTATION),
        }
    }
    /// Returns the LLVM integer value inside if this is Value::Int; otherwise
    /// an InternalError.
    pub fn as_int(self) -> LangResult<IntValue<'static>> {
        match self {
            Value::Int(i) => Ok(i),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the LLVM integer value inside if this is Value::CellState;
    /// otherwise an InternalError.
    pub fn as_cell_state(self) -> LangResult<IntValue<'static>> {
        match self {
            Value::CellState(i) => Ok(i),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the LLVM vector value inside if this is Value::Vector; otherwise
    /// an InternalError.
    pub fn as_vector(self) -> LangResult<VectorValue<'static>> {
        match self {
            Value::Vector(v) => Ok(v),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the PatternValue inside if this is Value::Pattern; otherwise an
    /// InternalError.
    pub fn as_pattern(self) -> LangResult<PatternValue> {
        match self {
            Value::Pattern(p) => Ok(p),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the LLVM vector value inside if this is Value::IntRange;
    /// otherwise an InternalError.
    pub fn as_int_range(self) -> LangResult<VectorValue<'static>> {
        match self {
            Value::IntRange(r) => Ok(r),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the LLVM struct value inside if this is Value::Rectangle;
    /// otherwise an InternalError.
    pub fn as_rectangle(self) -> LangResult<StructValue<'static>> {
        match self {
            Value::Rectangle(r) => Ok(r),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the LLVM vector value inside if this is Value::CellStateFilter;
    /// otherwise an InternalError.
    pub fn as_cell_state_filter(self) -> LangResult<VectorValue<'static>> {
        match self {
            Value::CellStateFilter(_, f) => Ok(f),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns this value as an LLVM basic value if it is representable as one;
    /// otherwise a TypeError.
    pub fn into_basic_value(self) -> LangResult<BasicValueEnum<'static>> {
        match self {
            Value::Void => Ok(super::types::void().get_undef().into()),
            Value::Int(i) => Ok(i.into()),
            Value::CellState(i) => Ok(i.into()),
            Value::Vector(v) => Ok(v.into()),
            Value::Pattern(p) => Ok(p.value.into()),
            Value::IntRange(r) => Ok(r.into()),
            Value::Rectangle(r) => Ok(r.into()),
            Value::CellStateFilter(_, f) => Ok(f.into()),
            // _ => internal_error!("{} has no BasicValue representation", self),
        }
    }
}

/// Pattern of cells with an arbitrary shape.
#[derive(Debug, Clone)]
pub struct PatternValue {
    /// Struct containing two values:
    /// 1. Pointer to the origin (0 along all axes) in an array of cell states.
    /// 2. Pointer offset to increment the given axis by 1. (Length of this
    ///    array is the number of dimensions in the pattern.)
    pub value: StructValue<'static>,
    /// The shape of the pattern, which includes the pattern bounds and mask.
    pub shape: Rc<PatternShape>,
}
