//! Values used by the JIT compiler for NDCA.

use inkwell::values::{BasicValueEnum, IntValue, VectorValue};

use super::super::errors::*;
use super::super::Type;

/// A value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// Integer.
    Int(IntValue<'static>),
    /// Cell state.
    CellState(IntValue<'static>),
    /// Vector of a specific length (from 1 to 6).
    Vector(VectorValue<'static>),
}
impl Value {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(v) => Type::Vector(v.get_type().get_size() as u16),
        }
    }
    /// Constructs a value of the given type from an LLVM basic value.
    pub fn from_basic_value(ty: Type, basic_value: BasicValueEnum<'static>) -> Self {
        match ty {
            Type::Int => Self::Int(basic_value.into_int_value()),
            Type::CellState => Self::CellState(basic_value.into_int_value()),
            Type::Vector(len) => {
                let ret = Self::Vector(basic_value.into_vector_value());
                assert_eq!(Type::Vector(len), ret.ty(), "Vector length does not match");
                ret
            }
        }
    }
    /// Returns the LLVM integer value inside if this is Value::Int; otherwise a
    /// TypeError.
    pub fn as_int(&self) -> LangResult<IntValue<'static>> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the LLVM integer value inside if this is Value::CellState;
    /// otherwise a TypeError.
    pub fn as_cell_state(&self) -> LangResult<IntValue<'static>> {
        match self {
            Value::CellState(i) => Ok(*i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the LLVM vector value inside if this is Value::Vector; otherwise
    /// a TypeError.
    pub fn as_vector(&self) -> LangResult<VectorValue<'static>> {
        match self {
            Value::Vector(v) => Ok(*v),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns this value as an LLVM basic value if it is representable as one;
    /// otherwise a TypeError.
    pub fn into_basic_value(&self) -> LangResult<BasicValueEnum<'static>> {
        match self {
            Value::Int(i) => Ok((*i).into()),
            Value::CellState(i) => Ok((*i).into()),
            Value::Vector(v) => Ok((*v).into()),
            // Value::Pattern => Err(InternalError(format!("{} has no BasicValue representation", self).into())),
        }
    }
    // TODO: figure out what to do with this
    // pub fn coerce(self, compiler: &mut Compiler, span: Span, ty: Type) -> Option<Self> {
    //     // TODO implement type coercion
    //     if self.ty() == ty {
    //         Some(self)
    //     } else {
    //         None
    //     }

    //     // match self {
    //     //     Value::Int(i) => match ty {
    //     //         Type::Int => Value::Int(i),
    //     //         Type::Vector(v) =>
    //     //     }
    //     // }
    // }
}
