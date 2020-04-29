//! Values used by the JIT compiler for NDCA.

use inkwell::values::{BasicValueEnum, IntValue, VectorValue};

use super::super::types::Type;
use super::super::{errors::*, Spanned};
use LangErrorMsg::TypeError;

/// A value of any type.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value<'ctx> {
    /// Integer.
    Int(IntValue<'ctx>),
    /// Cell state.
    CellState(IntValue<'ctx>),
    /// Vector of a specific length (from 1 to 6).
    Vector(VectorValue<'ctx>),
}
impl<'ctx> Value<'ctx> {
    /// Returns the type of this value.
    pub fn ty(self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(v) => Type::Vector(v.get_type().get_size() as u8),
        }
    }
    /// Constructs a value of the given type from an LLVM basic value.
    pub fn from_basic_value(ty: Type, basic_value: BasicValueEnum<'ctx>) -> Self {
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
}
impl<'ctx> Spanned<Value<'ctx>> {
    /// Returns the LLVM integer value inside if this is Value::Int; otherwise a
    /// TypeError.
    pub fn as_int(self) -> LangResult<IntValue<'ctx>> {
        match self.inner {
            Value::Int(i) => Ok(i),
            _ => self.type_err(Type::Int),
        }
    }
    /// Returns the LLVM integer value inside if this is Value::CellState;
    /// otherwise a TypeError.
    pub fn as_cell_state(self) -> LangResult<IntValue<'ctx>> {
        match self.inner {
            Value::CellState(i) => Ok(i),
            _ => self.type_err(Type::CellState),
        }
    }
    /// Returns the LLVM vector value inside if this is Value::Vector; otherwise
    /// a TypeError.
    pub fn as_vector(self) -> LangResult<VectorValue<'ctx>> {
        match self.inner {
            Value::Vector(v) => Ok(v),
            _ => self.type_err(Type::CellState),
        }
    }
    /// Returns this value as an LLVM basic value if it is representable;
    /// otherwise a TypeError.
    pub fn into_basic_value(self) -> LangResult<BasicValueEnum<'ctx>> {
        match self.inner {
            Value::Int(i) => Ok(i.into()),
            Value::CellState(i) => Ok(i.into()),
            Value::Vector(v) => Ok(v.into()),
            // Value::Pattern => Err(CannotAssignTypeToVariable(self.inner.ty()).with_span(self)),
        }
    }
    /// Returns a TypeError relating to this value.
    fn type_err<T>(self, expected: Type) -> LangResult<T> {
        Err(TypeError {
            expected,
            got: self.inner.ty(),
        }
        .with_span(self))
    }
}
