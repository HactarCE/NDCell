use inkwell::values::{BasicValueEnum, IntValue};

use super::super::types::Type;
use super::super::{errors::*, Spanned};
use LangErrorMsg::{CannotAssignTypeToVariable, TypeError};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value<'ctx> {
    Int(IntValue<'ctx>),
    CellState(IntValue<'ctx>),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl<'ctx> Value<'ctx> {
    pub fn get_type(self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
        }
    }
    pub fn from_basic_value(ty: Type, basic_value: BasicValueEnum<'ctx>) -> Self {
        match ty {
            Type::Int => Self::Int(basic_value.into_int_value()),
            Type::CellState => Self::CellState(basic_value.into_int_value()),
            // Type::Pattern => panic!("Cannot construct type {} from basic value", ty),
        }
    }
}
impl<'ctx> Spanned<Value<'ctx>> {
    pub fn as_int(self) -> LangResult<IntValue<'ctx>> {
        match self.inner {
            Value::Int(i) => Ok(i),
            _ => self.type_err(Type::Int),
        }
    }
    pub fn as_cell_state(self) -> LangResult<IntValue<'ctx>> {
        match self.inner {
            Value::CellState(i) => Ok(i),
            _ => self.type_err(Type::CellState),
        }
    }
    pub fn as_basic_value(self) -> LangResult<BasicValueEnum<'ctx>> {
        match self.inner {
            Value::Int(i) => Ok(i.into()),
            Value::CellState(i) => Ok(i.into()),
            // Value::Pattern => Err(CannotAssignTypeToVariable(self.inner.get_type()).with_span(self)),
        }
    }
    fn type_err<T>(self, expected: Type) -> LangResult<T> {
        Err(TypeError {
            expected,
            got: self.inner.get_type(),
        }
        .with_span(self))
    }
}
