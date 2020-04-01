use inkwell::values::IntValue;

use super::super::types::Type;
use super::super::{errors::*, Spanned};
use LangErrorMsg::TypeError;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value<'ctx> {
    Null,
    Int(IntValue<'ctx>),
    CellState(IntValue<'ctx>),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl<'ctx> Value<'ctx> {
    pub fn get_type(self) -> Type {
        match self {
            Self::Null => Type::Void,
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
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
    fn type_err<T>(self, expected: Type) -> LangResult<T> {
        Err(TypeError {
            expected,
            got: self.inner.get_type(),
        }
        .with_span(self))
    }
}
