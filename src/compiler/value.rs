use inkwell::values::IntValue;

use super::super::{errors::*, Spanned, Type};

pub const INT_BITS: u32 = 64;
pub const CELL_STATE_BITS: u32 = 64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'ctx> {
    Null,
    Int(IntValue<'ctx>),
    CellState(IntValue<'ctx>),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl<'ctx> Value<'ctx> {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Null => Type::Void,
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
        }
    }
}
impl<'ctx> Spanned<Value<'ctx>> {
    pub fn as_int(&self) -> LangResult<IntValue<'ctx>> {
        match self.inner {
            Value::Int(i) => Ok(i),
            _ => type_error(self, self.inner.get_type(), Type::Int),
        }
    }
    pub fn as_cell_state(&self) -> LangResult<IntValue<'ctx>> {
        match self.inner {
            Value::CellState(i) => Ok(i),
            _ => type_error(self, self.inner.get_type(), Type::CellState),
        }
    }
}
