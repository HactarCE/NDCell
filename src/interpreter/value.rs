use super::super::types::{LangCellState, LangInt, Type};
use super::super::{errors::*, Spanned};
use LangErrorMsg::TypeError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    Int(LangInt),
    CellState(LangCellState),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Null => Type::Void,
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            // Self::Pattern(_) => Type::Pattern,
        }
    }
    pub fn from_type(ty: Type) -> Option<Self> {
        match ty {
            Type::Void => None,
            Type::Int => Some(Value::Int(0)),
            Type::CellState => Some(Value::CellState(0)),
            // Type::Pattern => Value::Null,
        }
    }
}
impl Spanned<Value> {
    pub fn as_int(&self) -> LangResult<LangInt> {
        match self.inner {
            Value::Int(i) => Ok(i),
            _ => self.type_err(Type::Int),
        }
    }
    pub fn as_cell_state(&self) -> LangResult<LangCellState> {
        match self.inner {
            Value::CellState(i) => Ok(i),
            _ => self.type_err(Type::CellState),
        }
    }
    fn type_err<T>(&self, expected: Type) -> LangResult<T> {
        Err(TypeError {
            expected,
            got: self.inner.get_type(),
        }
        .with_span(self))
    }
}
