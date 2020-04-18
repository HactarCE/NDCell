use super::super::errors::*;
use super::super::types::{LangCellState, LangInt, Type};
use LangErrorMsg::InternalError;

const INTERNAL_VAR_USE_ERROR: LangError = InternalError(std::borrow::Cow::Borrowed(
    "Invalid variable use not caught by type checker",
))
.without_span();

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(LangInt),
    CellState(LangCellState),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            // Self::Pattern(_) => Type::Pattern,
        }
    }
    pub fn from_type(ty: Type) -> Option<Self> {
        match ty {
            Type::Int => Some(Value::Int(0)),
            Type::CellState => Some(Value::CellState(0)),
            // Type::Pattern => unimplemented!(),
        }
    }
}
impl Value {
    pub fn int(&self) -> LangResult<LangInt> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(INTERNAL_VAR_USE_ERROR),
        }
    }
    pub fn cell_state(&self) -> LangResult<LangCellState> {
        match self {
            Value::CellState(i) => Ok(*i),
            _ => Err(INTERNAL_VAR_USE_ERROR),
        }
    }
}
