//! Values used by the interpreter for NDCA.

use super::super::errors::*;
use super::super::types::{LangCellState, LangInt, Type};
use LangErrorMsg::InternalError;

/// InternalError reported when a variable is used improperly and it was not
/// caught by the type checker.
const INTERNAL_VAR_USE_ERROR: LangError = InternalError(std::borrow::Cow::Borrowed(
    "Invalid variable use not caught by type checker",
))
.without_span();

/// A value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// Integer
    Int(LangInt),
    /// Cell state
    CellState(LangCellState),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl Value {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            // Self::Pattern(_) => Type::Pattern,
        }
    }
    /// Constructs a default value of the given type.
    pub fn from_type(ty: Type) -> Option<Self> {
        match ty {
            Type::Int => Some(Value::Int(0)),
            Type::CellState => Some(Value::CellState(0)),
            // Type::Pattern => unimplemented!(),
        }
    }
}
impl Value {
    /// Returns the integer value inside if this is a Value::Int; otherwise a
    /// TypeError.
    pub fn as_int(&self) -> LangResult<LangInt> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(INTERNAL_VAR_USE_ERROR),
        }
    }
    /// Returns the integer value inside if this is a Value::CellState;
    /// otherwise a TypeError.
    pub fn as_cell_state(&self) -> LangResult<LangCellState> {
        match self {
            Value::CellState(i) => Ok(*i),
            _ => Err(INTERNAL_VAR_USE_ERROR),
        }
    }
}
