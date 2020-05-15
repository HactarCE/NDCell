//! Values used by the interpreter for NDCA.

use super::errors::*;
use super::types::{LangCellState, LangInt, Type};
use LangErrorMsg::InternalError;

/// InternalError reported when a variable is used improperly and it was not
/// caught by the type checker.
const INTERNAL_VAR_USE_ERROR: LangError = InternalError(std::borrow::Cow::Borrowed(
    "Invalid variable use not caught by type checker",
))
.without_span();

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    /// Integer
    Int(LangInt),
    /// Cell state
    CellState(LangCellState),
    /// Vector of a specific length from 1 to 256 (extra components are zero).
    Vector(Vec<LangInt>),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl ConstValue {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(values) => Type::Vector(values.len() as u16),
            // Self::Pattern(_) => Type::Pattern,
        }
    }
    /// Constructs a default value of the given type.
    pub fn from_type(ty: Type) -> Option<Self> {
        match ty {
            Type::Int => Some(ConstValue::Int(0)),
            Type::CellState => Some(ConstValue::CellState(0)),
            Type::Vector(len) => Some(ConstValue::Vector(vec![0; len as usize])),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::Int; otherwise a
    /// TypeError.
    pub fn as_int(self) -> LangResult<LangInt> {
        match self {
            ConstValue::Int(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::CellState;
    /// otherwise a TypeError.
    pub fn as_cell_state(self) -> LangResult<LangCellState> {
        match self {
            ConstValue::CellState(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
}
