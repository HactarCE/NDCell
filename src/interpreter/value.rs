use std::fmt;

use super::super::{errors::*, Span, Spanned};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    CellState,
    // Pattern,
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Void => "void",
                Self::Int => "integer",
                Self::CellState => "cell state",
                // Self::Pattern => "pattern",
            }
        )
    }
}
impl Type {
    pub fn default<'a>(self) -> Value {
        match self {
            Self::Void => Value::Null,
            Self::Int => Value::Int(0),
            Self::CellState => Value::CellState(0),
            // Self::Pattern => Value::Null,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    Int(i64),
    CellState(i64),
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
}
impl Spanned<Value> {
    pub fn as_int(&self) -> LangResult<i64> {
        match self.inner {
            Value::Int(i) => Ok(i),
            _ => type_error(self, self.inner.get_type(), Type::Int),
        }
    }
    pub fn as_cell_state(&self) -> LangResult<i64> {
        match self.inner {
            Value::CellState(i) => Ok(i),
            _ => type_error(self, self.inner.get_type(), Type::CellState),
        }
    }
}

fn type_error<T>(spanned: impl Into<Span>, got_type: Type, expected_type: Type) -> LangResult<T> {
    lang_err(
        spanned,
        format!(
            "Type error: expected {} but got {}",
            expected_type, got_type
        ),
    )
}
