use super::super::{errors::*, Span, Spanned, Type};

type IntValue = i64;
type CellStateValue = i64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    Int(IntValue),
    CellState(CellStateValue),
    // Pattern(crate::automaton::ArrayView2D<u8>),
}
impl From<Type> for Value {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Void => Value::Null,
            Type::Int => Value::Int(0),
            Type::CellState => Value::CellState(0),
            // Type::Pattern => Value::Null,
        }
    }
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
    pub fn as_int(&self) -> LangResult<IntValue> {
        match self.inner {
            Value::Int(i) => Ok(i),
            _ => type_error(self, self.inner.get_type(), Type::Int),
        }
    }
    pub fn as_cell_state(&self) -> LangResult<CellStateValue> {
        match self.inner {
            Value::CellState(i) => Ok(i),
            _ => type_error(self, self.inner.get_type(), Type::CellState),
        }
    }
}

fn type_error<T>(spanned: impl Into<Span>, got_type: Type, expected_type: Type) -> LangResult<T> {
    spanned_lang_err(
        spanned,
        format!(
            "Type error: expected {} but got {}",
            expected_type, got_type
        ),
    )
}
