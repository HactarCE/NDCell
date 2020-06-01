//! Types used by NDCA.

use std::fmt;

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;

/// Rust type used for an NDCA cell state.
pub type LangCellState = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 8;

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;

use crate::errors::*;
use crate::Spanned;
use LangErrorMsg::{CustomTypeError, TypeError};

/// Any data type.
///
/// When adding new types, make sure that check lexer::TypeToken and add a
/// corresponding variant there if needed. Also update the list in the error
/// message in parser::ParseBuilder::type_name().
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Type {
    /// Integer.
    Int,
    /// Cell state.
    CellState,
    /// Vector of a specific length (from 1 to 256).
    Vector(usize),
}
impl Default for Type {
    fn default() -> Self {
        Self::Int
    }
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::CellState => write!(f, "cellstate"),
            Self::Vector(len) => write!(f, "vec{}", len),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "integer"),
            Self::CellState => write!(f, "cellstate"),
            Self::Vector(len) => write!(f, "vector{}", len),
        }
    }
}
impl Type {
    /// Returns true if this type has a representation in compiled code, or
    /// false otherwise; i.e. whether a variable can contain a value of this
    /// type.
    pub fn has_runtime_representation(self) -> bool {
        match self {
            Self::Int | Self::CellState | Self::Vector(_) => true,
        }
    }
    /// Returns the number of bytes used to represent this type in compiled
    /// code, or None if this type has no runtime representation.
    pub fn size_of(self) -> Option<usize> {
        // TODO: test this method along with Value::from_bytes() and to_bytes()
        match self {
            Self::Int => Some(std::mem::size_of::<LangInt>()),
            Self::CellState => Some(std::mem::size_of::<LangCellState>()),
            Self::Vector(len) => Some(len * Self::Int.size_of().unwrap()),
        }
    }

    /// Returns true if this type can bde converted to a boolean, or false
    /// otherwise.
    ///
    /// When updating this, make sure to also update ConstValue::to_bool() and
    /// Compiler::build_convert_to_bool()
    pub fn can_convert_to_bool(self) -> bool {
        match self {
            Self::Int | Self::CellState | Self::Vector(_) => true,
        }
    }

    /// Returns a TypeError where this type is the "got" type, given an
    /// "expected" type.
    pub fn type_error(self, expected: Type) -> LangErrorMsg {
        TypeError {
            expected,
            got: self,
        }
    }
}

impl Spanned<Type> {
    /// Returns a TypeError if this type does not match the given expected type.
    pub fn check_eq(self, expected: Type) -> LangResult<()> {
        if self.inner == expected {
            Ok(())
        } else {
            Err(self.inner.type_error(expected).with_span(self.span))
        }
    }
    /// Returns a CustomTypeError if this type is not an integer or vector.
    pub fn check_int_or_vec(self) -> LangResult<()> {
        match self.inner {
            Type::Int | Type::Vector(_) => Ok(()),
            _ => Err(CustomTypeError {
                expected: "integer or vector",
                got: self.inner,
            }
            .with_span(self.span)),
        }
    }
    /// Returns a CustomTypeError if this type cannot be converted to a boolean.
    pub fn check_can_convert_to_bool(self) -> LangResult<()> {
        if self.inner.can_convert_to_bool() {
            Ok(())
        } else {
            Err(CustomTypeError {
                expected: "type that can be converted to boolean",
                got: self.inner,
            }
            .with_span(self.span))
        }
    }
}
