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

/// Any data type.
///
/// When adding new types, make sure that check lexer::TypeToken and add a
/// corresponding variant there if needed. Also update the list in the error
/// message in parser::ParseBuilder::type_name().
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "integer"),
            Self::CellState => write!(f, "cell state"),
            Self::Vector(len) => write!(f, "vector of length {}", len),
        }
    }
}
impl Type {
    /// Returns true if this type has a representation in compiled code or false
    /// otherwise; i.e. whether a variable can contain a value of this type.
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
            Self::Vector(len) => Some(len as usize * Self::Int.size_of().unwrap()),
        }
    }
}
