//! Types used by NDCA.

use std::fmt;

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;

/// Rust type used for an NDCA cell state.
pub type LangCellState = u32;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 32;

/// Any data type.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    /// Integer.
    Int,
    /// Cell state.
    CellState,
    /// Vector of a specific length (from 1 to 6).
    Vector(u8),
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
