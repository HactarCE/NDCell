use std::fmt;

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;
/// Name of LLVM type used for an NDCA integer.
pub const INT_LLVM_TYPE_STR: &'static str = "i64";

/// Rust type used for an NDCA cell state.
pub type LangCellState = u32;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 32;

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
