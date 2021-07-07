use codemap::Span;
use std::convert::TryInto;

use crate::errors::{Error, Result};

#[macro_use]
mod types;
mod array;
mod cell_lut;
mod cell_set;
mod compile_value;
mod int_set;
mod pattern;
mod runtime_value;
mod value;
mod vector_set;

pub use array::Array;
pub use cell_lut::CellLut;
pub use cell_set::CellSet;
pub use compile_value::{CpVal, SpannedCompileValueExt};
pub use int_set::IntegerSet;
pub use pattern::Pattern;
pub use runtime_value::{RtVal, SpannedRuntimeValueExt};
pub use types::Type;
pub use value::{FallibleTypeOf, Val};
pub use vector_set::VectorSet;

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Unsigned rust type used for NDCA integers.
pub type LangUint = u64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;
/// Number of bytes in an NDCA integer.
pub const INT_BYTES: usize = INT_BITS as usize / 8;

/// Rust type used for an NDCA cell state.
pub type LangCell = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 8;
/// Number of bytes in an NDCA cell state.
pub const CELL_STATE_BYTES: usize = CELL_STATE_BITS as usize / 8;

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;
/// Maximum number of cells for a vector set's bounding rectangle.
pub const MAX_VECTOR_SET_SIZE: usize = 1 << 16;

/// Axis names.
pub const AXES: &'static str = "xyzwuv";

/// Returns whether a vector length is valid.
pub fn is_valid_vector_len(len: usize) -> bool {
    1 <= len && len <= MAX_VECTOR_LEN
}

/// Checks whether a vector length is valid and returns an error if it is not.
pub fn check_vector_len(span: Span, len: impl TryInto<usize>) -> Result<usize> {
    len.try_into()
        .ok()
        .filter(|&n| is_valid_vector_len(n))
        .ok_or(Error::invalid_vector_length(span))
}
