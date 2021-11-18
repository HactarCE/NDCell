use codemap::{Span, Spanned};
use std::convert::TryInto;

use crate::errors::{Error, Result};

#[macro_use]
mod types;
mod cell_array;
mod cell_lut;
mod cell_set;
mod compile_value;
mod integer_set;
mod pattern_matcher;
mod runtime_value;
mod value;
mod vector_set;

pub use cell_array::{CellArray, LlvmCellArray};
pub use cell_lut::CellLut;
pub use cell_set::CellSet;
pub use compile_value::{CpVal, SpannedCompileValueExt};
pub use integer_set::IntegerSet;
pub use pattern_matcher::PatternMatcher;
pub use runtime_value::{RtVal, SpannedRuntimeValueExt};
pub use types::{GetType, Type};
pub use value::{SpannedVal, SpannedValExt, Val};
pub use vector_set::{check_vector_set_vec_len, VectorSet};

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Unsigned rust type used for NDCA integers.
pub type LangUint = u64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = LangInt::BITS;
/// Number of bytes in an NDCA integer.
pub const INT_BYTES: usize = INT_BITS as usize / 8;

/// Rust type used for an NDCA cell state.
pub type LangCell = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = LangCell::BITS;
/// Number of bytes in an NDCA cell state.
pub const CELL_STATE_BYTES: usize = CELL_STATE_BITS as usize / 8;

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;
/// Maximum number of cells for a vector set's bounding rectangle.
pub const MAX_VECTOR_SET_SIZE: usize = 1 << 16;
/// Maximum component value for a member of a vector set.
pub const MAX_VECTOR_SET_EXTENT: isize = 1 << 16;

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

/// Coerce two values to vectors of the same length.
pub fn coerce_vectors_together(
    v1: &Spanned<RtVal>,
    v2: &Spanned<RtVal>,
    vec_len_merge: impl FnMut(usize, usize) -> usize,
) -> Option<(Vec<LangInt>, Vec<LangInt>)> {
    let len = crate::utils::map_and_merge_options(
        v1.as_vector().ok(),
        v2.as_vector().ok(),
        |v| v.len(),
        vec_len_merge,
    )?;
    // Resize the vectors to the same length.
    Some((v1.to_vector(len).ok()?, v2.to_vector(len).ok()?))
}
