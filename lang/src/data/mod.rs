#[macro_use]
mod types;

mod array;
mod cell_lut;
mod cell_set;
mod int_set;
mod llvm_value;
mod pattern;
mod value;
mod vec_set;

pub use array::Array;
pub use cell_lut::CellLut;
pub use cell_set::CellSet;
pub use int_set::IntegerSet;
pub use llvm_value::LlvmValue;
pub use pattern::Pattern;
pub use types::{FnSignature, SpannedTypeExt, Type};
pub use value::{SpannedValueExt, Value};
pub use vec_set::VectorSet;

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

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;

/// Axis names.
pub const AXES: &'static str = "xyzwuv";
