#[macro_use]
mod types;

mod array;
mod cell_lut;
mod cell_set;
mod int_set;
mod pattern;
mod value;
mod vec_set;

pub use array::Array;
pub use cell_lut::CellLut;
pub use cell_set::CellSet;
pub use int_set::IntegerSet;
pub use pattern::Pattern;
pub use types::{FnSignature, LangInt, LangUint, Type, TypeClass, INT_BITS, INT_BYTES};
pub use value::Value;
pub use vec_set::VectorSet;

/// Rust type used for an NDCA cell state.
pub type LangCellState = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 8;

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;

/// Axis names.
pub const AXES: &'static str = "xyzwuv";
