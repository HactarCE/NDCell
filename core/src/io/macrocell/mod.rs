//! N-dimensional generalization of Golly's 2D ["Macrocell"
//! format](http://golly.sourceforge.net/Help/formats.html#mc)
//!
//! See NDCell documentation for a description of this format.

use thiserror::Error;

mod components;
mod convert;

use super::CaFormatTrait;
pub use components::Macrocell;
use components::*;

/// Result type returned by fallible Macrocell routines.
pub type MacrocellResult<T> = Result<T, MacrocellError>;

/// Error encountered during Macrocell import.
#[allow(missing_docs)]
#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub enum MacrocellError {
    #[error("state out of range: #{0}")]
    StateOutOfRange(u32),
    #[error("duplicate Macrocell header")]
    DuplicateHeader,
    #[error("missing Macrocell header")]
    MissingHeader,
    #[error("invalid Macrocell content")]
    InvalidContent,
    #[error("non-2D Macrocell contains 2D 8x8 leaf node")]
    LeafNodeNon2D,
}

#[cfg(test)]
mod tests;
