//! N-dimensional generalization of Golly's 2D ["Extended RLE"
//! format](http://golly.sourceforge.net/Help/formats.html#rle)
//!
//! Note that 2D RLEs always have Y values increasing downwards, while NDCell
//! has Y values increasing upwards, so all coordinates except X must be negated
//! and offset, so Y coordinates of 2D RLEs must be negated and offset.
//!
//! See NDCell documentation for a description of this format.

use thiserror::Error;

mod components;
mod convert;

use super::{CaFormatTrait, TwoState};
pub use components::Rle;
use components::*;

const MAX_LINE_LEN: usize = 70;

/// Result type returned by fallible RLE routines.
pub type RleResult<T> = Result<T, RleError>;

lazy_static::lazy_static! {
    /// Regex matching an optional positive integer followed by a single RLE
    /// item.
    static ref RLE_RUN_REGEX: regex::Regex =
        regex::Regex::new(r"\d*([bo\.$/!]|[p-y]?[A-X]|%[WUV])").unwrap();
}

/// Error encountered during RLE import/export.
#[allow(missing_docs)]
#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub enum RleError {
    #[error("unknown symbol: '{0}'")]
    UnknownSymbol(char),
    #[error("unknown symbols: '{0}{1}'")]
    UnknownSymbolPair(char, char),
    #[error("expected letter from 'A' to 'X' to follow '{0}'")]
    ExpectedABC(char),
    #[error("expected 'W', 'U', or 'V' to follow '%'")]
    ExpectedWUV,
    #[error("state out of range: #{0}")]
    StateOutOfRange(u32),
    #[error("invalid size")]
    InvalidSize,
    #[error("invalid count")]
    InvalidCount,
    #[error("bad CXRLE header")]
    BadCxrleHeader,
    #[error("duplicate RLE header")]
    DuplicateRleHeader,
    #[error("duplicate CXRLE header")]
    DuplicateCxrleHeader,
    #[error("missing RLE header")]
    MissingHeader,
    #[error("invalid RLE item")]
    InvalidItem,
    #[error("pattern is too big; try another format like Macrocell")]
    TooBig,
    #[error("rLE contains non-ASCII characters")]
    NonAscii,
    #[error("unknown rule {0:?}")]
    UnknownRule(String),
    #[error("something is wrong with the rule {0:?}; try creating a new pattern with that rule to troubleshoot")]
    ErrorLoadingRule(String),
    #[error("expected {expected}D rule; got {got}D rule")]
    BadRuleDimensionality { expected: usize, got: usize },
}

#[cfg(test)]
mod tests;
