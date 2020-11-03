//! N-dimensional generalization of Golly's 2D ["Extended RLE"
//! format](http://golly.sourceforge.net/Help/formats.html#rle)
//!
//! Use `format!("{:b}", some_rle)` to convert a 2-state RLE to a string, or
//! `format!("{}", some_rle)` to convert any other RLE to a string. RLEs can be
//! `parse()`d from strings and convert to/from various types for storing
//! patterns.
//!
//! Note that 2D RLEs always have Y values increasing downwards, while NDCell
//! has Y values increasing upwards, so all coordinates except X must be negated
//! and offset, so Y coordinates of 2D RLEs must be negated and offset.
//!
//! See NDCell documentation for a description of this format.

mod components;
mod convert;

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
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RleError {
    UnknownSymbol(char),
    UnknownSymbolPair(char, char),
    ExpectedABC(char),
    ExpectedWUV,
    StateOutOfRange(u32),
    InvalidSize,
    InvalidCount,
    BadCxrleHeader,
    DuplicateRleHeader,
    DuplicateCxrleHeader,
    MissingHeader,
    InvalidItem,
    TooBig,
    NonAscii,
    UnknownRule(String),
    ErrorLoadingRule(String),
    BadRuleDimensionality { expected: usize, got: usize },
}
impl std::fmt::Display for RleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RleError::UnknownSymbol(ch) => write!(f, "Unknown symbol: '{}'", ch),
            RleError::UnknownSymbolPair(ch1, ch2) => write!(f, "Unknown symbols: '{}{}'", ch1, ch2),
            RleError::ExpectedABC(ch) => {
                write!(f, "Expected letter from 'A' to 'X' to follow '{}'", ch)
            }
            RleError::ExpectedWUV => write!(f, "Expected 'W', 'U', or 'V' to follow '%'"),
            RleError::StateOutOfRange(state) => write!(f, "State out of range: #{}", state),
            RleError::InvalidSize => write!(f, "Invalid size"),
            RleError::InvalidCount => write!(f, "Invalid count"),
            RleError::BadCxrleHeader => write!(f, "Bad CXRLE header"),
            RleError::DuplicateRleHeader => write!(f, "Duplicate RLE header"),
            RleError::DuplicateCxrleHeader => write!(f, "Duplicate CXRLE header"),
            RleError::MissingHeader => write!(f, "Missing RLE header"),
            RleError::InvalidItem => write!(f, "Invalid RLE item"),
            RleError::TooBig => write!(f, "Pattern is too big; try another format like Macrocell"),
            RleError::NonAscii => write!(f, "RLE contains non-ASCII characters"),
            RleError::UnknownRule(name) => write!(f, "Unknown rule {:?}", name),
            RleError::ErrorLoadingRule(name) => write!(
                f,
                "Something is wrong with the rule {:?}\nTry creating a new pattern with that rule to troubleshoot",
                name
            ),
            RleError::BadRuleDimensionality{expected, got} => write!(f, "Expected {}D rule; got {}D rule", expected, got),
        }
    }
}

#[cfg(test)]
mod tests;
