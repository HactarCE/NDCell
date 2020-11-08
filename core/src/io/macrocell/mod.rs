mod components;
mod convert;

pub use components::Macrocell;
use components::*;

/// Result type returned by fallible Macrocell routines.
pub type MacrocellResult<T> = Result<T, MacrocellError>;

/// Error encountered during Macrocell import.
#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MacrocellError {
    StateOutOfRange(u32),
    DuplicateHeader,
    MissingHeader,
    InvalidContent,
    LeafNodeNon2D,
}
impl std::fmt::Display for MacrocellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MacrocellError::StateOutOfRange(state) => write!(f, "State out of range: #{}", state),
            MacrocellError::DuplicateHeader => write!(f, "Duplicate Macrocell header"),
            MacrocellError::MissingHeader => write!(f, "Missing Macrocell header"),
            MacrocellError::InvalidContent => write!(f, "Invalid Macrocell content"),
            MacrocellError::LeafNodeNon2D => {
                write!(f, "Non-2D Macrocell contains 2D 8x8 leaf node")
            }
        }
    }
}

#[cfg(test)]
mod tests;
