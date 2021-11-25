//! Data types of NDCA.
//!
//! When adding a new type, these modifications are necessary:
//!
//! - Add new variant to `Type`
//! - Add new variant to `RtVal`
//! - Add new variant to `CpVal`
//! - Update `TryFrom<Token>` impl for `TypeClass`
//! - Add new method in `SpannedRuntimeValueExt`
//! - Add new method in `SpannedCompileValueExt`

use codemap::Spanned;
use std::fmt;
use std::sync::Arc;

use super::VectorSet;

/// Specific data type.
///
/// When adding new types, make sure that check `lexer::TypeToken` and add a
/// corresponding variant there if needed.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// No value.
    Null,
    /// 64-bit signed integer, also used for boolean values.
    Integer,
    /// Cell state represented by an 8-bit unsigned integer.
    Cell,
    /// Property held by some cell states.
    Tag,
    /// Sequence of Unicode characters.
    String,
    /// Type value.
    Type,

    /// Sequence of `Integer` values of a certain length (from 1 to 256).
    Vector(Option<usize>),
    /// Masked immutable N-dimensional array of `Cell` values with a specific
    /// size and shape.
    CellArray(Option<Arc<VectorSet>>),
    /// Masked mutable N-dimensional array of `Cell` values with a specific size
    /// and shape.
    CellArrayMut(Option<Arc<VectorSet>>),

    /// Empty set.
    EmptySet,
    /// Set of `Integer` values.
    IntegerSet,
    /// Set of `Cell` values.
    CellSet,
    /// Set of `Vector` values with the same length (from 1 to 256).
    VectorSet(Option<usize>),
    /// Predicate that accepts or rejects `CellArray` values with a specific
    /// number of cells.
    PatternMatcher(Option<usize>),
    /// Regular expression.
    Regex,
}
impl Default for Type {
    fn default() -> Self {
        Type::Null
    }
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Null => write!(f, "Null"),
            Type::Integer => write!(f, "Integer"),
            Type::Cell => write!(f, "Cell"),
            Type::Tag => write!(f, "Tag"),
            Type::String => write!(f, "String"),
            Type::Type => write!(f, "Type"),

            Type::Vector(None) => write!(f, "Vector"),
            Type::Vector(Some(len)) => write!(f, "Vector[{:?}]", len),
            Type::CellArray(None) => write!(f, "CellArray"),
            Type::CellArray(Some(shape)) => write!(f, "CellArray[{:?}]", shape),
            Type::CellArrayMut(None) => write!(f, "CellArrayMut"),
            Type::CellArrayMut(Some(shape)) => write!(f, "CellArrayMut[{:?}]", shape),

            Type::EmptySet => write!(f, "EmptySet"),
            Type::IntegerSet => write!(f, "IntegerSet"),
            Type::CellSet => write!(f, "CellSet"),
            Type::VectorSet(None) => write!(f, "VectorSet"),
            Type::VectorSet(Some(len)) => write!(f, "VectorSet[{:?}]", len),
            Type::PatternMatcher(None) => write!(f, "PatternMatcher"),
            Type::PatternMatcher(Some(shape)) => write!(f, "PatternMatcher[{:?}]", shape),
            Type::Regex => write!(f, "Regex"),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Null => write!(f, "Null"),
            Type::Integer => write!(f, "Integer"),
            Type::Cell => write!(f, "Cell"),
            Type::Tag => write!(f, "Tag"),
            Type::String => write!(f, "String"),
            Type::Type => write!(f, "Type"),

            Type::Vector(None) => write!(f, "Vector"),
            Type::Vector(Some(len)) => write!(f, "Vector[{}]", len),
            Type::CellArray(None) => write!(f, "CellArray"),
            Type::CellArray(Some(shape)) => write!(f, "CellArray[{}]", shape),
            Type::CellArrayMut(None) => write!(f, "CellArrayMut"),
            Type::CellArrayMut(Some(shape)) => write!(f, "CellArrayMut[{}]", shape),

            Type::EmptySet => write!(f, "EmptySet"),
            Type::IntegerSet => write!(f, "IntegerSet"),
            Type::CellSet => write!(f, "CellSet"),
            Type::VectorSet(None) => write!(f, "VectorSet"),
            Type::VectorSet(Some(len)) => write!(f, "VectorSet[{}]", len),
            Type::PatternMatcher(None) => write!(f, "PatternMatcher"),
            Type::PatternMatcher(Some(shape)) => write!(f, "PatternMatcher[{}]", shape),
            Type::Regex => write!(f, "Regex"),
        }
    }
}
impl Type {
    /// Returns the name of the unparametrized type.
    pub fn unparametrized_name(&self) -> &str {
        match self {
            Type::Null => "Null",
            Type::Integer => "Integer",
            Type::Cell => "Cell",
            Type::Tag => "Tag",
            Type::String => "String",
            Type::Type => "Type",

            Type::Vector(_) => "Vector",
            Type::CellArray(_) => "CellArray",
            Type::CellArrayMut(_) => "CellArrayMut",

            Type::EmptySet => "EmptySet",
            Type::IntegerSet => "IntegerSet",
            Type::CellSet => "CellSet",
            Type::VectorSet(_) => "VectorSet",
            Type::PatternMatcher(_) => "PatternMatcher",
            Type::Regex => "Regex",
        }
    }
}

/// Value with a well-defined type.
pub trait GetType {
    /// Returns the type of a value.
    fn ty(&self) -> Type;
}
impl<T: GetType> GetType for Spanned<T> {
    fn ty(&self) -> Type {
        self.node.ty()
    }
}
