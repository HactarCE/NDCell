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

    /// Returns true if this type can be converted to a boolean, or false
    /// otherwise.
    ///
    /// When updating this, make sure to also update `ConstValue::to_bool()` and
    /// `Compiler::build_convert_to_bool()`.
    pub fn can_convert_to_bool(&self) -> bool {
        match self {
            Type::Null => true,
            Type::Integer => true,
            Type::Cell => true,
            Type::Tag => false,
            Type::String => true,
            Type::Type => true,

            Type::Vector(_) => true,
            Type::CellArray(_) | Type::CellArrayMut(_) => true,

            Type::EmptySet => false,
            Type::IntegerSet => false,
            Type::CellSet => false,
            Type::VectorSet(_) => false,
            Type::PatternMatcher(_) => false,
            Type::Regex => false,
        }
    }
    /// Returns the type yielded by iteration over this type, or None if this
    /// type cannot be iterated over.
    pub fn iteration_type(&self) -> Option<Type> {
        match self {
            Type::Null => None,
            Type::Integer => None,
            Type::Cell => None,
            Type::Tag => Some(Type::Cell),
            Type::String => Some(Type::String),
            Type::Type => None,

            Type::Vector(_) => Some(Type::Integer),
            Type::CellArray(_) | Type::CellArrayMut(_) => Some(Type::Cell),

            Type::EmptySet => Some(Type::Null),
            Type::IntegerSet => Some(Type::Integer),
            Type::CellSet => Some(Type::Cell),
            Type::VectorSet(len) => Some(Type::Vector(*len)),
            Type::PatternMatcher(_) => None,
            Type::Regex => None,
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
