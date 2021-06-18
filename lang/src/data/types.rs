//! Data types of NDCA.
//!
//! When adding a new type, these modifications are necessary:
//!
//! - Add new variant to `Type`
//! - Update `impl TypeChecker for Type` if necessary
//! - Add new variant to `ConstValue`
//! - Add new variant to `LlvmValue`
//! - Add new token to `Token` enum
//! - Update `TryFrom<Token>` impl for `TypeClass`
//! - Add new method in `generate_type_unwrap_methods!`

use std::fmt;
use std::sync::Arc;

use super::VectorSet;

/// Specific data type.
///
/// When adding new types, make sure that check `lexer::TypeToken` and add a
/// corresponding variant there if needed.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
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
    /// No value.
    Null,

    /// Sequence of :data:`Integer` values of a certain length (from 1 to 256).
    Vector(Option<usize>),
    /// Masked N-dimensional array of :data:`Cell` values with a specific size
    /// and shape.
    Array(Option<Arc<VectorSet>>),

    /// Set of `Integer` values.
    IntegerSet,
    /// Set of `Cell` values.
    CellSet,
    /// Set of `Vector` values with the same length (from 1 to 256).
    VectorSet(Option<usize>),
    /// Predicate that accepts or rejects `Array` values with a specific number
    /// of cells.
    Pattern(Option<usize>),
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
            Type::Integer => write!(f, "Integer"),
            Type::Cell => write!(f, "Cell"),
            Type::Tag => write!(f, "Tag"),
            Type::String => write!(f, "String"),
            Type::Type => write!(f, "Type"),
            Type::Null => write!(f, "Null"),

            Type::Vector(None) => write!(f, "Vector"),
            Type::Vector(Some(len)) => write!(f, "Vector[{:?}]", len),
            Type::Array(None) => write!(f, "Array"),
            Type::Array(Some(shape)) => write!(f, "Array[{:?}]", shape),

            Type::IntegerSet => write!(f, "IntegerSet"),
            Type::CellSet => write!(f, "CellSet"),
            Type::VectorSet(None) => write!(f, "VectorSet"),
            Type::VectorSet(Some(len)) => write!(f, "VectorSet[{:?}]", len),
            Type::Pattern(None) => write!(f, "Pattern"),
            Type::Pattern(Some(shape)) => write!(f, "Pattern[{:?}]", shape),
            Type::Regex => write!(f, "Regex"),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Integer"),
            Type::Cell => write!(f, "Cell"),
            Type::Tag => write!(f, "Tag"),
            Type::String => write!(f, "String"),
            Type::Type => write!(f, "Type"),
            Type::Null => write!(f, "Null"),

            Type::Vector(None) => write!(f, "Vector"),
            Type::Vector(Some(len)) => write!(f, "Vector[{}]", len),
            Type::Array(None) => write!(f, "Array"),
            Type::Array(Some(shape)) => write!(f, "Array[{}]", shape),

            Type::IntegerSet => write!(f, "IntegerSet"),
            Type::CellSet => write!(f, "CellSet"),
            Type::VectorSet(None) => write!(f, "VectorSet"),
            Type::VectorSet(Some(len)) => write!(f, "VectorSet[{}]", len),
            Type::Pattern(None) => write!(f, "Pattern"),
            Type::Pattern(Some(shape)) => write!(f, "Pattern[{}]", shape),
            Type::Regex => write!(f, "Regex"),
        }
    }
}
impl Type {
    /// Returns true if this type can be converted to a boolean, or false
    /// otherwise.
    ///
    /// When updating this, make sure to also update `ConstValue::to_bool()` and
    /// `Compiler::build_convert_to_bool()`.
    pub fn can_convert_to_bool(&self) -> bool {
        match self {
            Type::Integer => true,
            Type::Cell => true,
            Type::Tag => false,
            Type::String => true,
            Type::Type => true,
            Type::Null => true,

            Type::Vector(_) => true,
            Type::Array(_) => true,

            Type::IntegerSet => false,
            Type::CellSet => false,
            Type::VectorSet(_) => false,
            Type::Pattern(_) => false,
            Type::Regex => false,
        }
    }
    /// Returns the type yielded by iteration over this type, or None if this
    /// type cannot be iterated over.
    pub fn iteration_type(&self) -> Option<Type> {
        match self {
            Type::Integer => None,
            Type::Cell => None,
            Type::Tag => None,
            Type::String => Some(Type::String),
            Type::Type => None,
            Type::Null => None,

            Type::Vector(_) => Some(Type::Integer),
            Type::Array(_) => Some(Type::Cell),

            Type::IntegerSet => Some(Type::Integer),
            Type::CellSet => Some(Type::Cell),
            Type::VectorSet(len) => Some(Type::Vector(*len)),
            Type::Pattern(_) => None,
            Type::Regex => None,
        }
    }
}
