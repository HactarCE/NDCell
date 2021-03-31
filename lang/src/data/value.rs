use codemap::Spanned;
use std::sync::Arc;

use super::{Array, CellSet, IntegerSet, LangCell, LangInt, Pattern, Type, VectorSet};
use crate::errors::{Error, Result};
use crate::regex::Regex;

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    /// 64-bit signed integer, also used for boolean values.
    Integer(LangInt),
    /// Cell state represented by an 8-bit unsigned integer.
    Cell(LangCell),
    /// Property held by some cell states (represented by its name).
    Tag(Arc<String>),
    /// Sequence of Unicode characters.
    String(Arc<String>),
    /// Type value.
    Type(Type),
    /// No value.
    Null,

    /// Sequence of :data:`Integer` values of a certain length (from 1 to 256).
    Vector(Vec<LangInt>),
    /// Masked N-dimensional array of :data:`Cell` values with a specific size
    /// and shape.
    Array(Arc<Array>),

    /// Set of `Integer` values.
    IntegerSet(Arc<IntegerSet>),
    /// Set of `Cell` values.
    CellSet(CellSet),
    /// Set of `Vector` values with the same length (from 1 to 256).
    VectorSet(Arc<VectorSet>),
    /// Predicate that accepts or rejects `Array` values with a specific number
    /// of cells.
    Pattern(Arc<Pattern>),
    /// Regular expression.
    Regex(Arc<Regex>),
}
impl Value {
    /// Returns the type of the value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Integer(_) => Type::Integer,
            Self::Cell(_) => Type::Cell,
            Self::Tag(_) => Type::Tag,
            Self::String(_) => Type::String,
            Self::Type(_) => Type::Type,
            Self::Null => Type::Null,

            Self::Vector(v) => Type::Vector(Some(v.len())),
            Self::Array(a) => Type::Array(Some(a.shape())),

            Self::IntegerSet(_) => Type::IntegerSet,
            Self::CellSet(_) => Type::CellSet,
            Self::VectorSet(set) => Type::VectorSet(Some(set.vec_len())),
            Self::Pattern(pat) => Type::Pattern(Some(pat.len())),
            Self::Regex(_) => Type::Regex,
        }
    }
}

pub trait SpannedValueExt {
    /// Returns the value inside if this is an `Integer` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_integer(self) -> Result<LangInt>;
    /// Returns the value inside if this is a `Cell` variant or subtype of one;
    /// otherwise returns a type error.
    fn as_cell(self) -> Result<LangCell>;
    /// Returns the value inside if this is a `Tag` variant or subtype of one;
    /// otherwise returns a type error.
    fn as_tag(self) -> Result<Arc<String>>;
    /// Returns the value inside if this is a `String` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_string(self) -> Result<Arc<String>>;
    /// Returns the value inside if this is a `Type` variant or subtype of one;
    /// otherwise returns a type error.
    fn as_type(self) -> Result<Type>;
    /// Returns the value inside if this is a `Null` variant or subtype of one;
    /// otherwise returns a type error.
    fn as_null(self) -> Result<()>;
    /// Returns the value inside if this is a `Vector` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_vector(self) -> Result<Vec<LangInt>>;
    /// Returns the value inside if this is an `Array` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_array(self) -> Result<Arc<Array>>;
    /// Returns the value inside if this is an `IntegerSet` variant or subtype
    /// of one; otherwise returns a type error.
    fn as_integer_set(self) -> Result<Arc<IntegerSet>>;
    /// Returns the value inside if this is a `CellSet` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_cell_set(self) -> Result<CellSet>;
    /// Returns the value inside if this is a `VectorSet` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_vector_set(self) -> Result<Arc<VectorSet>>;
    /// Returns the value inside if this is a `Pattern` variant or subtype of
    /// one; otherwise returns a type error.
    fn as_pattern(self) -> Result<Arc<Pattern>>;
    /// Returns the value inside if this is a `Regex` variant or subtype of one;
    /// otherwise returns a type error.
    fn as_regex(self) -> Result<Arc<Regex>>;

    /// Converts this value to a boolean if it can be converted; otherwise
    /// returns a type error.
    fn to_bool(self) -> Result<bool>;

    /// Returns an iterator for this value if it can be iterated over; otherwise
    /// returns a type error.
    fn iterate(self) -> Result<Box<dyn Iterator<Item = Self>>>;
}

impl SpannedValueExt for Spanned<Value> {
    fn as_integer(self) -> Result<LangInt> {
        match self.node {
            Value::Integer(x) => Ok(x),
            _ => uncaught_type_error!(in as_integer),
        }
    }
    fn as_cell(self) -> Result<LangCell> {
        match self.node {
            Value::Cell(x) => Ok(x),
            _ => uncaught_type_error!(in as_cell),
        }
    }
    fn as_tag(self) -> Result<Arc<String>> {
        match self.node {
            Value::Tag(x) => Ok(x),
            _ => uncaught_type_error!(in as_tag),
        }
    }
    fn as_string(self) -> Result<Arc<String>> {
        match self.node {
            Value::String(x) => Ok(x),
            _ => uncaught_type_error!(in as_string),
        }
    }
    fn as_type(self) -> Result<Type> {
        match self.node {
            Value::Type(x) => Ok(x),
            _ => uncaught_type_error!(in as_type),
        }
    }
    fn as_null(self) -> Result<()> {
        match self.node {
            Value::Null => Ok(()),
            _ => uncaught_type_error!(in as_null),
        }
    }
    fn as_vector(self) -> Result<Vec<LangInt>> {
        match self.node {
            Value::Vector(x) => Ok(x),
            _ => uncaught_type_error!(in as_vector),
        }
    }
    fn as_array(self) -> Result<Arc<Array>> {
        match self.node {
            Value::Array(x) => Ok(x),
            _ => uncaught_type_error!(in as_array),
        }
    }
    fn as_integer_set(self) -> Result<Arc<IntegerSet>> {
        match self.node {
            Value::IntegerSet(x) => Ok(x),
            _ => uncaught_type_error!(in as_integer_set),
        }
    }
    fn as_cell_set(self) -> Result<CellSet> {
        match self.node {
            Value::Cell(x) => Ok(CellSet::single_cell(x)),
            Value::Tag(x) => Err(Error::unimplemented(self.span)), // TODO: only allow in compiled code? should allow everywhere
            Value::CellSet(x) => Ok(x),
            _ => uncaught_type_error!(in as_cell_set),
        }
    }
    fn as_vector_set(self) -> Result<Arc<VectorSet>> {
        match self.node {
            Value::VectorSet(x) => Ok(x),
            _ => uncaught_type_error!(in as_vector_set),
        }
    }
    fn as_pattern(self) -> Result<Arc<Pattern>> {
        match self.node {
            Value::Pattern(x) => Ok(x),
            _ => uncaught_type_error!(in as_pattern),
        }
    }
    fn as_regex(self) -> Result<Arc<Regex>> {
        match self.node {
            Value::Regex(x) => Ok(x),
            _ => uncaught_type_error!(in as_regex),
        }
    }

    fn to_bool(self) -> Result<bool> {
        match self.node {
            Value::Integer(i) => Ok(i != 0),
            Value::Cell(i) => Ok(i != 0),
            Value::String(s) => Ok(!s.is_empty()),

            Value::Vector(v) => Ok(v.into_iter().any(|i| i != 0)),
            Value::Array(a) => Ok(a.any_nonzero()),

            _ => uncaught_type_error!(in to_bool),
        }
    }

    fn iterate(self) -> Result<Box<dyn Iterator<Item = Self>>> {
        Err(Error::unimplemented(None))
    }
}
