use codemap::Spanned;
use std::fmt;
use std::sync::Arc;

use super::{Array, CellSet, IntegerSet, LangCell, LangInt, Pattern, Type, VectorSet};
use crate::errors::{Error, Result};
use crate::regex::Regex;

/// Runtime value or compile-time constant of any type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RtVal {
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
impl fmt::Display for RtVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RtVal::Integer(i) => write!(f, "{}", i),
            RtVal::Cell(i) => write!(f, "#{}", i),
            RtVal::Tag(_) => todo!("display Tag"),
            RtVal::String(s) => write!(f, "{:?}", s),
            RtVal::Type(_) => todo!("display Type"),
            RtVal::Null => write!(f, "Null"),

            RtVal::Vector(v) => write!(f, "{}", crate::utils::display_bracketed_list(v)),
            RtVal::Array(_) => todo!("display Array"),

            RtVal::IntegerSet(_) => todo!("display IntegerSet"),
            RtVal::CellSet(_) => todo!("display CellSet"),
            RtVal::VectorSet(set) => write!(f, "{}", set),
            RtVal::Pattern(_) => todo!("display Pattern"),
            RtVal::Regex(_) => todo!("display Regex"),
        }
    }
}
impl RtVal {
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

    /// Converts the value to a boolean if it can be converted; otherwise
    /// returns `None`.
    pub fn to_bool(&self) -> Option<bool> {
        match self {
            RtVal::Integer(i) => Some(*i != 0),
            RtVal::Cell(i) => Some(*i != 0),
            RtVal::String(s) => Some(!s.is_empty()),

            RtVal::Vector(v) => Some(v.iter().any(|&i| i != 0)),
            RtVal::Array(a) => Some(a.any_nonzero()),

            _ => None,
        }
    }
    /// Converts the value to a vector if it can be converted; otherwise returns
    /// `None`.
    pub fn to_vector(&self, len: usize) -> Option<Vec<LangInt>> {
        match self {
            RtVal::Integer(i) => Some(vec![*i; len]),
            RtVal::Vector(v) => {
                let mut ret = v.clone();
                ret.resize(len, 0);
                ret.shrink_to_fit();
                Some(ret)
            }
            _ => None,
        }
    }
}

pub trait SpannedRuntimeValueExt {
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

    /// Converts the value to a boolean if it can be converted; otherwise
    /// returns a type error.
    fn to_bool(&self) -> Result<bool>;
    /// Converts the value to a vector if it can be converted; otherwise returns
    /// a type error.
    fn to_vector(&self, len: usize) -> Result<Vec<LangInt>>;

    /// Returns an iterator for the value if it can be iterated over; otherwise
    /// returns a type error.
    fn iterate(self) -> Result<Box<dyn Iterator<Item = Self>>>;
}
impl SpannedRuntimeValueExt for Spanned<RtVal> {
    fn as_integer(self) -> Result<LangInt> {
        match self.node {
            RtVal::Integer(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Integer, &self.ty())),
        }
    }
    fn as_cell(self) -> Result<LangCell> {
        match self.node {
            RtVal::Cell(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Cell, &self.ty())),
        }
    }
    fn as_tag(self) -> Result<Arc<String>> {
        match self.node {
            RtVal::Tag(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Tag, &self.ty())),
        }
    }
    fn as_string(self) -> Result<Arc<String>> {
        match self.node {
            RtVal::String(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::String, &self.ty())),
        }
    }
    fn as_type(self) -> Result<Type> {
        match self.node {
            RtVal::Type(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Type, &self.ty())),
        }
    }
    fn as_null(self) -> Result<()> {
        match self.node {
            RtVal::Null => Ok(()),
            _ => Err(Error::type_error(self.span, Type::Null, &self.ty())),
        }
    }
    fn as_vector(self) -> Result<Vec<LangInt>> {
        match self.node {
            RtVal::Vector(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Vector(None), &self.ty())),
        }
    }
    fn as_array(self) -> Result<Arc<Array>> {
        match self.node {
            RtVal::Array(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Array(None), &self.ty())),
        }
    }
    fn as_integer_set(self) -> Result<Arc<IntegerSet>> {
        match self.node {
            RtVal::IntegerSet(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::IntegerSet, &self.ty())),
        }
    }
    fn as_cell_set(self) -> Result<CellSet> {
        match self.node {
            RtVal::Cell(x) => Ok(CellSet::single_cell(x)),
            RtVal::Tag(x) => Err(Error::unimplemented(self.span)), // TODO: only allow in compiled code? should allow everywhere
            RtVal::CellSet(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::CellSet, &self.ty())),
        }
    }
    fn as_vector_set(self) -> Result<Arc<VectorSet>> {
        match self.node {
            RtVal::VectorSet(x) => Ok(x),
            _ => Err(Error::type_error(
                self.span,
                Type::VectorSet(None),
                &self.ty(),
            )),
        }
    }
    fn as_pattern(self) -> Result<Arc<Pattern>> {
        match self.node {
            RtVal::Pattern(x) => Ok(x),
            _ => Err(Error::type_error(
                self.span,
                Type::Pattern(None),
                &self.ty(),
            )),
        }
    }
    fn as_regex(self) -> Result<Arc<Regex>> {
        match self.node {
            RtVal::Regex(x) => Ok(x),
            _ => Err(Error::type_error(self.span, Type::Regex, &self.ty())),
        }
    }

    fn to_bool(&self) -> Result<bool> {
        self.node.to_bool().ok_or_else(|| {
            Error::type_error(
                self.span,
                "type that can be converted to boolean",
                &self.ty(),
            )
        })
    }
    fn to_vector(&self, len: usize) -> Result<Vec<LangInt>> {
        self.node.to_vector(len).ok_or_else(|| {
            Error::type_error(
                self.span,
                "type that can be converted to a vector",
                &self.ty(),
            )
        })
    }

    fn iterate(self) -> Result<Box<dyn Iterator<Item = Self>>> {
        Err(Error::unimplemented(None))
    }
}
