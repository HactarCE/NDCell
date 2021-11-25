use codemap::{Span, Spanned};
use std::fmt;
use std::sync::Arc;

use super::{
    CellArray, CellSet, GetType, IntegerSet, LangCell, LangInt, PatternMatcher, Type, VectorSet,
};
use crate::errors::{Error, Result};
use crate::regex::Regex;

/// Runtime value or compile-time constant of any type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RtVal {
    /// No value.
    Null,
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

    /// Sequence of `Integer` values of a certain length (from 1 to 256).
    Vector(Vec<LangInt>),
    /// Masked immutable N-dimensional array of `Cell` values with a specific
    /// size and shape.
    CellArray(Arc<CellArray>),

    /// Empty set.
    EmptySet,
    /// Set of `Integer` values.
    IntegerSet(Arc<IntegerSet>),
    /// Set of `Cell` values.
    CellSet(CellSet),
    /// Set of `Vector` values with the same length (from 1 to 256).
    VectorSet(Arc<VectorSet>),
    /// Predicate that accepts or rejects `CellArray` values with a specific
    /// number of cells.
    PatternMatcher(Arc<PatternMatcher>),
    /// Regular expression.
    Regex(Arc<Regex>),
}
impl fmt::Display for RtVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RtVal::Null => write!(f, "Null"),
            RtVal::Integer(i) => write!(f, "{}", i),
            RtVal::Cell(i) => write!(f, "#{}", i),
            RtVal::Tag(_) => todo!("display Tag"),
            RtVal::String(s) => write!(f, "{:?}", s),
            RtVal::Type(_) => todo!("display Type"),

            RtVal::Vector(v) => write!(f, "{}", crate::utils::display_bracketed_list(v)),
            RtVal::CellArray(a) => write!(f, "{}", a),

            RtVal::EmptySet => write!(f, "{{}}"),
            RtVal::IntegerSet(set) => write!(f, "{}", set),
            RtVal::CellSet(_) => todo!("display CellSet"),
            RtVal::VectorSet(set) => write!(f, "{}", set),
            RtVal::PatternMatcher(_) => todo!("display PatternMatcher"),
            RtVal::Regex(_) => todo!("display Regex"),
        }
    }
}
impl GetType for RtVal {
    fn ty(&self) -> Type {
        match self {
            Self::Null => Type::Null,
            Self::Integer(_) => Type::Integer,
            Self::Cell(_) => Type::Cell,
            Self::Tag(_) => Type::Tag,
            Self::String(_) => Type::String,
            Self::Type(_) => Type::Type,

            Self::Vector(v) => Type::Vector(Some(v.len())),
            Self::CellArray(a) => Type::CellArray(Some(Arc::clone(a.shape()))),

            Self::EmptySet => Type::EmptySet,
            Self::IntegerSet(_) => Type::IntegerSet,
            Self::CellSet(_) => Type::CellSet,
            Self::VectorSet(set) => Type::VectorSet(Some(set.vec_len())),
            Self::PatternMatcher(pat) => Type::PatternMatcher(Some(pat.len())),
            Self::Regex(_) => Type::Regex,
        }
    }
}
impl RtVal {
    /// Converts the value to a boolean if it can be converted; otherwise
    /// returns `None`.
    pub fn to_bool(&self) -> Option<bool> {
        match self {
            RtVal::Integer(i) => Some(*i != 0),
            RtVal::Cell(i) => Some(*i != 0),
            RtVal::String(s) => Some(!s.is_empty()),

            RtVal::Vector(v) => Some(v.iter().any(|&i| i != 0)),
            RtVal::CellArray(a) => Some(a.any_nonzero()),

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

impl From<CellArray> for RtVal {
    fn from(a: CellArray) -> Self {
        Self::CellArray(Arc::new(a))
    }
}
impl From<IntegerSet> for RtVal {
    fn from(set: IntegerSet) -> Self {
        Self::IntegerSet(Arc::new(set))
    }
}
impl From<CellSet> for RtVal {
    fn from(set: CellSet) -> Self {
        Self::CellSet(set)
    }
}
impl From<VectorSet> for RtVal {
    fn from(set: VectorSet) -> Self {
        Self::VectorSet(Arc::new(set))
    }
}

pub trait SpannedRuntimeValueExt {
    /// Returns the value inside if this is a `Null` or subtype of one;
    /// otherwise returns a type error.
    fn as_null(&self) -> Result<()>;
    /// Returns the value inside if this is an `Integer` or subtype of one;
    /// otherwise returns a type error.
    fn as_integer(&self) -> Result<LangInt>;
    /// Returns the value inside if this is a `Cell` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell(&self) -> Result<LangCell>;
    /// Returns the value inside if this is a `Tag` or subtype of one; otherwise
    /// returns a type error.
    fn as_tag(&self) -> Result<Arc<String>>;
    /// Returns the value inside if this is a `String` or subtype of one;
    /// otherwise returns a type error.
    fn as_string(&self) -> Result<Arc<String>>;
    /// Returns the value inside if this is a `Type` or subtype of one;
    /// otherwise returns a type error.
    fn as_type(&self) -> Result<Type>;
    /// Returns the value inside if this is a `Vector` or subtype of one;
    /// otherwise returns a type error.
    fn as_vector(&self) -> Result<Vec<LangInt>>;
    /// Returns the value inside if this is an `CellArray` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell_array(&self) -> Result<Arc<CellArray>>;
    /// Returns the value inside if this is an `EmptySet` or subtype of one;
    /// otherwise returns a type error.
    fn as_empty_set(&self) -> Result<()>;
    /// Returns the value inside if this is an `IntegerSet` or subtype of one;
    /// otherwise returns a type error.
    fn as_integer_set(&self) -> Result<Arc<IntegerSet>>;
    /// Returns the value inside if this is a `CellSet` or subtype of one;
    /// otherwise returns a type error.
    fn as_cell_set(&self) -> Result<CellSet>;
    /// Returns the value inside if this is a `VectorSet`; otherwise returns a
    /// type error.
    ///
    /// `vec_len` is merely a suggestion, used when casting an `EmptySet`. **The
    /// returned set may not have the specified `vec_len`.** Additionally, this
    /// method may return an internal error if `vec_len` is invalid.
    fn as_vector_set(&self, vec_len: usize) -> Result<Arc<VectorSet>>;
    /// Returns the value inside if this is a `PatternMatcher` or subtype of
    /// one; otherwise returns a type error.
    fn as_pattern_matcher(&self) -> Result<Arc<PatternMatcher>>;
    /// Returns the value inside if this is a `Regex` or subtype of one;
    /// otherwise returns a type error.
    fn as_regex(&self) -> Result<Arc<Regex>>;

    /// Converts the value to a boolean if it can be converted; otherwise
    /// returns a type error.
    fn to_bool(&self) -> Result<bool>;
    /// Converts the value to a vector of the specified length if it can be
    /// converted; otherwise returns a type error.
    fn to_vector(&self, len: usize) -> Result<Vec<LangInt>>;
    /// Converts the value to a vector set with the specified vector length if
    /// it can be converted; otherwise returns a type error.
    ///
    /// `span` is the span of `vec_len`.
    fn to_vector_set(&self, span: Span, vec_len: usize) -> Result<Arc<VectorSet>>;
    /// Selects a cell from the value.
    ///
    /// - If this is a `Cell`, returns the value.
    /// - If this is a `CellSet` or subtype of one and it is nonempty, returns
    ///   the lowest cell in the set.
    /// - Otherwise, returns an error.
    fn select_cell(&self) -> Result<LangCell>;

    /// Returns an iterator for the value if it can be iterated over and its
    /// indices if they can be iterated over; otherwise returns a type error.
    ///
    /// Each element of the iterator is `(index, value)` where `index` is
    /// present either for all items or for none.
    fn iterate<'a>(&'a self) -> Result<Box<dyn 'a + Iterator<Item = (Option<RtVal>, RtVal)>>>;
}
impl SpannedRuntimeValueExt for Spanned<RtVal> {
    fn as_null(&self) -> Result<()> {
        match &self.node {
            RtVal::Null => Ok(()),
            _ => Err(Error::type_error(self.span, Type::Null, &self.ty())),
        }
    }
    fn as_integer(&self) -> Result<LangInt> {
        match &self.node {
            RtVal::Integer(x) => Ok(*x),
            _ => Err(Error::type_error(self.span, Type::Integer, &self.ty())),
        }
    }
    fn as_cell(&self) -> Result<LangCell> {
        match &self.node {
            RtVal::Cell(x) => Ok(*x),
            _ => Err(Error::type_error(self.span, Type::Cell, &self.ty())),
        }
    }
    fn as_tag(&self) -> Result<Arc<String>> {
        match &self.node {
            RtVal::Tag(x) => Ok(Arc::clone(x)),
            _ => Err(Error::type_error(self.span, Type::Tag, &self.ty())),
        }
    }
    fn as_string(&self) -> Result<Arc<String>> {
        match &self.node {
            RtVal::String(x) => Ok(Arc::clone(x)),
            _ => Err(Error::type_error(self.span, Type::String, &self.ty())),
        }
    }
    fn as_type(&self) -> Result<Type> {
        match &self.node {
            RtVal::Type(x) => Ok(x.clone()),
            _ => Err(Error::type_error(self.span, Type::Type, &self.ty())),
        }
    }
    fn as_vector(&self) -> Result<Vec<LangInt>> {
        match &self.node {
            RtVal::Vector(x) => Ok(x.clone()),
            _ => Err(Error::type_error(self.span, Type::Vector(None), &self.ty())),
        }
    }
    fn as_cell_array(&self) -> Result<Arc<CellArray>> {
        match &self.node {
            RtVal::CellArray(x) => Ok(Arc::clone(x)),
            _ => Err(Error::type_error(
                self.span,
                Type::CellArray(None),
                &self.ty(),
            )),
        }
    }
    fn as_empty_set(&self) -> Result<()> {
        match &self.node {
            RtVal::EmptySet => Ok(()),
            _ => Err(Error::type_error(self.span, Type::EmptySet, &self.ty())),
        }
    }
    fn as_integer_set(&self) -> Result<Arc<IntegerSet>> {
        match &self.node {
            RtVal::EmptySet => Ok(Arc::new(IntegerSet::empty())),
            RtVal::IntegerSet(x) => Ok(Arc::clone(x)),
            _ => Err(Error::type_error(self.span, Type::IntegerSet, &self.ty())),
        }
    }
    fn as_cell_set(&self) -> Result<CellSet> {
        match &self.node {
            RtVal::Cell(x) => Ok(CellSet::single_cell(*x)),
            RtVal::Tag(x) => Err(Error::unimplemented(self.span)), // TODO: only allow in compiled code? should allow everywhere
            RtVal::EmptySet => Ok(CellSet::empty()),
            RtVal::CellSet(x) => Ok(*x),
            _ => Err(Error::type_error(self.span, Type::CellSet, &self.ty())),
        }
    }
    fn as_vector_set(&self, vec_len: usize) -> Result<Arc<VectorSet>> {
        match &self.node {
            RtVal::EmptySet => match VectorSet::empty(self.span, vec_len) {
                Ok(set) => Ok(Arc::new(set)),
                Err(_) => internal_error!("invalid vector length for vector set"),
            },
            RtVal::VectorSet(x) => Ok(Arc::clone(x)),
            _ => Err(Error::type_error(
                self.span,
                Type::VectorSet(None),
                &self.ty(),
            )),
        }
    }
    fn as_pattern_matcher(&self) -> Result<Arc<PatternMatcher>> {
        match &self.node {
            RtVal::PatternMatcher(x) => Ok(Arc::clone(x)),
            _ => Err(Error::type_error(
                self.span,
                Type::PatternMatcher(None),
                &self.ty(),
            )),
        }
    }
    fn as_regex(&self) -> Result<Arc<Regex>> {
        match &self.node {
            RtVal::Regex(x) => Ok(Arc::clone(x)),
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
    fn to_vector_set(&self, span: Span, vec_len: usize) -> Result<Arc<VectorSet>> {
        self.as_vector_set(vec_len)?
            .convert_to_vec_len(span, vec_len)
    }
    fn select_cell(&self) -> Result<LangCell> {
        match &self.node {
            RtVal::Cell(c) => Ok(*c),
            RtVal::Tag(t) => todo!("select cell from tag"),
            RtVal::CellSet(set) => todo!("select cell from cell set"),
            _ => Err(Error::type_error(
                self.span,
                "Cell or nonempty CellSet",
                &self.ty(),
            )),
        }
    }

    fn iterate<'a>(&'a self) -> Result<Box<dyn 'a + Iterator<Item = (Option<RtVal>, RtVal)>>> {
        match &self.node {
            RtVal::Tag(_) => Err(Error::unimplemented(self.span)),
            RtVal::String(s) => Ok(Box::new(s.chars().enumerate().map(|(i, c)| {
                (
                    Some(RtVal::Integer(i as LangInt)),
                    RtVal::String(Arc::new(c.to_string())),
                )
            }))),

            RtVal::Vector(v) => {
                Ok(Box::new(v.iter().enumerate().map(|(i, &x)| {
                    (Some(RtVal::Integer(i as LangInt)), RtVal::Integer(x))
                })))
            }
            RtVal::CellArray(a) => {
                Ok(Box::new(a.shape().iter().zip(a.cells_iter()).map(
                    |(pos, c)| (Some(RtVal::Vector(pos)), RtVal::Cell(c)),
                )))
            }

            RtVal::EmptySet => Ok(Box::new(std::iter::empty())),
            RtVal::IntegerSet(set) => Ok(Box::new(set.iter().map(|x| (None, RtVal::Integer(x))))),
            RtVal::CellSet(set) => Err(Error::unimplemented(self.span)),
            RtVal::VectorSet(set) => Ok(Box::new(set.iter().map(|x| (None, RtVal::Vector(x))))),

            _ => Err(Error::iterate_type_error(self.span, &self.ty())),
        }
    }
}
