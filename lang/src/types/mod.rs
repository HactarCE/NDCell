//! Types used by NDCA.

use std::fmt;
use std::rc::Rc;

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;

/// Rust type used for an NDCA cell state.
pub type LangCellState = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 8;

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;

mod patterns;

pub use patterns::PatternShape;

use crate::errors::*;
use crate::Spanned;
use LangErrorMsg::{CustomTypeError, TypeError};

/// Any data type.
///
/// When adding new types, make sure that check lexer::TypeToken and add a
/// corresponding variant there if needed. Also update the list in the error
/// message in parser::ParseBuilder::type_name().
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Integer.
    Int,
    /// Cell state.
    CellState,
    /// Vector of a specific length (from 1 to 256).
    Vector(usize),
    /// Configuration of cells of a specific size and shape.
    Pattern(Rc<PatternShape>),
    /// Contiguous range of integers.
    IntRange,
    /// Hyperrectangle of a specific dimensionality (from 1 to 256).
    Rectangle(usize),
}
impl Default for Type {
    fn default() -> Self {
        Self::Int
    }
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::CellState => write!(f, "Cell"),
            Self::Vector(len) => write!(f, "{:?}{}", VagueType::Vector, len),
            Self::Pattern(shape) => write!(f, "{:?}{}", VagueType::Pattern, shape),
            Self::IntRange => write!(f, "Range"),
            Self::Rectangle(ndim) => write!(f, "{:?}{}", VagueType::Rectangle, ndim),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Integer"),
            Self::CellState => write!(f, "CellState"),
            Self::Vector(len) => write!(f, "{}{}", VagueType::Vector, len),
            Self::Pattern(shape) => write!(f, "{}{}", VagueType::Pattern, shape),
            Self::IntRange => write!(f, "Range"),
            Self::Rectangle(ndim) => write!(f, "{}{}", VagueType::Rectangle, ndim),
        }
    }
}
impl Type {
    /// Returns true if this type has a representation in compiled code, or
    /// false otherwise; i.e. whether a variable can contain a value of this
    /// type.
    pub fn has_runtime_representation(&self) -> bool {
        match self {
            Self::Int
            | Self::CellState
            | Self::Vector(_)
            | Self::Pattern(_)
            | Self::IntRange
            | Self::Rectangle(_) => true,
        }
    }

    /// Returns true if this type can bde converted to a boolean, or false
    /// otherwise.
    ///
    /// When updating this, make sure to also update ConstValue::to_bool() and
    /// Compiler::build_convert_to_bool()
    pub fn can_convert_to_bool(&self) -> bool {
        match self {
            Self::Int | Self::CellState | Self::Vector(_) | Self::Pattern(_) => true,
            Self::IntRange | Self::Rectangle(_) => false,
        }
    }

    /// Returns a TypeError where this type is the "got" type, given an
    /// "expected" type.
    pub fn type_error(&self, expected: impl Into<VagueType>) -> LangErrorMsg {
        TypeError {
            expected: expected.into(),
            got: self.clone(),
        }
    }
    /// Returns a CustomTypeError where this type is the "got" type, given an
    /// "expected" message.
    pub fn custom_type_error(&self, expected: String) -> LangErrorMsg {
        CustomTypeError {
            expected,
            got: self.clone(),
        }
    }
    /// Returns a CustomTypeError where this type is the "got" type, given a
    /// list of "expected" types.
    pub fn multi_type_error(&self, expected: &[impl fmt::Display]) -> LangErrorMsg {
        self.custom_type_error(crate::utils::join_with_conjunction("or", expected))
    }
}

impl Spanned<Type> {
    /// Returns a TypeError if this type does not match the given expected type.
    pub fn check_eq(&self, expected: Type) -> LangResult<()> {
        if self.inner == expected {
            Ok(())
        } else {
            Err(self.inner.type_error(expected).with_span(self.span))
        }
    }
    /// Returns a TypeError if this type is not a vector; otherwise returns the
    /// length of the vector.
    pub fn check_vec(&self) -> LangResult<usize> {
        match self.inner {
            Type::Vector(len) => Ok(len),
            _ => Err(self
                .inner
                .type_error(VagueType::Vector)
                .with_span(self.span)),
        }
    }
    /// Returns a CustomTypeError if this type is not an integer or vector.
    pub fn check_int_or_vec(&self) -> LangResult<()> {
        match self.inner {
            Type::Int | Type::Vector(_) => Ok(()),
            _ => Err(self
                .inner
                .multi_type_error(&[Type::Int.into(), VagueType::Vector])
                .with_span(self.span)),
        }
    }
    /// Returns a CustomTypeError if this type is not an integer, vector, or
    /// range of integers.
    pub fn check_int_or_vec_or_range(&self) -> LangResult<()> {
        match self.inner {
            Type::Int | Type::Vector(_) | Type::IntRange => Ok(()),
            _ => Err(self
                .inner
                .multi_type_error(&[Type::Int.into(), VagueType::Vector, Type::IntRange.into()])
                .with_span(self.span)),
        }
    }
    /// Returns a CustomTypeError if this type cannot be converted to a boolean.
    pub fn check_can_convert_to_bool(&self) -> LangResult<()> {
        if self.inner.can_convert_to_bool() {
            Ok(())
        } else {
            Err(self
                .inner
                .custom_type_error("type that can be converted to boolean".to_owned())
                .with_span(self.span))
        }
    }
}

/// More vague type (e.g. vector with unspecified length).
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum VagueType {
    /// A specific type.
    Specific(Type),
    /// Vector of any length.
    Vector,
    /// Configuration of cells of any size and shape.
    Pattern,
    /// Hyperrectangle of any dimensionality.
    Rectangle,
}
impl From<Type> for VagueType {
    fn from(ty: Type) -> Self {
        Self::Specific(ty)
    }
}
impl fmt::Debug for VagueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(ty) => write!(f, "{}", ty),
            Self::Vector => write!(f, "Vec"),
            Self::Pattern => write!(f, "Pat"),
            Self::Rectangle => write!(f, "Rect"),
        }
    }
}
impl fmt::Display for VagueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(ty) => write!(f, "{}", ty),
            Self::Vector => write!(f, "Vector"),
            Self::Pattern => write!(f, "Pattern"),
            Self::Rectangle => write!(f, "Rectangle"),
        }
    }
}
