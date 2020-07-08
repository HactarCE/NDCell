//! Types used by NDCA.

use itertools::Itertools;
use std::fmt;
use std::rc::Rc;

mod filters;
mod patterns;

pub use filters::{CellStateFilter, CELL_STATE_FILTER_ARRAY_LEN};
pub use patterns::PatternShape;

use crate::errors::*;
use crate::Spanned;
use LangErrorMsg::{CustomTypeError, TypeError};

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Unsigned rust type used for NDCA integers.
pub type LangUint = u64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;

/// Rust type used for an NDCA cell state.
pub type LangCellState = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 8;

/// Maximum length for a vector.
pub const MAX_VECTOR_LEN: usize = 256;

/// Axis names.
pub const AXES: &'static str = "xyzwuv";

/// Any data type.
///
/// When adding new types, make sure that check lexer::TypeToken and add a
/// corresponding variant there if needed.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Void.
    Void,
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
    /// Cell state filter
    CellStateFilter,
}
impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Int"),
            Self::CellState => write!(f, "Cell"),
            Self::Vector(len) => write!(f, "{:?}{}", TypeDesc::Vector, len),
            Self::Pattern(shape) => write!(f, "{:?}{}", TypeDesc::Pattern, shape),
            Self::IntRange => write!(f, "Range"),
            Self::Rectangle(ndim) => write!(f, "{:?}{}", TypeDesc::Rectangle, ndim),
            Self::CellStateFilter => write!(f, "CellFilter"),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Integer"),
            Self::CellState => write!(f, "CellState"),
            Self::Vector(len) => write!(f, "{}{}", TypeDesc::Vector, len),
            Self::Pattern(shape) => write!(f, "{}{}", TypeDesc::Pattern, shape),
            Self::IntRange => write!(f, "Range"),
            Self::Rectangle(ndim) => write!(f, "{}{}", TypeDesc::Rectangle, ndim),
            Self::CellStateFilter => write!(f, "CellStateFilter"),
        }
    }
}
impl Type {
    /// Returns true if this type has a representation in compiled code, or
    /// false otherwise; i.e. whether a variable can contain a value of this
    /// type.
    pub fn has_runtime_representation(&self) -> bool {
        match self {
            Self::Void
            | Self::Int
            | Self::CellState
            | Self::Vector(_)
            | Self::Pattern(_)
            | Self::IntRange
            | Self::Rectangle(_)
            | Self::CellStateFilter => true,
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
            Self::Void | Self::IntRange | Self::Rectangle(_) | Self::CellStateFilter => false,
        }
    }

    /// Returns a TypeError where this type is the "got" type, given an
    /// "expected" type.
    pub fn type_error(&self, expected: impl Into<TypeDesc>) -> LangErrorMsg {
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
    /// Returns a type error if this type does not match the given expected
    /// type(s).
    pub fn typecheck(&self, expected: impl TypeChecker) -> LangResult<()> {
        if expected.matches(&self.inner) {
            Ok(())
        } else {
            Err(expected.type_error(self.inner.clone()).with_span(self.span))
        }
    }

    /// Returns a CustomTypeError if this type cannot be converted to a boolean.
    pub fn typecheck_can_convert_to_bool(&self) -> LangResult<()> {
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

/// Vague type description (e.g. vector with unspecified length).
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeDesc {
    /// A specific type.
    Specific(Type),
    /// Vector of any length.
    Vector,
    /// Configuration of cells of any size and shape.
    Pattern,
    /// Hyperrectangle of any dimensionality.
    Rectangle,
}
impl From<Type> for TypeDesc {
    fn from(ty: Type) -> Self {
        Self::Specific(ty)
    }
}
impl fmt::Debug for TypeDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(ty) => write!(f, "{:?}", ty),
            Self::Vector => write!(f, "Vec"),
            Self::Pattern => write!(f, "Pat"),
            Self::Rectangle => write!(f, "Rect"),
        }
    }
}
impl fmt::Display for TypeDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(ty) => write!(f, "{}", ty),
            Self::Vector => write!(f, "Vector"),
            Self::Pattern => write!(f, "Pattern"),
            Self::Rectangle => write!(f, "Rectangle"),
        }
    }
}

/// Rust types that can be used to check NDCA types.
pub trait TypeChecker {
    /// Returns true if the given type matches this type checker, or false
    /// if it does not.
    fn matches(&self, ty: &Type) -> bool;
    /// Returns a type error using the given "got" type.
    fn type_error(&self, got: Type) -> LangErrorMsg;
}
impl TypeChecker for Type {
    fn matches(&self, ty: &Type) -> bool {
        self == ty
    }
    fn type_error(&self, got: Type) -> LangErrorMsg {
        let expected = TypeDesc::from(self.clone());
        TypeError { expected, got }
    }
}
impl TypeChecker for TypeDesc {
    fn matches(&self, ty: &Type) -> bool {
        match self {
            TypeDesc::Specific(this) => this.matches(ty),
            TypeDesc::Vector => matches!(ty, Type::Vector(_)),
            TypeDesc::Pattern => matches!(ty, Type::Pattern(_)),
            TypeDesc::Rectangle => matches!(ty, Type::Rectangle(_)),
        }
    }
    fn type_error(&self, got: Type) -> LangErrorMsg {
        let expected = self.clone();
        TypeError { expected, got }
    }
}
impl<T: TypeChecker + ToString> TypeChecker for &[T] {
    fn matches(&self, ty: &Type) -> bool {
        self.iter().any(|expected| expected.matches(ty))
    }
    fn type_error(&self, got: Type) -> LangErrorMsg {
        let expected = crate::utils::join_with_conjunction(
            "or",
            &self.iter().map(ToString::to_string).collect_vec(),
        );
        CustomTypeError { expected, got }
    }
}
impl<T: TypeChecker> TypeChecker for &T {
    fn matches(&self, ty: &Type) -> bool {
        (*self).matches(ty)
    }
    fn type_error(&self, got: Type) -> LangErrorMsg {
        (*self).type_error(got)
    }
}

/// Check that the given value matches any one of the given types, returning
/// Ok(()) if it does match and a type error if it does not match.
macro_rules! typecheck {
    ( $got:expr, [ $( $expected:tt ),+ $(,)? ] ) => {
        $got.typecheck([ $( get_type_desc!($expected) ),+ ].as_ref())
    };
    ( $got:expr, $expected:tt ) => {
        $got.typecheck(get_type_desc!($expected))
    };
}

/// Convert type names (such as "Int") or type description name (such as
/// "Vector" or "Pattern") into a TypeDesc.
macro_rules! get_type_desc {
    ( Vector ) => {
        crate::types::TypeDesc::Vector
    };
    ( Pattern ) => {
        crate::types::TypeDesc::Pattern
    };
    ( Rectangle ) => {
        crate::types::TypeDesc::Rectangle
    };
    ( $other:tt ) => {
        crate::types::TypeDesc::Specific(crate::types::Type::$other)
    };
}
