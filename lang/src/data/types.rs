//! Data types of NDCA.
//!
//! When adding a new type, these modifications are necessary:
//!
//! - Add new variant to `Type`
//! - Add new variant to `TypeClass` if necessary
//! - Add new token to `Token` enum
//! - Update `TryFrom<Token>` impl for `TypeClass`
//! - Add new variant to `eval::Value`
//! - Add new variant to `compile::Value`
//! - Add new method in `generate_type_unwrap_methods!`

use codemap::Spanned;
use itertools::Itertools;
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;

use super::{IntegerSet, Pattern, VectorSet};
use crate::errors::Error;
use crate::lexer::Token;

/// Rust type used for NDCA integers.
pub type LangInt = i64;
/// Unsigned rust type used for NDCA integers.
pub type LangUint = u64;
/// Number of bits in an NDCA integer.
pub const INT_BITS: u32 = 64;
/// Number of bytes in an NDCA integer.
pub const INT_BYTES: usize = INT_BITS as usize / 8;

/// Rust type used for an NDCA cell state.
pub type LangCellState = u8;
/// Number of bits in an NDCA cell state.
pub const CELL_STATE_BITS: u32 = 8;

/// Specific data type.
///
/// When adding new types, make sure that check `lexer::TypeToken` and add a
/// corresponding variant there if needed.
#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    /// Integer.
    Integer,
    /// Cell state.
    Cell,
    /// Integer vector of a specific length (from 1 to 256).
    Vector(usize),
    /// Masked N-dimensional array of cells with a specific size and shape.
    Array(VectorSet),

    /// Specific set of integers.
    IntegerSet(IntegerSet),
    /// Set of cell states in a CA with specific number of cell states.
    CellSet(usize),
    /// Specific set of integer vectors of a specific length.
    VectorSet(VectorSet),
    /// Specific predicate that accepts or rejects a cell array of a specific
    /// size and shape.
    Pattern(Pattern),

    /// Specific Unicode string.
    String(Rc<String>),

    /// Specific type value.
    Type(Rc<Type>),
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Integer"),
            Type::Cell => write!(f, "Cell"),
            Type::Vector(len) => write!(f, "Vector[{:?}]", len),
            Type::Array(shape) => write!(f, "Array[{:?}]", shape),

            Type::IntegerSet(set) => write!(f, "IntegerSet[{:?}]", set),
            Type::CellSet(state_count) => write!(f, "CellSet[{:?}]", state_count),
            Type::VectorSet(set) => write!(f, "VectorSet[{:?}]", set),
            Type::Pattern(pat) => write!(f, "Pattern[{:?}]", pat),

            Type::String(s) => write!(f, "String[{:?}]", s),

            Type::Type(t) => write!(f, "Type[{}]", t),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Integer"),
            Type::Cell => write!(f, "Cell"),
            Type::Vector(len) => write!(f, "Vector[{}]", len),
            Type::Array(shape) => write!(f, "Array[{}]", shape),

            Type::IntegerSet(set) => write!(f, "IntegerSet[{}]", set),
            Type::CellSet(_state_count) => write!(f, "CellSet"),
            Type::VectorSet(set) => write!(f, "VectorSet[{}]", set.vec_len()),
            Type::Pattern(pat) => write!(f, "Pattern[{}]", pat.shape()),

            Type::String(s) => write!(f, "String[{:?}]", s),

            Type::Type(t) => write!(f, "Type[{:?}]", t),
        }
    }
}
impl Type {
    /// Returns true if variables of this type can be reassigned, or false if
    /// they can only be assigned at their declaration.
    pub fn vars_can_be_reassigned(&self) -> bool {
        match self {
            Type::Integer | Type::Cell | Type::Vector(_) | Type::Array(_) => true,

            Type::IntegerSet(_) => false,
            Type::CellSet(_) => true,
            Type::VectorSet(_) => false,
            Type::Pattern(_) => false,

            Type::String(_) => false,

            Type::Type(_) => false,
        }
    }

    /// Returns true if this type can be converted to a boolean, or false
    /// otherwise.
    ///
    /// When updating this, make sure to also update ConstValue::to_bool() and
    /// Compiler::build_convert_to_bool()
    pub fn can_convert_to_bool(&self) -> bool {
        match self {
            Type::Integer | Type::Cell | Type::Vector(_) | Type::Array(_) => true,

            Type::IntegerSet(_) | Type::CellSet(_) | Type::VectorSet(_) | Type::Pattern(_) => false,

            Type::String(_) => false,

            Type::Type(_) => false,
        }
    }
    /// Returns the type yielded by iteration over this type, or None if this
    /// type cannot be iterated over.
    pub fn iteration_type(&self) -> Option<Type> {
        match self {
            Type::Integer | Type::Cell | Type::Vector(_) => None,
            Type::Array(_) => Some(Type::Cell),

            Type::IntegerSet(_) => Some(Type::Integer),
            Type::CellSet(_) => Some(Type::Cell),
            Type::VectorSet(set) => Some(Type::Vector(set.vec_len())),
            Type::Pattern(_) => None,

            Type::String(_) => None, // Iteration over characters may be supported in the future.

            Type::Type(_) => None,
        }
    }
}

pub trait SpannedTypeExt {
    /// Returns a type error if this type does not match the given expected
    /// type(s).
    fn typecheck(&self, expected: impl TypeChecker) -> crate::Result<()>;

    /// Returns a CustomTypeError if this type cannot be converted to a boolean.
    fn typecheck_can_convert_to_bool(&self) -> crate::Result<()>;

    /// Returns the type yielded by iterating over this type, or a
    /// CustomTypeError if this type cannot be iterated over.
    fn typecheck_can_iterate(&self) -> crate::Result<Type>;
}

impl SpannedTypeExt for Spanned<Type> {
    fn typecheck(&self, expected: impl TypeChecker) -> crate::Result<()> {
        if expected.matches(&self.node) {
            Ok(())
        } else {
            Err(Error::type_error(self.span, expected, &self.node))
        }
    }

    fn typecheck_can_convert_to_bool(&self) -> crate::Result<()> {
        if self.node.can_convert_to_bool() {
            Ok(())
        } else {
            Err(Error::type_error(
                self.span,
                "type that can be converted to boolean",
                &self.node,
            ))
        }
    }

    fn typecheck_can_iterate(&self) -> crate::Result<Type> {
        self.node.iteration_type().ok_or_else(|| {
            Error::type_error(self.span, "type that can be iterated over", &self.node)
        })
    }
}

/// General type class (e.g. vector with unspecified length).
#[derive(Clone, PartialEq, Eq)]
pub enum TypeClass {
    /// Concrete type.
    Specific(Type),
    /// Integer vector of any length.
    Vector,
    /// Configuration of cells with any size and shape.
    Array,

    /// Set of integers.
    IntegerSet,
    /// Set of cell states in a CA with a specific number of states.
    CellSet,
    /// Set of integer vectors of any length or a specific length.
    VectorSet(Option<usize>),
    /// Predicate that accepts or rejects a cell array of any shape or of a
    /// specific size and shape.
    Pattern(Option<VectorSet>),

    /// String.
    String,

    /// Type value, either any or one fitting a particular type class.
    Type(Option<Rc<TypeClass>>),
}
impl From<Type> for TypeClass {
    fn from(ty: Type) -> Self {
        Self::Specific(ty)
    }
}
impl fmt::Debug for TypeClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(ty) => write!(f, "{:?}", ty),
            Self::Vector => write!(f, "Vector"),
            Self::Array => write!(f, "Array"),

            Self::IntegerSet => write!(f, "IntegerSet"),
            Self::CellSet => write!(f, "CellSet"),
            Self::VectorSet(Some(len)) => write!(f, "VectorSet[{:?}]", len),
            Self::VectorSet(None) => write!(f, "VectorSet"),
            Self::Pattern(Some(shape)) => write!(f, "Pattern[{:?}]", shape),
            Self::Pattern(None) => write!(f, "Pattern"),

            Self::String => write!(f, "String"),

            Self::Type(Some(t)) => write!(f, "Type[{:?}]", t),
            Self::Type(None) => write!(f, "Type"),
        }
    }
}
impl fmt::Display for TypeClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(ty) => write!(f, "{}", ty),
            Self::Vector => write!(f, "Vector"),
            Self::Array => write!(f, "Array"),

            Self::IntegerSet => write!(f, "IntegerSet"),
            Self::CellSet => write!(f, "CellSet"),
            Self::VectorSet(Some(len)) => write!(f, "VectorSet[{}]", len),
            Self::VectorSet(None) => write!(f, "VectorSet"),
            Self::Pattern(Some(shape)) => write!(f, "Pattern[{}]", shape),
            Self::Pattern(None) => write!(f, "Pattern"),

            Self::String => write!(f, "String"),

            Self::Type(Some(t)) => write!(f, "Type[{}]", t),
            Self::Type(None) => write!(f, "Type"),
        }
    }
}
impl TryFrom<Token> for TypeClass {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::KeywordInteger => Ok(Type::Integer.into()),
            Token::KeywordVector => Ok(TypeClass::Vector),
            Token::KeywordCell => Ok(Type::Cell.into()),
            Token::KeywordArray => Ok(TypeClass::Array),
            Token::KeywordIntegerSet => Ok(TypeClass::IntegerSet),
            Token::KeywordVectorSet => Ok(TypeClass::VectorSet(None)),
            Token::KeywordCellSet => Ok(TypeClass::CellSet),
            Token::KeywordPattern => Ok(TypeClass::Pattern(None)),
            Token::KeywordString => Ok(TypeClass::String),
            Token::KeywordType => Ok(TypeClass::Type(None)),
            // Token::KeywordTag => Type::Tag.into(),
            _ => Err(()),
        }
    }
}

/// Rust types that can be used to check NDCA types.
pub trait TypeChecker: fmt::Display {
    /// Returns true if the given type matches this type checker, or false if it
    /// does not.
    fn matches(&self, ty: &Type) -> bool;
}
impl TypeChecker for Type {
    fn matches(&self, ty: &Type) -> bool {
        self == ty
    }
}
impl TypeChecker for TypeClass {
    fn matches(&self, ty: &Type) -> bool {
        match self {
            TypeClass::Specific(this) => this.matches(ty),
            TypeClass::Vector => matches!(ty, Type::Vector(_)),
            TypeClass::Array => matches!(ty, Type::Array(_)),

            TypeClass::IntegerSet => matches!(ty, Type::IntegerSet(_)),
            TypeClass::CellSet => matches!(ty, Type::CellSet(_)),
            TypeClass::VectorSet(Some(len)) => match ty {
                Type::VectorSet(set) => set.vec_len() == *len,
                _ => false,
            },
            TypeClass::VectorSet(None) => matches!(ty, Type::VectorSet(_)),
            TypeClass::Pattern(Some(shape)) => match ty {
                Type::Pattern(pat) => pat.shape() == shape,
                _ => false,
            },
            TypeClass::Pattern(None) => matches!(ty, Type::Pattern(_)),

            TypeClass::String => matches!(ty, Type::String(_)),

            TypeClass::Type(Some(t1)) => matches!(ty, Type::Type(t2) if t1.matches(t2)),
            TypeClass::Type(None) => matches!(ty, Type::Type(_)),
        }
    }
}
impl<T: TypeChecker> TypeChecker for &T {
    fn matches(&self, ty: &Type) -> bool {
        (*self).matches(ty)
    }
}

pub struct AnyTypeChecker<'a, T>(&'a [T]);
impl<T: fmt::Display> fmt::Display for AnyTypeChecker<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            crate::utils::join_with_conjunction(
                "or",
                &self.0.iter().map(ToString::to_string).collect_vec(),
            )
        )
    }
}
impl<T: TypeChecker> AnyTypeChecker<'_, T> {
    fn matches(&self, ty: &Type) -> bool {
        self.0.iter().any(|expected| expected.matches(ty))
    }
}

/// Check that the given value matches any one of the given types, returning
/// Ok(()) if it does match and a type error if it does not match.
macro_rules! typecheck {
    ( $got:expr, [ $( $expected:tt ),+ $(,)? ] ) => {
        $got.typecheck(AnyTypeChecker(&[ $( typeclass!($expected) ),+ ]))
    };
    ( $got:expr, $expected:tt $(,)? ) => {
        $got.typecheck(typeclass!($expected))
    };
}

/// Convert type names (such as "Int") or type description name (such as
/// "Vector" or "Pattern") into a TypeClass.
macro_rules! typeclass {
    ( Vector ) => {
        crate::data::TypeClass::Vector
    };
    ( Array ) => {
        crate::data::TypeClass::Array
    };
    ( IntegerSet ) => {
        crate::data::TypeClass::IntegerSet
    };
    ( CellSet ) => {
        crate::data::TypeClass::CellSet
    };
    ( VectorSet(Option<usize>) ) => {
        crate::data::TypeClass::VectorSet(Option<usize>)
    };
    ( Pattern(Option<VectorSet>) ) => {
        crate::data::TypeClass::Pattern(Option<VectorSet>)
    };
    ( $other:tt $($args:tt)? ) => {
        crate::data::TypeClass::Specific(crate::data::Type::$other $($args)?)
    };
}

/// Function signature, consisting of types for the arguments and a return
/// type.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FnSignature {
    pub args: Vec<Type>,
    pub ret: Option<Type>,
}
impl FnSignature {
    /// Constructs the signature for a function that takes no arguments.
    pub fn atom(ret: Option<Type>) -> Self {
        Self::new(vec![], ret)
    }
    /// Constructs the signature for a function that takes one argument and
    /// returns a property of that argument.
    pub fn property(self_type: Type, ret: Option<Type>) -> Self {
        Self::new(vec![self_type], ret)
    }
    /// Constructs any arbitrary function signature.
    pub fn new(args: Vec<Type>, ret: Option<Type>) -> Self {
        let args = args.into();
        Self { args, ret }
    }

    /// Returns true if the given argument types match the arguments of this
    /// function signature.
    pub fn matches(&self, arg_types: &[Spanned<Type>]) -> bool {
        arg_types.iter().map(|s| &s.node).eq(&self.args)
    }
}
