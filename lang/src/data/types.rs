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

use codemap::Spanned;
use itertools::Itertools;
use std::fmt;
use std::sync::Arc;

use super::VectorSet;
use crate::errors::{Error, Result};

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

pub trait SpannedTypeExt {
    /// Returns a type error if this type does not match the given expected
    /// type(s).
    fn typecheck(&self, expected: impl TypeChecker) -> Result<()>;

    /// Returns a CustomTypeError if this type cannot be converted to a boolean.
    fn typecheck_can_convert_to_bool(&self) -> Result<()>;

    /// Returns the type yielded by iterating over this type, or a
    /// CustomTypeError if this type cannot be iterated over.
    fn typecheck_can_iterate(&self) -> Result<Type>;
}

impl SpannedTypeExt for Spanned<Type> {
    fn typecheck(&self, expected: impl TypeChecker) -> Result<()> {
        if expected.matches(&self.node) {
            Ok(())
        } else {
            Err(Error::type_error(self.span, expected, &self.node))
        }
    }

    fn typecheck_can_convert_to_bool(&self) -> Result<()> {
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

    fn typecheck_can_iterate(&self) -> Result<Type> {
        self.node.iteration_type().ok_or_else(|| {
            Error::type_error(self.span, "type that can be iterated over", &self.node)
        })
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
        match self {
            Type::Vector(None) => matches!(ty, Type::Vector(_)),
            Type::Array(None) => matches!(ty, Type::Array(_)),
            Type::VectorSet(None) => matches!(ty, Type::VectorSet(_)),
            Type::Pattern(None) => matches!(ty, Type::Pattern(_)),
            _ => self == ty,
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
