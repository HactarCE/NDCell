use super::{Array, CellSet, IntegerSet, LangCellState, LangInt, Pattern, Type, VectorSet};
use crate::Result;
use std::rc::Rc;

/// Declares an item with a doc attribute computed by some macro expression.
/// This allows documentation to be dynamically generated based on input.
/// Necessary to work around https://github.com/rust-lang/rust/issues/52607.
///
/// Source: https://github.com/rust-lang/rust/issues/52607#issuecomment-514043687

macro_rules! generate_type_unwrap_methods {
    ($(
        pub fn $name:ident -> Result<$ret_type:ty> { Self::$variant:ident }
    )*) => {
        paste! { $(
            #[doc = "Returns the value inside if this is a `"
                    $variant
                    "`; otherwise returns an internal error."]
            pub fn $name(self) -> Result<$ret_type> {
                match self {
                    Self::$variant(x) => Ok(x),
                    _ => uncaught_type_error!(in $name),
                }
            }
        )* }
    };
}

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// Integer.
    Integer(LangInt),
    /// Cell state.
    Cell(LangCellState),
    /// Integer vector.
    Vector(Vec<LangInt>),
    /// Masked N-dimensional array of cells.
    Array(Array),

    /// Set of integers.
    IntegerSet(IntegerSet),
    /// Set of cell states.
    CellSet(CellSet),
    /// Set of integer vectors.
    VectorSet(VectorSet),
    /// Predicate that accepts or rejects a cell array of a specific
    /// size and shape.
    Pattern(Pattern),

    /// Unicode string.
    String(Rc<String>),

    /// Type value.
    Type(Rc<Type>),
}
impl Value {
    /// Returns the type of the value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Integer(_) => Type::Integer,
            Self::Cell(_) => Type::Cell,
            Self::Vector(v) => Type::Vector(v.len()),
            Self::Array(a) => Type::Array(a.shape().clone()),

            Self::IntegerSet(set) => Type::IntegerSet(set.clone()),
            Self::CellSet(set) => Type::CellSet(set.state_count()),
            Self::VectorSet(set) => Type::VectorSet(set.clone()),
            Self::Pattern(pat) => Type::Pattern(pat.clone()),

            Self::String(s) => Type::String(Rc::clone(&s)),

            Self::Type(t) => Type::Type(t.clone()),
        }
    }

    /// Returns the default value of a type.
    pub fn default(ty: &Type) -> Self {
        match ty {
            Type::Integer => Self::Integer(0),
            Type::Cell => Self::Cell(0),
            Type::Vector(len) => Self::Vector(vec![0; *len]),
            // Default array is all #0.
            Type::Array(shape) => Self::Array(Array::zeros(shape.clone())),

            Type::IntegerSet(set) => Self::IntegerSet(set.clone()),
            // Default cell set is empty.
            Type::CellSet(state_count) => Self::CellSet(CellSet::empty(*state_count)),
            Type::VectorSet(set) => Self::VectorSet(set.clone()),
            Type::Pattern(pat) => Self::Pattern(pat.clone()),

            Type::String(s) => Self::String(Rc::clone(&s)),

            Type::Type(t) => Self::Type(t.clone()),
        }
    }

    generate_type_unwrap_methods!(
        pub fn as_integer -> Result<LangInt> { Self::Integer }
        pub fn as_cell -> Result<LangCellState> { Self::Cell }
        pub fn as_vector -> Result<Vec<LangInt>> { Self::Vector }
        pub fn as_array -> Result<Array> { Self::Array }

        pub fn as_integer_set -> Result<IntegerSet> { Self::IntegerSet }
        pub fn as_cell_set -> Result<CellSet> { Self::CellSet }
        pub fn as_vector_set -> Result<VectorSet> { Self::VectorSet }
        pub fn as_pattern -> Result<Pattern> { Self::Pattern }

        pub fn as_string -> Result<Rc<String>> { Self::String }
        pub fn as_type -> Result<Rc<Type>> { Self::Type }
    );

    /// Converts this value to a boolean if it can be converted; otherwise
    /// returns an internal error.
    pub fn to_bool(self) -> Result<bool> {
        match self {
            Self::Integer(i) => Ok(i != 0),
            Self::Cell(i) => Ok(i != 0),
            Self::Vector(v) => Ok(v.into_iter().any(|i| i != 0)),
            Self::Array(a) => Ok(a.any_nonzero()),

            Self::IntegerSet(_) | Self::CellSet(_) | Self::VectorSet(_) | Self::Pattern(_) => {
                uncaught_type_error!(in to_bool)
            }

            Self::String(s) => Ok(!s.is_empty()),

            Self::Type(_) => uncaught_type_error!(in to_bool),
        }
    }
}
