//! Values used by the interpreter for NDCA.

use crate::errors::*;
use crate::types::{LangCellState, LangInt, Type};

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    /// Integer.
    Int(LangInt),
    /// Cell state.
    CellState(LangCellState),
    /// Vector of a specific length from 1 to 256.
    Vector(Vec<LangInt>),
    /// Inclusive integer range.
    IntRange {
        /// First number in the range.
        start: LangInt,
        /// Number to end at or before (inclusive).
        end: LangInt,
        /// Step (may be negative).
        step: LangInt,
    },
    /// Inclusive hyperrectangle, represented by the coordinates of two opposite corners.
    Rectangle(Vec<LangInt>, Vec<LangInt>),
}
impl ConstValue {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(values) => Type::Vector(values.len()),
            Self::IntRange { .. } => Type::IntRange,
            Self::Rectangle(start, _) => Type::Rectangle(start.len()),
        }
    }
    /// Constructs a default value of the given type.
    pub fn default(ty: &Type) -> Option<Self> {
        match ty {
            Type::Int => Some(Self::Int(0)),
            Type::CellState => Some(Self::CellState(0)),
            Type::Vector(len) => Some(Self::Vector(vec![0; *len])),
            // TODO: default pattern (all #0)
            Type::Pattern(_) => None,
            // Default integer range includes only zero.
            Type::IntRange => Some(Self::IntRange {
                start: 0,
                end: 0,
                step: 1,
            }),
            // Default rectangle includes only the origin.
            Type::Rectangle(ndim) => Some(Self::Rectangle(vec![0; *ndim], vec![0; *ndim])),
        }
    }

    /// Returns the integer value inside if this is a ConstValue::Int; otherwise
    /// returns an InternalError.
    pub fn as_int(self) -> LangResult<LangInt> {
        match self {
            Self::Int(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::CellState;
    /// otherwise returns an InternalError.
    pub fn as_cell_state(self) -> LangResult<LangCellState> {
        match self {
            Self::CellState(i) => Ok(i),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the vector value inside if this is a ConstValue::Vector;
    /// otherwise returns an InternalError.
    pub fn as_vector(self) -> LangResult<Vec<LangInt>> {
        match self {
            Self::Vector(v) => Ok(v),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Returns the start, end, and step inside if this is a
    /// ConstValue::IntRange; otherwise returns an InternalError.
    pub fn as_int_range(self) -> LangResult<(LangInt, LangInt, LangInt)> {
        match self {
            Self::IntRange { start, end, step } => Ok((start, end, step)),
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }

    /// Converts this value to a boolean if it can be converted; otherwise
    /// returns an InternalError.
    pub fn to_bool(self) -> LangResult<bool> {
        match self {
            Self::Int(i) => Ok(i != 0),
            Self::CellState(i) => Ok(i != 0),
            Self::Vector(v) => Ok(v.into_iter().any(|i| i != 0)),
            Self::IntRange { .. } | Self::Rectangle { .. } => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
    /// Converts this value to a vector of the specified length if this is a
    /// ConstValue::Int or ConstValue::Vector; otherwise returns an
    /// InternalError.
    pub fn coerce_to_vector(self, len: usize) -> LangResult<Vec<LangInt>> {
        match self {
            Self::Int(i) => Ok(vec![i; len]),
            Self::Vector(mut v) => {
                if v.len() < len {
                    // Not long enough; extend with zeros.
                    v.extend(std::iter::repeat(0).take(len - v.len()));
                } else if v.len() > len {
                    // Too long; truncate.
                    v.truncate(len);
                }
                Ok(v)
            }
            _ => Err(UNCAUGHT_TYPE_ERROR),
        }
    }
}
