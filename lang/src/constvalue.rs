//! Values used by the interpreter for NDCA.

use crate::errors::*;
use crate::types::{CellStateFilter, LangCellState, LangInt, Pattern, Stencil, Type};

/// Constant value of any type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    /// Void.
    Void,
    /// Integer.
    Int(LangInt),
    /// Cell state.
    CellState(LangCellState),
    /// Vector of a specific length from 1 to 256.
    Vector(Vec<LangInt>),
    /// Configuration of cells of a specific size and shape.
    Pattern(Pattern),
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
    /// Cell state filter.
    CellStateFilter(CellStateFilter),

    /// Description of a configuration of cells.
    Stencil(Stencil),
}
impl ConstValue {
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::Void => Type::Void,
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
            Self::Vector(values) => Type::Vector(values.len()),
            Self::Pattern(p) => Type::Pattern {
                shape: p.shape.clone(),
                has_lut: p.lut.is_some(),
            },
            Self::IntRange { .. } => Type::IntRange,
            Self::Rectangle(start, end) => {
                assert_eq!(start.len(), end.len());
                Type::Rectangle(start.len())
            }
            Self::CellStateFilter(f) => Type::CellStateFilter(f.state_count()),
            Self::Stencil(_) => Type::Stencil,
        }
    }
    /// Constructs a default value of the given type.
    pub fn default(ty: &Type) -> LangResult<Self> {
        match ty {
            Type::Void => Ok(Self::Void),
            Type::Int => Ok(Self::Int(0)),
            Type::CellState => Ok(Self::CellState(0)),
            Type::Vector(len) => Ok(Self::Vector(vec![0; *len])),
            // Default pattern is all 0.
            Type::Pattern { shape, has_lut } => {
                let lut = if *has_lut { Some(0) } else { None };
                Ok(Self::Pattern(Pattern::zeros(shape.clone(), lut)))
            }
            // Default integer range includes only zero.
            Type::IntRange => Ok(Self::IntRange {
                start: 0,
                end: 0,
                step: 1,
            }),
            // Default rectangle includes only the origin.
            Type::Rectangle(ndim) => Ok(Self::Rectangle(vec![0; *ndim], vec![0; *ndim])),
            // Default cell state filter includes no cells.
            Type::CellStateFilter(state_count) => {
                Ok(Self::CellStateFilter(CellStateFilter::none(*state_count)))
            }
            // There is no default stencil.
            Type::Stencil => internal_error!(NO_RUNTIME_REPRESENTATION),
        }
    }

    /// Returns the integer value inside if this is a ConstValue::Int; otherwise
    /// returns an InternalError.
    pub fn as_int(self) -> LangResult<LangInt> {
        match self {
            Self::Int(i) => Ok(i),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the integer value inside if this is a ConstValue::CellState;
    /// otherwise returns an InternalError.
    pub fn as_cell_state(self) -> LangResult<LangCellState> {
        match self {
            Self::CellState(i) => Ok(i),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the vector value inside if this is a ConstValue::Vector;
    /// otherwise returns an InternalError.
    pub fn as_vector(self) -> LangResult<Vec<LangInt>> {
        match self {
            Self::Vector(v) => Ok(v),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the start, end, and step inside if this is a
    /// ConstValue::IntRange; otherwise returns an InternalError.
    pub fn as_int_range(self) -> LangResult<(LangInt, LangInt, LangInt)> {
        match self {
            Self::IntRange { start, end, step } => Ok((start, end, step)),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the start and end inside if this is a ConstValue::Rectangle;
    /// otherwise returns an InternalError.
    pub fn as_rectangle(self) -> LangResult<(Vec<LangInt>, Vec<LangInt>)> {
        match self {
            Self::Rectangle(start, end) => Ok((start, end)),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the value inside if this is a ConstValue::CellStateFilter;
    /// otherwise returns an InternalError.
    pub fn as_cell_state_filter(self) -> LangResult<CellStateFilter> {
        match self {
            Self::CellStateFilter(f) => Ok(f),
            _ => uncaught_type_error!(),
        }
    }
    /// Returns the value inside if this is a ConstValue::Stencil; otherwise
    /// returns an InternalError.
    pub fn as_stencil(self) -> LangResult<Stencil> {
        match self {
            Self::Stencil(s) => Ok(s),
            _ => uncaught_type_error!(),
        }
    }

    /// Converts this value to a boolean if it can be converted; otherwise
    /// returns an InternalError.
    pub fn to_bool(self) -> LangResult<bool> {
        match self {
            Self::Int(i) => Ok(i != 0),
            Self::CellState(i) => Ok(i != 0),
            Self::Vector(v) => Ok(v.into_iter().any(|i| i != 0)),
            Self::Pattern(p) => Ok(p
                .cells
                .iter()
                .zip(p.shape.flat_mask())
                .any(|(&cell, &mask)| mask && cell != 0)),
            Self::Void
            | Self::IntRange { .. }
            | Self::Rectangle { .. }
            | Self::CellStateFilter(_)
            | Self::Stencil(_) => uncaught_type_error!(),
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
            _ => uncaught_type_error!(),
        }
    }
    /// Converts this value to a rectangle of the specified number of dimensions
    /// if this is a ConstValue::Int, ConstValue::Vector, ConstValue::IntRange,
    /// or ConstValue::Rectangle; otherwise returns an InternalError.
    pub fn coerce_to_rectangle(self, ndim: usize) -> LangResult<(Vec<LangInt>, Vec<LangInt>)> {
        match self {
            Self::Int(i) => Ok((vec![i; ndim], vec![i; ndim])),
            Self::Vector(v) => {
                let pos = Self::Vector(v).coerce_to_vector(ndim)?;
                Ok((pos.clone(), pos))
            }
            Self::IntRange { start, end, .. } => Ok((vec![start; ndim], vec![end; ndim])),
            Self::Rectangle(start, end) => Ok((
                Self::Vector(start).coerce_to_vector(ndim)?,
                Self::Vector(end).coerce_to_vector(ndim)?,
            )),
            _ => uncaught_type_error!(),
        }
    }
    /// Converts this value to a cell state filter if this is a
    /// ConstValue::CellState or ConstValue::CellStateFilter; otherwise returns
    /// an InternalError.
    pub fn coerce_to_cell_state_filter(self, state_count: usize) -> LangResult<CellStateFilter> {
        match self {
            Self::CellState(i) => Ok(CellStateFilter::single_cell_state(state_count, i)),
            Self::CellStateFilter(f) => Ok(f),
            _ => uncaught_type_error!(),
        }
    }

    /// Returns the range step to use by default given start and end integers.
    /// +1 if start <= end; -1 if start > end.
    pub fn infer_range_step(start: LangInt, end: LangInt) -> LangInt {
        if start <= end {
            1
        } else {
            -1
        }
    }
}
