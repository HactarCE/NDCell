//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use core::fmt;
use itertools::Itertools;
use std::sync::Arc;

pub mod aliases;
mod totalistic;

use crate::dim::*;
use crate::ndarray::NdArray;
use crate::ndrect::URect;
use crate::ndvec::UVec;
pub use aliases::*;
pub use totalistic::*;

/// Type alias for a CA transition function that transitions all the cells in an
/// array by one generation.
pub type TransitionFunction<'a, D> =
    Box<dyn 'a + FnMut(&NdArray<u8, D>, URect<D>) -> NdArray<u8, D>>;

/// Cellular automaton rule of any dimensionality.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum Rule {
    Rule1D(Rule1D),
    Rule2D(Rule2D),
    Rule3D(Rule3D),
    Rule4D(Rule4D),
    Rule5D(Rule5D),
    Rule6D(Rule6D),
}
impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rule::Rule1D(r) => write!(f, "{}", r),
            Rule::Rule2D(r) => write!(f, "{}", r),
            Rule::Rule3D(r) => write!(f, "{}", r),
            Rule::Rule4D(r) => write!(f, "{}", r),
            Rule::Rule5D(r) => write!(f, "{}", r),
            Rule::Rule6D(r) => write!(f, "{}", r),
        }
    }
}
impl<D: Dim> From<Arc<dyn NdRule<D>>> for Rule {
    fn from(r: Arc<dyn NdRule<D>>) -> Self {
        match_ndim!(match D {
            1 => Self::Rule1D(unsafe {
                std::mem::transmute::<Arc<dyn NdRule<D>>, Arc<dyn NdRule<Dim1D>>>(r)
            }),
            2 => Self::Rule2D(unsafe {
                std::mem::transmute::<Arc<dyn NdRule<D>>, Arc<dyn NdRule<Dim2D>>>(r)
            }),
            3 => Self::Rule3D(unsafe {
                std::mem::transmute::<Arc<dyn NdRule<D>>, Arc<dyn NdRule<Dim3D>>>(r)
            }),
            4 => Self::Rule4D(unsafe {
                std::mem::transmute::<Arc<dyn NdRule<D>>, Arc<dyn NdRule<Dim4D>>>(r)
            }),
            5 => Self::Rule5D(unsafe {
                std::mem::transmute::<Arc<dyn NdRule<D>>, Arc<dyn NdRule<Dim5D>>>(r)
            }),
            6 => Self::Rule6D(unsafe {
                std::mem::transmute::<Arc<dyn NdRule<D>>, Arc<dyn NdRule<Dim6D>>>(r)
            }),
        })
    }
}

/// Cellular automaton rule.
pub trait NdRule<D: Dim>: fmt::Debug + fmt::Display + Send + Sync {
    /// Returns the maximum distance away that a cell may need to see in order
    /// to compute its next state.
    fn radius(&self) -> usize;
    /// Returns a function that computes a cell's next state, given its
    /// neighborhood.
    fn transition_function<'a>(&'a self) -> TransitionFunction<'a, D>;
    /// Returns the maximum cell state value, which is one less than the number
    /// of cell states.
    fn max_state(&self) -> u8;
    /// Returns the rule as an `Arc<dyn Rule>`.
    fn into_arc(self) -> Arc<dyn NdRule<D>>
    where
        Self: 'static + Sized,
    {
        Arc::new(self)
    }
}

/// A basic rule that never changes any cell states.
#[derive(Debug, Default, Copy, Clone)]
pub struct DummyRule;
impl fmt::Display for DummyRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Dummy")
    }
}
impl<D: Dim> NdRule<D> for DummyRule {
    fn radius(&self) -> usize {
        0
    }
    fn transition_function<'a>(&'a self) -> TransitionFunction<'a, D> {
        Box::new(|nbhd, rect| transition_cell_array(rect, |pos| nbhd[pos]))
    }
    fn max_state(&self) -> u8 {
        u8::MAX
    }
}

/// Utility function that applies a transition function for a single cell to an
/// array of cells.
pub fn transition_cell_array<D: Dim>(
    rect: URect<D>,
    mut single_cell_transition_function: impl FnMut(UVec<D>) -> u8,
) -> NdArray<u8, D> {
    NdArray::from_flat_slice(
        rect.size(),
        rect.iter()
            .map(|pos| single_cell_transition_function(pos))
            .collect_vec(),
    )
}
