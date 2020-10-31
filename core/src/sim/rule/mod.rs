//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use core::fmt;
use itertools::Itertools;
use std::sync::Arc;

mod totalistic;

use crate::dim::Dim;
use crate::ndarray::NdArray;
use crate::ndrect::URect;
use crate::ndvec::UVec;
pub use totalistic::*;

/// Type alias for a CA transition function that transitions all the cells in an
/// array by one generation.
pub type TransitionFunction<'a, D> =
    Box<dyn 'a + FnMut(&NdArray<u8, D>, URect<D>) -> NdArray<u8, D>>;

/// A cellular automaton rule.
pub trait Rule<D: Dim>: fmt::Debug + fmt::Display + Send + Sync {
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
    fn into_arc(self) -> Arc<dyn Rule<D>>
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
impl<D: Dim> Rule<D> for DummyRule {
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
