//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use core::fmt;

mod totalistic;

use crate::dim::Dim;
use crate::ndarray::NdArray;
use crate::ndvec::UVec;
pub use totalistic::*;

/// Type alias for a CA transition function.
pub type TransitionFunction<'a, D> = Box<dyn 'a + FnMut(&NdArray<u8, D>, UVec<D>) -> u8>;

/// A cellular automaton rule.
pub trait Rule<D: Dim>: fmt::Debug + Send + Sync {
    /// Returns the maximum distance away that a cell may need to see in order
    /// to compute its next state.
    fn radius(&self) -> usize;
    /// Returns a function that computes a cell's next state, given its
    /// neighborhood.
    fn transition_function<'a>(&'a self) -> TransitionFunction<'a, D>;
}

/// A basic rule that never changes any cell states.
#[derive(Debug, Default, Copy, Clone)]
pub struct DummyRule;
impl<D: Dim> Rule<D> for DummyRule {
    fn radius(&self) -> usize {
        0
    }
    fn transition_function<'a>(&'a self) -> TransitionFunction<'a, D> {
        Box::new(|nbhd, pos| nbhd[pos])
    }
}
