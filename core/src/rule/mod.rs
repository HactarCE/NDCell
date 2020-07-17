//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use crate::space::*;
use core::fmt;

mod totalistic;

pub use totalistic::*;

/// Type alias for a CA transition function.
pub type TransitionFunction<'a, C, D> = Box<dyn 'a + FnMut(NdArrayView<C, D>) -> C>;

/// A cellular automaton rule.
pub trait Rule<C: CellType, D: Dim>: fmt::Debug + Send + Sync {
    /// Returns the maximum distance away that a cell may need to see in order
    /// to compute its next state.
    fn radius(&self) -> usize;
    /// Returns a function that computes a cell's next state, given its
    /// neighborhood.
    fn get_transition_function(&self) -> TransitionFunction<C, D>;
}

/// A basic rule that never changes any cell states.
#[derive(Debug, Default, Copy, Clone)]
pub struct DummyRule;
impl<C: CellType, D: Dim> Rule<C, D> for DummyRule {
    fn radius(&self) -> usize {
        0
    }
    fn get_transition_function(&self) -> TransitionFunction<C, D> {
        Box::new(|nbhd| nbhd[&NdVec::origin()])
    }
}
