//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use crate::automaton::space::*;
use core::fmt;

mod lang;
mod totalistic;

pub use lang::{NdcaRule, NdcaRuleGenerator};
pub use totalistic::*;

pub type TransitionFunction<'a, C, D> = Box<dyn 'a + FnMut(&NdArraySlice<C, D>) -> C>;

/// A cellular automaton rule.
pub trait Rule<C: CellType, D: Dim>: fmt::Debug + Send + Sync {
    /// Returns the maximum distance away that a cell may need to see in order
    /// to compute its next state.
    fn radius(&self) -> usize;
    /// Returns a function that can be used to compute cell transitions.
    fn get_transition_function(&self) -> TransitionFunction<C, D>;
}

/// A basic rule that never changes any cell states.
#[derive(Debug)]
pub struct DummyRule;
impl<C: CellType, D: Dim> Rule<C, D> for DummyRule {
    fn radius(&self) -> usize {
        0
    }
    fn get_transition_function(&self) -> TransitionFunction<C, D> {
        Box::new(|napkin| napkin[&NdVec::origin()])
    }
}
