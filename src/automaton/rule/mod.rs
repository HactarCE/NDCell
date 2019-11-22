//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use crate::automaton::space::*;
use core::fmt;

mod totalistic;

pub use totalistic::*;

/// A cellular automaton rule.
pub trait Rule<C: CellType, D: Dim>: fmt::Debug {
    /// Returns the maximum distance away that a cell may need to see in order
    /// to compute its next state.
    fn radius(&self) -> usize;

    /// Computes the next state for the cell at the center of the napkin.
    fn transition(&self, napkin: &NdTreeSlice<C, D>) -> C;
}

/// A basic rule that never changes any cell states.
#[derive(Debug)]
pub struct DummyRule;
impl<C: CellType, D: Dim> Rule<C, D> for DummyRule {
    fn radius(&self) -> usize {
        0
    }
    fn transition(&self, napkin: &NdTreeSlice<C, D>) -> C {
        napkin[NdVec::origin()]
    }
}
