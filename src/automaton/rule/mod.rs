//! Everything related to the description of a cellular automaton rule,
//! primarily the transition function.

use crate::automaton::space::*;

mod totalistic;

pub use totalistic::*;

/// A cellular automaton rule.
pub trait Rule<T: CellType, D: Dim> {
    /// Returns the maximum distance away that a cell may need to see in order
    /// to compute its next state.
    fn radius(&self) -> usize;

    /// Computes the next state for the cell at the center of the napkin.
    fn transition(&self, napkin: &NdTree<T, D>) -> T;
}
