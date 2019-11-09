use crate::automaton::space::*;

mod totalistic;

pub use totalistic::*;

pub trait Algorithm<T: CellType, D: Dim> {
    type R: Region<D>;
    /// Compute the next state for the cell at the center of the napkin.
    fn transition(&self, napkin: &Napkin<T, D, Self::R>) -> T;
}
