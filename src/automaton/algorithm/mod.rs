use crate::automaton::space::*;

mod totalistic;

pub use totalistic::*;

pub trait Algorithm<T: CellType, D: Dim> {
    type R: Region<D>;
    /// Return the neighborhood in use by this algorithm.
    fn get_neighborhood(&self) -> Self::R;
    /// Compute the next state for the cell at the center of the napkin.
    fn transition(&self, napkin: &Napkin<T, D, Self::R>) -> T;
}
