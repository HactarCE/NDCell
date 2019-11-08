//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

mod algorithm;
pub mod space;

use algorithm::Algorithm;
use space::*;

use std::marker::PhantomData;

/// A cellular automaton simulation.
pub struct Automaton<T: CellType, C: Coords, A: Algorithm<T, C>> {
    phantom: PhantomData<(T, C)>,
    /// The algorithm to simulate.
    pub algorithm: A,
    /// The grid over which to simulate that algorithm.
    pub grid: Grid<T, C>,
}

impl<T: CellType, C: Coords, A: Algorithm<T, C>> Automaton<T, C, A> {
    /// Construct a new Automaton that simulates a given rule over a given grid.
    pub fn new(algorithm: A, grid: Grid<T, C>) -> Self {
        Self {
            phantom: PhantomData,
            algorithm,
            grid,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cgol() {
        // let sim = Automaton::new(algorithm::Totalistic());
    }
}
