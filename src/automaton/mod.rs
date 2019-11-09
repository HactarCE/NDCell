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
pub struct Automaton<T: CellType, D: Dim, A: Algorithm<T, D>> {
    phantom: PhantomData<(T, D)>,
    /// The algorithm to simulate.
    pub algorithm: A,
    /// The grid over which to simulate that algorithm.
    pub grid: Grid<T, D>,
}

impl<T: CellType, D: Dim, A: Algorithm<T, D>> Automaton<T, D, A> {
    /// Construct a new Automaton that simulates a given rule over a given grid.
    pub fn new(algorithm: A, grid: Grid<T, D>) -> Self {
        Self {
            phantom: PhantomData,
            algorithm,
            grid,
        }
    }
    /// Compute the next step in the simulation.
    pub fn step(&mut self) {
        let mut new_grid = Grid::new();
        let neighborhood = self.algorithm.get_neighborhood();
        for (&chunk_coords, old_chunk) in self.grid.get_chunks().iter() {
            if old_chunk.is_empty() {
                continue;
            }
            let new_chunk = &mut new_grid[chunk_coords];
            for local_coords in LocalCoords::<D>::all() {
                let center_cell = chunk_coords + local_coords;
                new_chunk[local_coords] = self.algorithm.transition(&Napkin {
                    grid: &self.grid,
                    region: &neighborhood,
                    transformation: &|neighbor_coords| center_cell + neighbor_coords,
                });
            }
        }
        self.grid = new_grid;
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
