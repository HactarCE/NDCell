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
    use std::collections::HashSet;

    fn get_non_default_set<T: CellType, D: Dim>(grid: &Grid<T, D>) -> HashSet<CellCoords<D>> {
        let mut ret = HashSet::new();
        let default = T::default();
        for (&chunk_coords, chunk) in grid.get_chunks().iter() {
            for local_coords in LocalCoords::all() {
                if chunk[local_coords] != default {
                    ret.insert(chunk_coords + local_coords);
                }
            }
        }
        ret
    }

    fn make_cell_coords_set<D: Dim>(coords_vec: Vec<D>) -> HashSet<CellCoords<D>> {
        coords_vec.into_iter().map(CellCoords).collect()
    }

    #[test]
    fn test_cgol() {
        let mut sim = Automaton::new(algorithm::LIFE, Grid::new());
        // Make a glider
        sim.grid[CellCoords([3, 3])] = true;
        sim.grid[CellCoords([4, 3])] = true;
        sim.grid[CellCoords([5, 3])] = true;
        sim.grid[CellCoords([5, 2])] = true;
        sim.grid[CellCoords([4, 1])] = true;
        println!("{}", sim.grid[ChunkCoords([0, 0])]);
        assert_eq!(
            make_cell_coords_set(vec![[3, 3], [4, 3], [5, 3], [5, 2], [4, 1]]),
            get_non_default_set(&sim.grid)
        );
        sim.step();
        println!("{}", sim.grid[ChunkCoords([0, 0])]);
        assert_eq!(
            make_cell_coords_set(vec![[4, 4], [4, 3], [5, 3], [5, 2], [3, 2]]),
            get_non_default_set(&sim.grid)
        );
        sim.step();
        println!("{}", sim.grid[ChunkCoords([0, 0])]);
        assert_eq!(
            make_cell_coords_set(vec![[4, 4], [5, 4], [5, 3], [5, 2], [3, 3]]),
            get_non_default_set(&sim.grid)
        );
    }
}
