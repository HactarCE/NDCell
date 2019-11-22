//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

pub mod rule;
pub mod space;

pub use rule::{DummyRule, Rule};
pub use space::*;

use std::marker::PhantomData;

/// A cellular automaton simulation.
pub struct Automaton<T: CellType, D: Dim, R: Rule<T, D>> {
    phantom: PhantomData<(T, D)>,
    /// The rule to simulate.
    pub rule: R,
    /// The grid over which to simulate that rule.
    pub grid: NdTree<T, D>,
}

impl<T: CellType, D: Dim, R: Rule<T, D>> Automaton<T, D, R> {
    /// Construct a new Automaton that simulates a given rule over a given grid.
    pub fn new(rule: R, grid: NdTree<T, D>) -> Self {
        Self {
            phantom: PhantomData,
            rule,
            grid,
        }
    }
    /// Compute the next step in the simulation.
    pub fn step(&mut self) {
        // self.grid.sim(&self.rule, 1);
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    fn get_non_default_set<T: CellType, D: Dim>(slice: &NdTreeSlice<T, D>) -> HashSet<NdVec<D>> {
        let mut ret = HashSet::new();
        for (branch_idx, branch) in slice.root.branches.iter().enumerate() {
            let branch_offset = slice.offset + slice.root.branch_offset(branch_idx);
            match branch {
                NdTreeBranch::Leaf(cell_state) => {
                    if *cell_state != T::default() {
                        ret.insert(branch_offset);
                    }
                }
                NdTreeBranch::Node(node) => ret.extend(get_non_default_set(&NdTreeSlice {
                    root: node.clone(),
                    offset: branch_offset,
                })),
            }
        }
        ret
    }

    fn make_cell_coords_set<D: Dim>(coords_vec: Vec<D>) -> HashSet<NdVec<D>> {
        coords_vec.into_iter().map(Into::into).collect()
    }

    #[test]
    fn test_cgol() {
        let mut sim = Automaton::new(rule::LIFE, NdTree::new());

        // Make a glider
        sim.grid.set_cell([3, 3].into(), true);
        sim.grid.set_cell([4, 3].into(), true);
        sim.grid.set_cell([5, 3].into(), true);
        sim.grid.set_cell([5, 2].into(), true);
        sim.grid.set_cell([4, 1].into(), true);
        println!("{}", sim.grid);
        println!();

        assert_eq!(
            make_cell_coords_set(vec![[3, 3], [4, 3], [5, 3], [5, 2], [4, 1]]),
            get_non_default_set(&sim.grid.slice)
        );
        // Simulate it for a few steps.
        sim.step();
        println!("{}", sim.grid);
        println!();
        assert_eq!(
            make_cell_coords_set(vec![[4, 4], [4, 3], [5, 3], [5, 2], [3, 2]]),
            get_non_default_set(&sim.grid.slice)
        );
        sim.step();
        println!("{}", sim.grid);
        println!();
        assert_eq!(
            make_cell_coords_set(vec![[4, 4], [5, 4], [5, 3], [5, 2], [3, 3]]),
            get_non_default_set(&sim.grid.slice)
        );
    }
}
