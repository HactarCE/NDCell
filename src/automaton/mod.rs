//! The simulation backend.
//!
//! This module contains everything needed to simulate cellular automata,
//! without displaying, importing, or exporting them.

pub mod rule;
pub mod simulation;
pub mod space;

pub use rule::{DummyRule, Rule};
pub use simulation::*;
pub use space::*;

use std::marker::PhantomData;

/// A cellular automaton simulation.
pub struct Automaton<C: CellType, D: Dim, R: Rule<C, D>> {
    phantom: PhantomData<(C, D)>,
    /// The rule to simulate.
    pub rule: R,
    /// The grid over which to simulate that rule.
    pub grid: NdTree<C, D>,
}

impl<C: CellType, D: Dim, R: Rule<C, D>> Automaton<C, D, R> {
    /// Construct a new Automaton that simulates a given rule over a given grid.
    pub fn new(rule: R, grid: NdTree<C, D>) -> Self {
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

    fn get_non_default_set<C: CellType, D: Dim>(slice: &NdTreeSlice<C, D>) -> HashSet<NdVec<D>> {
        let mut ret = HashSet::new();
        for (branch_idx, branch) in slice.root.branches.iter().enumerate() {
            let branch_offset = slice.offset + slice.root.branch_offset(branch_idx);
            match branch {
                NdTreeBranch::Leaf(cell_state) => {
                    if *cell_state != C::default() {
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
        let mut grid = NdTree::new();
        let rule = rule::LIFE;
        let mut sim = Simulation::new(Box::new(&rule), 1);

        // Make a glider
        grid.set_cell([3, 3].into(), true);
        grid.set_cell([4, 3].into(), true);
        grid.set_cell([5, 3].into(), true);
        grid.set_cell([5, 2].into(), true);
        grid.set_cell([4, 1].into(), true);
        println!("{}", grid);
        println!();

        assert_eq!(
            make_cell_coords_set(vec![[3, 3], [4, 3], [5, 3], [5, 2], [4, 1]]),
            get_non_default_set(&grid.slice)
        );
        // Simulate it for a few steps.
        sim.step(&mut grid);
        println!("{}", grid);
        println!();
        assert_eq!(
            make_cell_coords_set(vec![[4, 4], [4, 3], [5, 3], [5, 2], [3, 2]]),
            get_non_default_set(&grid.slice)
        );
        sim.step(&mut grid);
        println!("{}", grid);
        println!();
        assert_eq!(
            make_cell_coords_set(vec![[4, 4], [5, 4], [5, 3], [5, 2], [3, 3]]),
            get_non_default_set(&grid.slice)
        );
        sim = Simulation::new(Box::new(&rule), 64);
        sim.step(&mut grid);
        assert_eq!(
            make_cell_coords_set(vec![[20, 20], [21, 20], [21, 19], [21, 18], [19, 19]]),
            get_non_default_set(&grid.slice)
        );
        sim = Simulation::new(Box::new(&rule), 1024);
        sim.step(&mut grid);
        assert_eq!(
            make_cell_coords_set(vec![
                [276, 276],
                [277, 276],
                [277, 275],
                [277, 274],
                [275, 275]
            ]),
            get_non_default_set(&grid.slice)
        );
    }
}
