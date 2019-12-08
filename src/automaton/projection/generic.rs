use std::rc::Rc;

use super::*;

/// A discrete-time simulation with some number of dimensions and undo history.
pub trait NdSimulate {
    /// Returns the number of dimensions in the simulation.
    ///
    /// If this were a const value or static function, then it would be
    /// impossible to make a trait object, which is half of the point of this
    /// entire module. See
    /// https://github.com/rust-lang/rust/pull/48026#issuecomment-363289757 for
    /// an explanation.
    fn get_ndim(&self) -> usize;
    /// Advances one variable-size (depending on `step_size`) step into the simulation.
    fn step(&mut self, record_in_history: bool);
    /// Advances a single step into the simulation.
    fn step_single(&mut self, record_in_history: bool);
    /// Saves the current state in the undo history.
    fn push_to_undo_history(&mut self);
    /// Saves the current state in the redo history.
    fn push_to_redo_history(&mut self);
    /// Returns the step size of the simulation.
    fn get_step_size(&self) -> usize;
    /// Sets the step size of the simulation.
    fn set_step_size(&mut self, new_step_size: usize);
    /// Returns true if there is something that can be undone; false otherwise.
    fn has_undo(&self) -> bool;
    /// Returns true if there is something that can be redone; false otherwise.
    fn has_redo(&self) -> bool;
    /// Restores the most recent state from the undo history.
    fn undo(&mut self) -> bool;
    /// Restores the most recently undone state from the redo history.
    fn redo(&mut self) -> bool;
    /// Restores the first state from the undo history.
    fn reset(&mut self) -> bool;
    /// Returns the number of discrete timesteps that have elapsed in the
    /// simulation.
    fn get_generation_count(&self) -> usize;
    /// Sets the generation count, totally detached from how many steps have actually been simulated.
    ///
    /// This is occasionally useful; e.g. for resetting the generation count to
    /// 0, or in simulations that depend on time parity (e.g. Golly's method of
    /// simulating B0 rules by swapping states on odd generation_count.)
    fn set_generation_count(&mut self, new_generation_count: usize);
    /// Resets the generation count to 0.
    fn reset_generation_count(&mut self) {
        self.set_generation_count(0);
    }
}

/// An N-dimensional automaton, complete with NdTree, Simulation, projection
/// info, and more.
#[derive(Debug, Default, Clone)]
pub struct NdAutomaton<C: CellType, D: Dim, P: NdProjectionInfo<D>> {
    /// A description of how to project the grid to the correct number of
    /// dimensions.
    pub projection_info: Rc<P>,
    /// The actual NdTree behind the scenes.
    pub tree: NdTree<C, D>,
    /// A simulation of this tree.
    pub sim: Simulation<C, D>,
    /// All previous states of this automaton ("undo history").
    pub undo_history: Vec<(NdTreeSlice<C, D>, usize)>,
    /// All undone future states of this automaton ("redo history").
    pub redo_history: Vec<(NdTreeSlice<C, D>, usize)>,
    /// The number of discrete timesteps that have elapsed in the simulation.
    pub generation_count: usize,
}
impl<C: CellType, D: Dim, P: NdProjectionInfo<D>> From<NdTree<C, D>> for NdAutomaton<C, D, P> {
    fn from(tree: NdTree<C, D>) -> Self {
        Self {
            tree: tree,
            projection_info: Default::default(),
            sim: Simulation::new(Rc::new(rule::DummyRule), 1),
            undo_history: vec![],
            redo_history: vec![],
            generation_count: 0,
        }
    }
}
impl<C: CellType, D: Dim, P: NdProjectionInfo<D>> NdSimulate for NdAutomaton<C, D, P> {
    fn get_ndim(&self) -> usize {
        D::NDIM
    }
    fn step(&mut self, record_in_history: bool) {
        if record_in_history {
            self.push_to_undo_history();
        }
        self.sim.step(&mut self.tree);
        self.generation_count += self.sim.get_step_size();
    }
    fn step_single(&mut self, record_in_history: bool) {
        if record_in_history {
            self.push_to_undo_history();
        }
        self.sim.step_single(&mut self.tree);
        self.generation_count += 1;
    }
    fn push_to_undo_history(&mut self) {
        self.undo_history
            .push((self.tree.slice.clone(), self.generation_count));
    }
    fn push_to_redo_history(&mut self) {
        self.redo_history
            .push((self.tree.slice.clone(), self.generation_count));
    }
    fn get_step_size(&self) -> usize {
        self.sim.get_step_size()
    }
    fn set_step_size(&mut self, new_step_size: usize) {
        self.sim.set_step_size(new_step_size);
    }
    fn has_undo(&self) -> bool {
        !self.undo_history.is_empty()
    }
    fn has_redo(&self) -> bool {
        !self.redo_history.is_empty()
    }
    fn undo(&mut self) -> bool {
        if let Some((last_tree, last_generation_count)) = self.undo_history.pop() {
            self.push_to_redo_history();
            self.tree.slice = last_tree;
            self.generation_count = last_generation_count;
            true
        } else {
            false
        }
    }
    fn redo(&mut self) -> bool {
        if let Some((next_tree, next_generation_count)) = self.redo_history.pop() {
            self.push_to_undo_history();
            self.tree.slice = next_tree;
            self.generation_count = next_generation_count;
            true
        } else {
            false
        }
    }
    fn reset(&mut self) -> bool {
        while self.undo_history.len() > 1 {
            self.redo_history.push(self.undo_history.pop().unwrap());
        }
        if let Some((first_tree, first_generation_count)) = self.undo_history.pop() {
            self.tree.slice = first_tree;
            self.generation_count = first_generation_count;
            true
        } else {
            false
        }
    }
    fn get_generation_count(&self) -> usize {
        self.generation_count
    }
    fn set_generation_count(&mut self, new_generation_count: usize) {
        self.generation_count = new_generation_count;
    }
}
impl<C: CellType, D: Dim, P: NdProjectionInfo<D>> NdAutomaton<C, D, P> {
    /// Returns the NdProjectedTreeSlice at the root of this NdAutomaton.
    pub fn nd_slice(&self) -> NdProjectedTreeSlice<C, D, P> {
        NdProjectedTreeSlice {
            slice: self.tree.slice.clone(),
            projection_info: self.projection_info.clone(),
        }
    }
}

/// An NdTree with associated projection info.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NdProjectedTreeSlice<C: CellType, D: Dim, P: NdProjectionInfo<D>> {
    /// A description of how to slice the NdTreeSlice.
    pub projection_info: Rc<P>,
    /// The actual NdTreeSlice behind the scenes.
    pub slice: NdTreeSlice<C, D>,
}

/// An NdTreeNode with associated projection info.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NdProjectedTreeNode<C: CellType, D: Dim, P: NdProjectionInfo<D>> {
    /// A description of how to slice the NdTreeNode.
    pub projection_info: Rc<P>,
    /// The actual NdTreeNode behind the scenes.
    pub node: NdCachedNode<C, D>,
}
