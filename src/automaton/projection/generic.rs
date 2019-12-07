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
    fn step(&mut self);
    /// Saves the current state in the undo history.
    fn push_to_history(&mut self);
    /// Returns the step size of the simulation.
    fn get_step_size(&self) -> usize;
    /// Sets the step size of the simulation.
    fn set_step_size(&mut self, new_step_size: usize);
    /// Restores the most recent state from the undo history.
    fn undo(&mut self) -> bool;
    /// Restores the most recent state from the undo history.
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
    pub history: Vec<NdTreeSlice<C, D>>,
    /// The number of discrete timesteps that have elapsed in the simulation.
    pub generation_count: usize,
}
impl<C: CellType, D: Dim, P: NdProjectionInfo<D>> From<NdTree<C, D>> for NdAutomaton<C, D, P> {
    fn from(tree: NdTree<C, D>) -> Self {
        Self {
            tree: tree,
            projection_info: Default::default(),
            sim: Simulation::new(Rc::new(rule::DummyRule), 1),
            history: vec![],
            generation_count: 0,
        }
    }
}
impl<C: CellType, D: Dim, P: NdProjectionInfo<D>> NdSimulate for NdAutomaton<C, D, P> {
    fn get_ndim(&self) -> usize {
        D::NDIM
    }
    fn step(&mut self) {
        self.sim.step(&mut self.tree);
        self.generation_count += self.sim.get_step_size();
    }
    fn push_to_history(&mut self) {
        self.history.push(self.tree.slice.clone());
    }
    fn get_step_size(&self) -> usize {
        self.sim.get_step_size()
    }
    fn set_step_size(&mut self, new_step_size: usize) {
        self.sim.set_step_size(new_step_size);
    }
    fn undo(&mut self) -> bool {
        if let Some(last_state) = self.history.pop() {
            self.tree.slice = last_state;
            true
        } else {
            false
        }
    }
    fn reset(&mut self) -> bool {
        let old_history = std::mem::replace(&mut self.history, vec![]);
        if let Some(first_state) = old_history.into_iter().next() {
            self.tree.slice = first_state;
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
