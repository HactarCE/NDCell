use std::rc::Rc;

use super::*;

/// A description of how a D-dimensional NdTree is projected into a 2D or 3D
/// environment.
pub trait NdTreeViewMeta<D: Dim>: Default + Copy + Clone {
    /// Returns the slice position of this TreeView.
    ///
    /// The slice position is the N-dimensional point in the automaton that is
    /// always visible regardless of which axes are being displayed. For
    /// example, when viewing a 2D slice along the X and Y axes of a 3D
    /// automaton, the slice position would determine the Z coordinate of the 2D
    /// slice being displayed.
    fn get_slice_pos(&self) -> NdVec<D>;
    /// Sets the slice position of this TreeView.
    fn set_slice_pos(&mut self, new_slice_pos: NdVec<D>);
}

/// A discrete-time simulation with some number of dimensions and undo history.
pub trait NdSimulate {
    /// The number of dimensions.
    const NDIM: usize;
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

#[derive(Debug, Default)]
pub struct NdTreeView<C: CellType, D: Dim, M: NdTreeViewMeta<D>> {
    /// A description of how to slice the NdTree.
    pub meta: M,
    /// The actual NdTree behind the scenes.
    pub tree: NdTree<C, D>,
    /// A simulation of this tree.
    pub sim: Simulation<C, D>,
    /// All previous states of this automaton ("undo history").
    pub history: Vec<NdTreeSlice<C, D>>,
    /// The number of discrete timesteps that have elapsed in the simulation.
    pub generation_count: usize,
}
impl<C: CellType, D: Dim, M: NdTreeViewMeta<D>> From<NdTree<C, D>> for NdTreeView<C, D, M> {
    fn from(tree: NdTree<C, D>) -> Self {
        Self {
            tree: tree,
            meta: M::default(),
            sim: Simulation::new(Rc::new(rule::DummyRule), 1),
            history: vec![],
            generation_count: 0,
        }
    }
}
impl<C: CellType, D: Dim, M: NdTreeViewMeta<D>> NdSimulate for NdTreeView<C, D, M> {
    const NDIM: usize = D::NDIM;
    fn step(&mut self) {
        self.sim.step(&mut self.tree);
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
impl<C: CellType, D: Dim, M: NdTreeViewMeta<D>> NdTreeView<C, D, M> {
    pub fn slice(&self) -> NdTreeSliceView<C, D, M> {
        NdTreeSliceView {
            slice: self.tree.slice.clone(),
            meta: self.meta,
        }
    }
}

pub struct NdTreeSliceView<C: CellType, D: Dim, M: NdTreeViewMeta<D>> {
    /// A description of how to slice the NdTreeSlice.
    pub meta: M,
    /// The actual NdTreeSlice behind the scenes.
    pub slice: NdTreeSlice<C, D>,
}

pub struct NdTreeNodeView<C: CellType, D: Dim, M: NdTreeViewMeta<D>> {
    /// A description of how to slice the NdTreeNode.
    pub meta: M,
    /// The actual NdTreeNode behind the scenes.
    pub node: NdCachedNode<C, D>,
}
