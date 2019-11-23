use std::collections::HashMap;

use super::*;

/// A HashLife simulation of a given automaton that caches simulation results.
pub struct Simulation<'a, C: CellType, D: Dim> {
    rule: Box<&'a dyn Rule<C, D>>,
    step_size: usize,
    min_layer: usize,
    results: ResultsCache<C, D>,
}

impl<'a, C: CellType, D: Dim> Simulation<'a, C, D> {
    /// Constructs a new Simulation with the given rule and step size.
    pub fn new(rule: Box<&'a dyn Rule<C, D>>, step_size: usize) -> Self {
        // Determine the minimum layer at which we can simulate one generation
        // of the automaton, using `n / 4 >= r * t`. (See the doumentation for
        // Simulation::advance_inner_node() for an explanation.) Even at r=0 or
        // r=1, the minimum layer is 2 because we need to return the inner node
        // (which is at a lower layer) and the minimum layer is 1.
        let mut min_layer = 2;
        while NdTreeNode::<C, D>::len_at_layer(min_layer) / 4 > rule.radius() {
            min_layer += 1;
        }

        Self {
            rule,
            step_size,
            min_layer,
            results: ResultsCache::default(),
        }
    }

    /// Advances the given NdTree by a number of generations equal to this
    /// simulation's step size.
    pub fn step(&mut self, tree: &mut NdTree<C, D>) {
        // Expand as much as necessary to ensure that the sphere of influence of
        // the existing pattern is included in the inner node of the tree's root
        // node, following `expansion_distance >= r * t`. This automatically
        // guarantees that `n / 4 >= r * t` as well.
        let mut expansion_distance = 0;
        while expansion_distance < self.rule.radius() * self.step_size {
            tree.expand();
            expansion_distance += tree.get_root().len() / 4;
        }
        // If we are somehow still at layer 1, expand to layer 2 because
        // Simulation::advance_inner_node() must return a node one layer lower
        // than its input, and layer 1 is the minimum layer.
        if tree.get_root().layer == 1 {
            tree.expand();
        }
        // Now do the actual simulation.
        let new_node = self.advance_inner_node(
            &mut tree.cache.borrow_mut(),
            &tree.slice.root,
            self.step_size,
        );
        tree.set_root_centered(new_node);
        // Shrink the tree as much as possible to avoid wasted space.
        tree.shrink();
    }

    /// Computes the "inner" node for a given node after the given numebr of
    /// generations.
    ///
    /// A node's "inner" node is the node one layer down, centered on the
    /// original node. For example, the inner node of a 16x16 node is the 8x8
    /// node centered on it. HashLife always performs calculations like this; a
    /// node is progressed some distance into the future, and the state of its
    /// inner node is the result. This is because without outside information,
    /// it is impossible to predict the state of the entire node (since adjacent
    /// cells outside of the node could affect it), but it is always possible to
    /// predict the inner node of a node with length `n` after `t` generations
    /// using a rule with max neighborhood radius `r` if `n / 4 >= r * t`. (`r`
    /// defines the maximum speed that information can travel, so `r * t` is the
    /// distance that information can travel, and `n / 4` is the distance from
    /// any edge of the inner node to the edge of the outer node.)
    fn advance_inner_node(
        &mut self,
        cache: &mut NdTreeCache<C, D>,
        node: &NdCachedNode<C, D>,
        steps: usize,
    ) -> NdCachedNode<C, D> {
        unimplemented!()
    }
}

/// A cache of simulation results for a variety of step sizes.
#[derive(Debug, Default)]
struct ResultsCache<C: CellType, D: Dim>(HashMap<usize, SingleStepResultsCache<C, D>, NodeHasher>);
impl<C: CellType, D: Dim> ResultsCache<C, D> {
    fn get_result(
        &self,
        node: &NdCachedNode<C, D>,
        step_size: usize,
    ) -> Option<&NdCachedNode<C, D>> {
        self.0
            .get(&step_size)
            .and_then(|single_step_cache| single_step_cache.get_result(node))
    }
    fn set_result(
        &mut self,
        node: NdCachedNode<C, D>,
        step_size: usize,
        result: NdCachedNode<C, D>,
    ) {
        self.0
            .entry(step_size)
            .or_insert_with(SingleStepResultsCache::default)
            .set_result(node, result);
    }
}

/// A cache of simulation results for a given step size.
#[derive(Debug, Default)]
struct SingleStepResultsCache<C: CellType, D: Dim>(
    HashMap<NdCachedNode<C, D>, NdCachedNode<C, D>, NodeHasher>,
);
impl<C: CellType, D: Dim> SingleStepResultsCache<C, D> {
    fn get_result(&self, node: &NdCachedNode<C, D>) -> Option<&NdCachedNode<C, D>> {
        self.0.get(node)
    }
    fn set_result(&mut self, node: NdCachedNode<C, D>, result: NdCachedNode<C, D>) {
        self.0.insert(node, result);
    }
}
