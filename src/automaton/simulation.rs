//! The functions that apply a rule to each cell in a grid.

use std::collections::HashMap;
use std::rc::Rc;

use super::*;
use crate::math::ceil_log_base_2;

/// A HashLife simulation of a given automaton that caches simulation results.
#[derive(Debug, Clone)]
pub struct Simulation<C: CellType, D: Dim> {
    rule: Rc<dyn Rule<C, D>>,
    step_size: usize,
    min_layer: usize,
    results: ResultsCache<C, D>,
}
impl<C: CellType, D: Dim> Default for Simulation<C, D> {
    fn default() -> Self {
        Self::new(Rc::new(DummyRule), 1)
    }
}

impl<C: CellType, D: Dim> Simulation<C, D> {
    /// Constructs a new Simulation with the given rule and step size.
    pub fn new(rule: Rc<dyn Rule<C, D>>, step_size: usize) -> Self {
        // Determine the minimum layer at which we can simulate one generation
        // of the automaton, using `n / 4 >= r`. (See the documentation for
        // Simulation::advance_inner_node() for an explanation.) Even at r=0 or
        // r=1, the minimum layer is 2 because we need to return the inner node
        // (which is at a lower layer) and the minimum layer is 1.
        let mut min_layer = 2;
        while (1 << min_layer) / 4 < rule.radius() {
            min_layer += 1;
        }

        Self {
            rule,
            step_size,
            min_layer,
            results: ResultsCache::default(),
        }
    }

    /// Returns the step size of this simulation.
    pub fn get_step_size(&self) -> usize {
        self.step_size
    }
    /// Sets the step size of this simulation to the given value.
    pub fn set_step_size(&mut self, new_step_size: usize) {
        if new_step_size != self.step_size {
            // If the new step is different, recompute ResultsCache. This is
            // only present to prevent holding onto memory we don't need any
            // more, and could probably be replaced with something smarter that
            // only prunes the SingleStepResultsCaches that won't be used with
            // this new step size.
            self.results = ResultsCache::default();
        }
        self.step_size = new_step_size;
    }

    /// Advances the given NdTree by a single generation.
    pub fn step_single(&mut self, tree: &mut NdTree<C, D>) {
        let old_step_size = self.step_size;
        self.step_size = 1;
        self.step(tree);
        self.step_size = old_step_size;
    }

    /// Advances the given NdTree by a number of generations equal to this
    /// simulation's step size.
    pub fn step(&mut self, tree: &mut NdTree<C, D>) {
        // Expand out to the sphere of influence of the existing pattern,
        // following `expansion_distance >= r * t`.
        let min_expansion_distance =
            1 << (ceil_log_base_2(self.rule.radius()) + ceil_log_base_2(self.step_size));
        let mut expansion_distance = 0;
        while expansion_distance < min_expansion_distance {
            tree.expand();
            expansion_distance += tree.get_root().len() / 4;
        }
        // Now expand one more layer to guarantee that the sphere of influence
        // is within the inner node, because Simulation::advance_inner_node()
        // must always returns a node one layer lower than its input. (This also
        // ensures that we aren't somehow still at layer 1; we need to be at at
        // least layer 2 so that the result can be at layer 1, which is the
        // minimum layer for a node.)
        tree.expand();
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

    /// Computes the inner node for a given node after the given numebr of
    /// generations.
    ///
    /// A node's inner node is the node one layer down, centered on the original
    /// node. For example, the inner node of a 16x16 node (layer 4) is the 8x8
    /// node (layer 3) centered on it. HashLife always performs calculations
    /// like this; a node is progressed some distance into the future, and the
    /// state of its inner node is the result. This is because without outside
    /// information, it is impossible to predict the state of the entire node
    /// (since adjacent cells outside of the node could affect it), but it is
    /// always possible in theory to predict the inner node of a node with
    /// length `n` after `t` generations using a rule with max neighborhood
    /// radius `r` if `n / 4 >= r * t`. (`r` defines the maximum speed that
    /// information can travel, so `r * t` is the distance that information can
    /// travel in time `t`, and `n / 4` is the distance from any edge of the
    /// inner node to the edge of the outer node.) In practice, however, each
    /// layer must be computed separately, so the `r` and `t` must each be
    /// replaced with their next lowest power of two.
    #[must_use]
    fn advance_inner_node(
        &mut self,
        cache: &mut NdTreeCache<C, D>,
        node: &NdCachedNode<C, D>,
        generations: usize,
    ) -> NdCachedNode<C, D> {
        // Handle the simplest case of just not simulating anything. This is one
        // of the recursive base cases.
        if generations == 0 {
            return node.get_inner_node(cache);
        }

        // If the result is already in the cache, just return that.
        if let Some(result) = self.results.get_result(node, generations) {
            return result.clone();
        }

        // Otherwise make sure we're above the minimum layer.
        assert!(
            node.layer >= self.min_layer,
            "Cannot advance inner node at layer below minimum simulation layer"
        );

        let mut ret;

        // If this is the minimum layer, just compute each cell manually. This
        // is the other recursive base case.
        if node.layer == self.min_layer {
            assert_eq!(
                1, generations,
                "Cannot simulate more than 1 generation at minimum layer"
            );
            ret = cache.get_empty_node(node.layer - 1);
            let mut slice = NdTreeSlice::centered(node.clone());
            for ret_pos in ret.rect().iter() {
                slice.offset = -ret_pos - slice.root.len() as isize / 4;
                ret = ret.set_cell(cache, ret_pos, self.rule.transition(&slice));
            }
        } else {
            // In the algorithm described below, there are two `t/2`s that must
            // add up to `t` (where `t` is the number of generations to
            // simulate). But of course if `t` is odd, then this may not be the
            // case. It hardly matters whether `t_outer` or `t_inner` is larger,
            // as long as they differ by no more than `1` and they add up to
            // `t`.
            let t_inner = generations / 2;
            let t_outer = generations - t_inner;

            // Colors refer to Figure 4 in this article:
            // https://www.drdobbs.com/jvm/_/184406478. Let `L` be the layer of
            // the current node, and let `t` be the number of generations to
            // simulate.
            let branch_count: usize = NdTreeNode::<C, D>::BRANCHES;
            let mut final_branches = Vec::with_capacity(branch_count);
            for final_branch_idx in 0..branch_count {
                let mut inner_branches = Vec::with_capacity(branch_count);
                for inner_branch_idx in 0..branch_count {
                    let mut outer_branches = Vec::with_capacity(branch_count);
                    for outer_branch_idx in 0..branch_count {
                        // 1. Grab sub-branches at layer `L-2` of the original
                        //    node at time `0`.
                        outer_branches.push(
                            node.get_sub_branch(
                                final_branch_idx,
                                inner_branch_idx,
                                outer_branch_idx,
                            )
                            .clone(),
                        );
                    }
                    // 2. Use these branches to make a node at layer `L-1` and
                    //    time `0`.
                    let inner_branch_node = cache.get_node(outer_branches);
                    // 3. Simulate that node to get a new node at layer `L-2`
                    //    and time `t/2` (red squares).
                    inner_branches.push(NdTreeBranch::Node(self.advance_inner_node(
                        cache,
                        &inner_branch_node,
                        t_outer,
                    )));
                }
                // 4. Using branches from step #3, create a node at layer `L-1`
                //    and time `t/2`.
                let final_branch_node = cache.get_node(inner_branches);
                // 5. Simulate that node to get a new node at layer `L-2` and
                //    time `t` (green squares).
                final_branches.push(NdTreeBranch::Node(self.advance_inner_node(
                    cache,
                    &final_branch_node,
                    t_inner,
                )));
            }
            // 6. Using branches from step #5, create a new node at layer `L-1`
            //    and time `t` (blue square). This is the final result.
            ret = cache.get_node(final_branches);
        }

        // Add the result to the cache so we don't have to do all that work next
        // time.
        self.results
            .set_result(node.clone(), generations, ret.clone());
        ret
    }
}

/// A cache of simulation results for a variety of step sizes.
#[derive(Debug, Default, Clone)]
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
#[derive(Debug, Default, Clone)]
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
