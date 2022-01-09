//! HashLife simulation algorithm.

use itertools::Itertools;

use super::rule::{NdRule, TransitionFunction};
use crate::dim::Dim;
use crate::ndarray::NdArray;
use crate::ndrect::{NdRect, URect};
use crate::ndtree::{
    HashLifeResultParamsBuilder, Layer, NdTree, NodeRef, NodeRefEnum, NodeRefTrait, SimCacheGuard,
};
use crate::ndvec::UVec;
use crate::num::{BigInt, Zero};

// TODO: parallelize using threadpool and crossbeam_channel (call execute threadpool.max_count times with closures that just loop)

/// Advances the given ND-tree by the given number of generations.
pub fn step<D: Dim>(tree: &mut NdTree<D>, rule: &dyn NdRule<D>, gens: &BigInt) {
    if gens.is_zero() {
        return; // No need to simulate anything!
    }

    // TODO: consider being nicer to GC threads
    let node_pool = tree.pool().new_ref();
    let node_pool_access = node_pool.access();
    let sim_guard = node_pool_access.sim_with(
        HashLifeResultParamsBuilder::new()
            .with_rule_radius(rule.radius())
            .with_step_size(gens)
            .build()
            .unwrap(), // TODO handle error
    );

    // Prepare the transition function.
    let mut transition_function = rule.transition_function();

    // If the number of generations is not a power of 2, we may have to
    // break this into multiple power-of-2-sized steps.
    for _ in 0..sim_guard.params().num_steps() {
        // Expand the existing pattern to `sim_base_layer`.
        tree.expand_while(|ndtree| ndtree.layer() < sim_guard.params().sim_base_layer());

        // Expand it by another layer to give room for new cells to be born.
        tree.expand();

        // Now expand one more layer to guarantee that the edges of the current
        // ND-tree, which could be altered by cells in the middle, will be
        // included in the final result, because `advance_inner_node()` returns
        // a node one layer lower than its input.
        tree.expand();

        assert!(tree.layer() > Layer(2));

        // Now do the actual simulation.
        let new_root = advance_inner_node(
            tree.root().as_ref(&node_pool_access),
            &sim_guard,
            &mut transition_function,
        );
        tree.set_root_centered(new_root);

        // Shrink the tree as much as possible to avoid wasted space. TODO:
        // is it better to have this inside the loop or outside the loop?
        tree.shrink();
    }
}

/// Computes the inner node for a given node after some predetermined number
/// of generations.
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
#[must_use = "This method returns a new value instead of mutating its input"]
fn advance_inner_node<'pool, D: Dim>(
    node: NodeRef<'pool, D>,
    sim_guard: &SimCacheGuard<'_, D>,
    transition_function: &mut TransitionFunction<'_, D>,
) -> NodeRef<'pool, D> {
    let sim_params = sim_guard.params();

    // Make sure we're above the minimum layer.
    assert!(
        node.layer() >= sim_params.min_layer(),
        "Cannot advance inner node at layer below minimum simulation layer"
    );

    if let Some(result) = node.result(sim_guard) {
        // If the result is already computed, just return that.
        return result;
    }

    let ret: NodeRef<'pool, D> = if node.is_empty() {
        // If the entire node is empty, then in the future it will remain
        // empty. This is not strictly necessary, but it is an obvious
        // optimization for rules without "B0" behavior.

        // Rather than constructing a new node or fetching one from the node
        // pool, just return one of the children of this one (since we know it's
        // empty).
        match node.as_enum() {
            NodeRefEnum::Leaf(n) => n.pool().get_empty(n.layer().child_layer()),
            // It's easier to get a reference to a child than to look up an
            // empty node.
            NodeRefEnum::NonLeaf(n) => n.child_at_index(0),
        }
    } else if node.layer() == sim_params.min_layer()
        || node.layer().child_layer() <= Layer::base::<D>()
    {
        // If this is the minimum layer or the node's children are leaf
        // nodes, just process each cell individually. This is the final
        // recursive base case.

        // Compute the rule radius (rounded up to the nearest power of 2, though
        // this isn't necessary).
        let r = 1 << sim_params.log2_rule_radius();

        // We start with a node at layer `L` and time `0` and will end with
        // a node at layer `L-1` at time `t`, centered on the original node.
        let l = node.layer();
        let t = sim_params.node_step_size(l).unwrap();

        // We're able to do this because the "speed of light" is equal to
        // `r*t`, where `r` is the maximum Chebyshev radius of a cell's
        // neighborhood (https://en.wikipedia.org/wiki/Chebyshev_distance)
        // and `r*t` is less than the "padding" between the edge of the
        // result node and the original node.
        let mut rt = r * t;
        assert!(
            rt <= l.len().unwrap() / 4,
            "Cannot simulate {} generations at {:?} with radius {}",
            t,
            l,
            r,
        );

        let mut cells_ndarray = NdArray::from(node);
        let mut cells_ndarray_len = l.len().unwrap();
        // For each timestep ...
        for _ in 0..t {
            // `rt` is how much "padding" we need around each edge of the
            // result in order to simulate remaining generations.
            rt -= r;
            let new_cells_ndarray_len = l.child_layer().len().unwrap() + 2 * rt;
            // Compute the offset between the lowest corner of the current
            // `cells_ndarray` and the new one.
            let offset = (cells_ndarray_len - new_cells_ndarray_len) / 2;
            // Make a new array that is as big as we need.
            cells_ndarray = transition_function(
                &cells_ndarray,
                URect::with_size(UVec::repeat(offset), UVec::repeat(new_cells_ndarray_len)),
            );
            cells_ndarray_len = new_cells_ndarray_len;
        }

        // Finally, make an ND-tree from those cells
        node.pool().get_from_cells(cells_ndarray.into_flat_slice())
    } else {
        // Let `L` be the layer of the current node, and let `t` be the
        // number of generations to simulate. Colors refer to Figure 4 in
        // this article: https://www.drdobbs.com/jvm/_/184406478.
        //
        // We already checked that this node's children (at layer `L-1`) are
        // not leaf nodes, but its grandchildren (at layer `L-2`) might be.

        // 1. Make a 4^D array of nodes at layer `L-2` of the original node
        //    at time `0`.
        let unsimmed_quarter_size_nodes: NdArray<NodeRef<'pool, D>, D> = NdArray::from_flat_slice(
            UVec::repeat(4_usize),
            (0..(D::BRANCHING_FACTOR * D::BRANCHING_FACTOR))
                .map(|i| node.as_non_leaf().unwrap().grandchild_at_index(i))
                .collect_vec(),
        );

        // 2. Combine adjacent nodes at layer `L-2` to make a 3^D array of
        //    nodes at layer `L-1` and time `0`.
        let unsimmed_half_size_nodes: NdArray<NodeRef<'pool, D>, D> = NdArray::from_flat_slice(
            UVec::repeat(3_usize),
            URect::<D>::span(UVec::zero(), UVec::repeat(2_usize))
                .iter()
                .map(|pos| {
                    node.pool().join_nodes(
                        NdRect::span(pos.clone(), pos + 1)
                            .iter()
                            .map(|pos| unsimmed_quarter_size_nodes[pos]),
                    )
                })
                .collect_vec(),
        );

        // 3. Simulate each of those nodes to get a new node at layer `L-2`
        //    and time `t/2` (red squares).
        let half_simmed_quarter_size_nodes: NdArray<NodeRef<'pool, D>, D> =
            unsimmed_half_size_nodes
                .map(|&n| advance_inner_node(n, sim_guard, transition_function));

        // 4. Combine adjacent nodes from step #3 to make a 2^D array of
        //    nodes at layer `L-1` and time `t/2`.
        let half_simmed_half_size_nodes = URect::<D>::span(UVec::zero(), UVec::repeat(1_usize))
            .iter()
            .map(|pos| {
                node.pool().join_nodes(
                    NdRect::span(pos.clone(), pos + 1)
                        .iter()
                        .map(|pos| half_simmed_quarter_size_nodes[pos]),
                )
            });

        let this_nodes_log2_step_size = sim_params.log2_node_step_size(node.layer());
        let childrens_log2_step_size = sim_params.log2_node_step_size(node.layer().child_layer());

        // 5. Simulate each of those nodes to get a new node at layer `L-2`
        //    and time `t` (green squares).
        let fully_simmed_quarter_size_nodes = half_simmed_half_size_nodes.map(|node| {
            if this_nodes_log2_step_size != childrens_log2_step_size {
                advance_inner_node(node, sim_guard, transition_function)
            } else {
                // ... unless that `t/2` was actually the total number of
                // generations we needed to simulate, in which case don't
                // simulate any more. Just grab the inner node of each of
                // those nodes (which is like simulating them for zero
                // generations).
                node.centered_inner().unwrap()
            }
        });

        // 6. Combine the nodes from step #5 to make a new node at layer
        //    `L-1` and time `t` (blue square). This is the final result.
        node.pool().join_nodes(fully_simmed_quarter_size_nodes)
    };

    // Cache that result so we don't have to do all that work next time.
    node.set_result(Some(ret));
    ret
}
