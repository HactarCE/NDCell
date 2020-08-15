//! ND-tree data structure, an N-dimensional generalization of a quadtree.

use itertools::Itertools;
use num::{BigInt, One};
use std::fmt;
use std::sync::Arc;

mod indexed;
mod node;
mod slice;

use super::*;
pub use indexed::*;
pub use node::*;
pub use slice::*;

/// An N-dimensional generalization of a quadtree.
#[derive(Debug, Clone)]
pub struct NdTree<D: Dim> {
    /// The root NdTreeNode of this slice.
    pub root: ArcNode<D>,
}
impl<D: Dim> PartialEq for NdTree<D> {
    fn eq(&self, other: &Self) -> bool {
        self.root() == other.root()
    }
}
impl<D: Dim> Eq for NdTree<D> {}

/// A 1D grid represented as a bintree.
pub type NdTree1D = NdTree<Dim1D>;
/// A 2D grid represented as a quadtree.
pub type NdTree2D = NdTree<Dim2D>;
/// A 3D grid represented as an octree.
pub type NdTree3D = NdTree<Dim3D>;
/// A 4D grid represented as a tree with nodes of degree 16.
pub type NdTree4D = NdTree<Dim4D>;
/// A 5D grid represented as a tree with nodes of degree 32.
pub type NdTree5D = NdTree<Dim5D>;
/// A 6D grid represented as a tree with nodes of degree 64.
pub type NdTree6D = NdTree<Dim6D>;

impl<D: Dim> fmt::Display for NdTree<D>
where
    for<'a> NdTreeSlice<'a, D>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: print nodes directly instead of relying on slice (although
        // cache access would still be required)
        let node_access = self.node_access();
        write!(f, "{}", self.slice(&node_access))
    }
}

impl<D: Dim> Default for NdTree<D> {
    fn default() -> Self {
        Self::new()
    }
}

impl<D: Dim> NdTree<D> {
    /// Constructs a new empty NdTree with an empty node cache centered on the
    /// origin.
    pub fn new() -> Self {
        Self::from_cache(Arc::new(NodeCache::default()))
    }
    pub fn from_cache(node_cache: Arc<NodeCache<D>>) -> Self {
        let node_access = node_cache.node_access();
        let root_ref = node_access.get_empty_base();
        let root = ArcNode::new(Arc::clone(&node_cache), root_ref);
        Self { root }
    }

    /// Returns the root node of this tree.
    pub fn root(&self) -> &ArcNode<D> {
        &self.root
    }
    /// Sets the root node of this tree.
    pub fn set_root(&mut self, new_root: ArcNode<D>) {
        // TODO: note that it must be at least 2x2 (layer 1)
        assert!(
            new_root.as_raw().layer() > 0,
            "Root of NdTree must be larger than a single cell"
        );
        self.root = new_root;
    }
    fn new_arc_node(&self, node: NodeRef<D>) -> ArcNode<D> {
        ArcNode::new(self.cache(), node)
    }

    pub fn cache(&self) -> Arc<NodeCache<D>> {
        todo!("document");
        Arc::clone(self.root().cache())
    }
    pub fn node_access(&self) -> NodeCacheAccess<D> {
        self.root().cache().node_access()
    }
    pub fn layer(&self) -> u32 {
        todo!("document");
        self.root().as_raw().layer()
    }
    /// Returns the length of this tree along one axis.
    pub fn len(&self) -> BigInt {
        BigInt::one() << self.root().as_raw().layer()
    }
    pub fn offset(&self) -> BigVec<D> {
        todo!("document");
        -BigVec::repeat(self.len()) >> 1
    }
    pub fn rect(&self) -> BigRect<D> {
        todo!("document");
        BigRect::span(self.offset(), -self.offset() - &BigInt::one())
    }
    /// Returns an `NdTreeSlice` view into this tree.
    pub fn slice<'cache>(
        &self,
        node_access: &'cache NodeCacheAccess<'cache, D>,
    ) -> NdTreeSlice<'cache, D> {
        NdTreeSlice::with_offset(self.root().as_ref(node_access), self.offset())
    }

    /// "Zooms out" of the current tree by a factor of 2.
    ///
    /// This is done by replacing each child with a larger node containing it in
    /// the opposite corner. For example, the NE child is replaced with a new
    /// node twice the size containing that old NE child in its SW corner. The
    /// final result is that the entire tree contains the same contents as
    /// before, but with 25% padding along each edge.
    pub fn expand(&mut self) {
        let node_access = self.node_access();
        let root = self.root().as_ref(&node_access);
        let empty_node = node_access.get_empty(root.layer() - 1);
        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        let new_root = self.new_arc_node(
            node_access.join_nodes(
                root.subdivide()
                    .unwrap()
                    .into_iter()
                    .enumerate()
                    .map(|(child_index, subcube)| {
                        // Invert the bits of the child index to get the index
                        // of the opposite child.
                        let opposite_child_index = child_index ^ child_index_bitmask;
                        // All children of this node will be empty ...
                        let mut children =
                            vec![empty_node.as_raw(); D::BRANCHING_FACTOR].into_boxed_slice();
                        // ... except for the opposite child, which will be
                        // closest to the center of the NdTree.
                        children[opposite_child_index] = subcube.as_raw();
                        node_access.join_nodes(children).as_raw()
                    })
                    .collect_vec(),
            ),
        );
        drop(node_access);
        self.set_root(new_root);
    }
    /// "Zooms out" by calling `NdTree::expand()` until this tree includes the
    /// given position.
    pub fn expand_to(&mut self, pos: &BigVec<D>) {
        while !self.rect().contains(pos) {
            self.expand();
        }
    }
    /// "Zooms in" to the current tree as much as possible without losing
    /// non-empty cells. Returns the number of times the tree was shrunk by a
    /// factor of 2.
    pub fn shrink(&mut self) {
        while self._shrink().is_ok() {}
    }
    fn _shrink(&mut self) -> Result<(), ()> {
        // If we are already at the minimum layer (2x2), we can't shrink any
        // more. The root node must be at least 4x4 so that we can split it into
        // grandchildren.
        if self.layer() == 1 {
            return Err(());
        }

        let node_access = self.node_access();
        let root = self.root().as_ref(&node_access);
        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        // Fetch the grandchildren of this node that are closest to the center.
        let new_children: Vec<_> = root
            .subdivide()
            .unwrap()
            .into_iter()
            .enumerate()
            .map(|(child_index, child)| {
                // Invert the bits of the child index to get the index of the
                // opposite child, which is the one closest to the center (e.g.
                // SE child of NE child).
                let opposite_child_index = child_index ^ child_index_bitmask;
                // If any grandchild other than the one closest to the center is
                // non-empty, then we can't shrink any more.
                let grandchildren = node_access.subdivide(child).unwrap();
                for (i, grandchild) in grandchildren.iter().enumerate() {
                    if i != opposite_child_index && !grandchild.is_empty() {
                        return Err(());
                    }
                }
                // Return the grandchild closest to the center.
                Ok(grandchildren[opposite_child_index].as_raw())
            })
            .try_collect()?;
        let new_root = self.new_arc_node(node_access.join_nodes(new_children));
        drop(node_access);
        self.set_root(new_root);
        Ok(())
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, node_access: &NodeCacheAccess<D>, pos: &BigVec<D>) -> u8 {
        let root = self.root().as_ref(node_access);
        if root.big_rect().contains(pos) {
            root.cell_at_pos(&(pos - self.offset()))
        } else {
            0
        }
    }
    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, node_access: &NodeCacheAccess<D>, pos: &BigVec<D>, cell_state: u8) {
        self.expand_to(&pos);
        let root = self.root().as_ref(node_access);
        let new_root = self.new_arc_node(root.set_cell(&(pos - self.offset()), cell_state));
        self.set_root(new_root);
    }

    /// Returns an NdTreeSlice of the smallest node in the grid containing the
    /// given rectangle.
    ///
    /// The result may not directly correspond to an existing node; it may be
    /// centered on an existing node, and thus composed out of smaller existing
    /// nodes. The only guarantee is that the node will be either a leaf node or
    /// less than twice as big as it needs to be.
    pub fn get_slice_containing(&self, rect: &'_ BigRect<D>) -> NdTreeSlice<D> {
        todo!("test this");
        let node_access = self.node_access();
        // Grow the NdTree until it contains the desired rectangle.
        let mut tmp_ndtree = self.clone();
        tmp_ndtree.expand_to(&rect.min());
        tmp_ndtree.expand_to(&rect.max());
        let mut ret = tmp_ndtree.slice(&node_access);
        self.shrink();
        // "Zoom in" on either a corner, an edge/face, or the center until it
        // can't shrink any more. Each iteration of the loop "zooms in" by a
        // factor of 2.
        loop {
            // If the node is a leaf node, don't zoom in any more; this is small
            // enough.
            if ret.root.is_leaf() {
                break;
            }
            let root = ret.root.as_non_leaf().unwrap();

            // Since the root is not a leaf node, we know it is at least at
            // layer 2.
            let grandchild_layer = root.layer() - 2;
            // Get the rectangle of grandchildren of the current root that
            // includes the desired rectangle. Each axis is in the range from 0
            // to 3.
            let grandchild_rect: URect<D> = (rect.clone() >> grandchild_layer).to_urect();
            let mut new_min = grandchild_rect.min();
            let new_max = grandchild_rect.max();
            for &ax in D::axes() {
                // We want to include exactly two grandchild-lengths (which is
                // equal to the length of one child) along each axis, so if
                // grandchild_rect is longer than 2 along any axis, we can't
                // shrink any more. If it's greater than 2, expand a little so
                // that it equals 2.
                match grandchild_rect.len(ax) {
                    0 => unreachable!(),
                    1 => {
                        // Expand so that the length along this axis is 2.
                        if new_min[ax] < 2 {
                            // It's more on the negative end, so bump up the
                            // maximum edge.
                            new_max[ax] += 1;
                        } else {
                            // It's more on the positive end, so bump down the
                            // minimum edge.
                            new_min[ax] -= 1;
                        };
                    }
                    2 => (),
                    _ => break,
                }
            }
            let grandchild_square = URect::span(new_min, new_max);

            // Fetch the 2^NDIM nodes that comprise the new node by iterating
            // over the grandchild positions in grandchild_square.
            let grandchildren = root
                .children()
                .map(|child| node_access.subdivide(child).unwrap())
                .collect_vec();
            let new_children = grandchild_square
                .iter()
                .map(|grandchild_pos| {
                    let child_index =
                        node_math::non_leaf_pos_to_child_index::<D>(2, &grandchild_pos.to_bigvec());
                    let grandchild_index =
                        node_math::non_leaf_pos_to_child_index::<D>(1, &grandchild_pos.to_bigvec());
                    grandchildren[child_index][grandchild_index].as_raw()
                })
                .collect_vec();
            // Compute the new offset.
            ret.offset += grandchild_square.min().to_bigvec() << grandchild_layer;

            // Create the new node.
            ret.root = node_access.join_nodes(new_children);
        }
        return ret;
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use proptest::prelude::*;
    use std::collections::HashMap;

    use super::*;

    fn assert_ndtree_valid(
        expected_cells: &HashMap<IVec2D, u8>,
        ndtree: &mut NdTree2D,
        cells_to_check: &Vec<IVec2D>,
    ) {
        let node_access = ndtree.node_access();
        assert_eq!(
            &BigUint::from(
                expected_cells
                    .iter()
                    .filter(|(_, &cell_state)| cell_state != 0)
                    .count()
            ),
            ndtree.root().population()
        );
        for pos in cells_to_check {
            assert_eq!(
                *expected_cells.get(pos).unwrap_or(&0),
                ndtree.get_cell(&node_access, &pos.to_bigvec())
            );
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 4096,
            ..Default::default()
        })]

        /// Tests set_cell() and get_cell() by comparing against a HashMap.
        #[test]
        fn test_ndtree_set_get(
            cells_to_set: Vec<(IVec2D, u8)>,
            mut cells_to_get: Vec<IVec2D>,
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            let cache = ndtree.cache();
            let node_access = cache.node_access();
            for (pos, state) in cells_to_set {
                hashmap.insert(pos, state);
                ndtree.set_cell(&node_access, &pos.to_bigvec(), state);
                cells_to_get.push(pos);
            }
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
            // Test that expansion preserves population and positions.
            let old_layer = ndtree.layer();
            while ndtree.layer() < 5 {
                ndtree.expand();
                assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
            }
            // Test that shrinking actually shrinks.
            ndtree.shrink();
            assert!(ndtree.layer() <= old_layer);
            // Test that shrinking preserves population and positions.
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
        }

        /// Tests that NdTreeCache automatically caches identical nodes.

        // TODO: does this work consistently now?
        // TODO: move this to ndtree::node::cache
        // #[ignore]
        #[test]
        fn test_ndtree_cache(
            cells_to_set: Vec<(IVec2D, u8)>,
        ) {
            prop_assume!(!cells_to_set.is_empty());
            let mut ndtree = NdTree::new();
            let cache = ndtree.cache();
            let node_access = cache.node_access();
            for (pos, state) in cells_to_set {
                ndtree.set_cell(&node_access, &(pos - 128).to_bigvec(), state);
                ndtree.set_cell(&node_access, &(pos + 128).to_bigvec(), state);
            }
            let slice = ndtree.slice(&node_access);
            let children = slice.subdivide().unwrap();
            let subnode1 = &children[0];
            let subnode2 = &children[children.len() - 1];
            assert_eq!(subnode1, subnode2);
            assert!(std::ptr::eq(subnode1.root.as_raw(), subnode2.root.as_raw()));
        }

        /// Tests NdTree::get_slice_containing().
        #[test]
        fn test_ndtree_get_slice_containing(
            cells_to_set: Vec<(IVec2D, u8)>,
            center: IVec2D,
            x_radius in 0..20_isize,
            y_radius in 0..20_isize,
        ) {
            let mut ndtree = NdTree::new();
            let mut hashmap = HashMap::new();
            let cache = ndtree.cache();
            let node_access = cache.node_access();
            for (pos, state) in cells_to_set {
                hashmap.insert(pos, state);
                ndtree.set_cell(&node_access, &pos.to_bigvec(), state);
            }
            let half_diag = NdVec([x_radius, y_radius]);
            let rect = IRect::span(center - half_diag, center + half_diag).to_bigrect();
            let slice = ndtree.get_slice_containing(&rect);
            let slice_rect = slice.rect();
            assert!(slice_rect.contains(&rect));
            assert!(
                slice.root.layer() == 1
                || slice.root.big_len() < rect.len(X) * 2
                || slice.root.big_len() < rect.len(Y) * 2
            );
            for (pos, state) in hashmap {
                if slice_rect.contains(&pos.to_bigvec()) {
                    if let Some(cell_state) = slice.get_cell(&pos.to_bigvec()) {
                        assert_eq!(state, cell_state);
                    }
                }
            }
        }
    }
}
