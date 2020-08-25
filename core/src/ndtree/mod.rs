//! N-dimensional generalization of quadtrees.
//!
//! To store an infinite N-dimensional universe, we start with power-of-2-sized
//! hypercubes of cells, which are stored in a flat array (see the crate root
//! documentation for more details). To build larger universes, we combine
//! 2^NDIM of these hypercubes into a hypercube twice the size. For example, in
//! 2D a 128x128 hypercube consists of four 64x64 hypercubes.
//!
//! Each of these hypercubes is stored in a **node**. See the `ndtree::node`
//! documentation for more details.

use itertools::Itertools;
use std::fmt;
use std::sync::Arc;

// TODO: `aliases` module, and include it in crate::prelude

pub mod indexed;
mod node;
mod slice;

use crate::dim::*;
use crate::ndrect::{BigRect, CanContain, URect};
use crate::ndvec::BigVec;
use crate::num::{BigInt, One};
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

impl<D: Dim> NdTree<D> {
    /// Creates a new empty ND-tree using a new node cache with the given state
    /// count.
    pub fn with_state_count(state_count: usize) -> Self {
        let repr = NodeRepr::with_state_count(state_count);
        let cache = Arc::new(NodeCache::with_repr(repr));
        Self::with_cache(cache)
    }
    /// Creates a new empty ND-tree using the given node cache.
    pub fn with_cache(node_cache: Arc<NodeCache<D>>) -> Self {
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
            new_root.as_raw().layer() > Layer(0),
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
    pub fn layer(&self) -> Layer {
        todo!("document");
        self.root().as_raw().layer()
    }
    /// Returns the length of this tree along one axis.
    pub fn len(&self) -> BigInt {
        self.layer().big_len()
    }
    pub fn offset(&self) -> BigVec<D> {
        todo!("document");
        BigVec::repeat(-self.layer().child_layer().big_len())
    }
    pub fn rect(&self) -> BigRect<D> {
        todo!("document");
        self.layer().big_rect() + self.offset()
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
        let empty_node = node_access.get_empty(root.layer().child_layer());
        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        let new_root = self.new_arc_node(
            node_access.join_nodes(root.subdivide().unwrap().into_iter().enumerate().map(
                |(child_index, subcube)| {
                    // Invert the bits of the child index to get the index
                    // of the opposite child.
                    let opposite_child_index = child_index ^ child_index_bitmask;
                    // All children of this node will be empty ...
                    let mut children = vec![empty_node; D::BRANCHING_FACTOR];
                    // ... except for the opposite child, which will be
                    // closest to the center of the NdTree.
                    children[opposite_child_index] = subcube;
                    node_access.join_nodes(children)
                },
            )),
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
        if self.layer() == Layer(1) {
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
                Ok(grandchildren[opposite_child_index])
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
            let grandchild_layer = root.layer().child_layer().child_layer();
            // Get the rectangle of grandchildren of the current root that
            // includes the desired rectangle. Each axis is in the range from 0
            // to 3.
            let grandchild_rect: URect<D> = (rect.clone() >> grandchild_layer.to_u32()).to_urect();
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
            // over the grandchild positions in `grandchild_square`.
            let grandchildren = root
                .children()
                .map(|child| node_access.subdivide(child).unwrap())
                .collect_vec();
            let new_children = grandchild_square.iter().map(|grandchild_pos| {
                root.grandchild_at_index(Layer(2).leaf_cell_index(grandchild_pos))
            });
            // Compute the new offset.
            ret.offset += grandchild_square.min().to_bigvec() << grandchild_layer.to_u32();

            // Create the new node.
            ret.root = node_access.join_nodes(new_children);
        }
        return ret;
    }
}

#[cfg(test)]
mod tests;
