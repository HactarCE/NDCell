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
use parking_lot::RwLock;
use std::fmt;
use std::sync::Arc;

pub mod aliases;
pub mod indexed;
mod node;
mod slice;

use crate::dim::*;
use crate::ndrect::{BigRect, CanContain, URect};
use crate::ndvec::BigVec;
use crate::num::BigInt;
pub use aliases::*;
pub use node::*;
pub use slice::*;

/// An N-dimensional generalization of a quadtree.
///
/// Note that an `NdTree` cannot be smaller than a base node.
#[derive(Debug, Clone)]
pub struct NdTree<D: Dim> {
    /// The root node of this slice.
    root: ArcNode<D>,
    /// Position of the lowest corner of the root node.
    offset: BigVec<D>,
}

impl<D: Dim> Default for NdTree<D> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<D: Dim> PartialEq for NdTree<D> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.root() == other.root()
    }
}
impl<D: Dim> Eq for NdTree<D> {}

impl<D: Dim> fmt::Display for NdTree<D>
where
    for<'a> NdTreeSlice<'a, D>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let node_cache = self.cache().read();
        fmt::Display::fmt(&self.slice(&*node_cache), f)
    }
}

impl<'a, D: Dim> From<NdTreeSlice<'a, D>> for NdTree<D> {
    fn from(slice: NdTreeSlice<'a, D>) -> Self {
        Self {
            root: slice.root.into(),
            offset: slice.offset,
        }
    }
}

impl<D: Dim> NdTree<D> {
    /// Creates an empty ND-tree using a new node cache.
    #[inline]
    pub fn new() -> Self {
        Self::with_cache(NodeCache::new())
    }
    /// Creates an empty ND-tree using the given node cache.
    #[inline]
    pub fn with_cache(node_cache: Arc<RwLock<NodeCache<D>>>) -> Self {
        Self::from_node_centered(node_cache.read().get_empty_base())
    }
    /// Creates an ND-tree containing the given node centered on the origin.
    ///
    /// # Panics
    ///
    /// This function panics if the node consists of only a single cell.
    #[inline]
    pub fn from_node_centered<'n>(node: impl CachedNodeRefTrait<'n, D = D>) -> Self {
        assert!(
            node.layer() > Layer(0),
            "Root of ND-tree must be larger than a single cell",
        );
        Self {
            root: node.into(),
            offset: BigVec::repeat(-node.layer().child_layer().big_len()),
        }
    }

    /// Returns the root node of the ND-tree.
    #[inline]
    pub fn root(&self) -> &ArcNode<D> {
        &self.root
    }
    /// Sets the root node of the ND-tree, maintaining the center position.
    ///
    /// # Panics
    ///
    /// This method panics if the new root consists of only a single cell.
    #[inline]
    pub fn set_root_centered(&mut self, new_root: impl Into<ArcNode<D>>) {
        let old_node_layer = self.layer();
        let new_root = new_root.into();
        assert!(
            new_root.layer() > Layer(0),
            "Root of ND-tree must be larger than a single cell",
        );
        self.root = new_root;
        let new_node_layer = self.layer();

        self.offset += &old_node_layer.child_layer().big_len();
        self.offset -= &new_node_layer.child_layer().big_len();
    }

    /// Returns the cache for nodes in the ND-tree.
    #[inline]
    pub fn cache(&self) -> &Arc<RwLock<NodeCache<D>>> {
        self.root().cache()
    }
    /// Returns the layer of the largest node in the ND-tree.
    #[inline]
    pub fn layer(&self) -> Layer {
        self.root().layer()
    }
    /// Returns the length of the grid along one axis.
    #[inline]
    pub fn len(&self) -> BigInt {
        self.layer().big_len()
    }
    /// Returns a lower bound for lowest coordinate in the grid.
    #[inline]
    pub fn offset(&self) -> &BigVec<D> {
        &self.offset
    }
    /// Returns a rectangle encompassing the grid.
    #[inline]
    pub fn rect(&self) -> BigRect<D> {
        self.layer().big_rect() + self.offset()
    }
    /// Returns an `NdTreeSlice` view into the grid.
    #[inline]
    pub fn slice<'cache>(&self, cache: &'cache NodeCache<D>) -> NdTreeSlice<'cache, D> {
        NdTreeSlice {
            root: self.root().as_ref(cache),
            offset: self.offset().clone(),
        }
    }

    /// "Zooms out" the grid by a factor of 2.
    ///
    /// This is done by replacing each child with a larger node containing it in
    /// the opposite corner. For example, the NE child is replaced with a new
    /// node twice the size containing that old NE child in its SW corner. The
    /// final result is that the entire tree contains the same contents as
    /// before, but with 25% padding along each edge.
    pub fn expand(&mut self, cache: &NodeCache<D>) {
        let empty_node = cache.get_empty(self.root().layer().child_layer());
        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        let new_root = cache.join_nodes(
            self.root()
                .as_ref(cache)
                .subdivide()
                .unwrap()
                .into_iter()
                .enumerate()
                .map(|(child_index, subcube)| {
                    // Invert the bits of the child index to get the index
                    // of the opposite child.
                    let opposite_child_index = child_index ^ child_index_bitmask;
                    // All children of this node will be empty ...
                    let mut children = vec![empty_node; D::BRANCHING_FACTOR];
                    // ... except for the opposite child, which will be
                    // closest to the center of the NdTree.
                    children[opposite_child_index] = subcube;
                    cache.join_nodes(children)
                }),
        );
        self.set_root_centered(new_root);
    }
    /// "Zooms out" the grid by calling `NdTree::expand()` until the tree
    /// includes the given position.
    pub fn expand_to(&mut self, cache: &NodeCache<D>, pos: &BigVec<D>) {
        while !self.rect().contains(pos) {
            self.expand(cache);
        }
    }
    /// "Zooms in" the grid as much as possible without losing
    /// non-empty cells. Returns the number of times the tree was shrunk by a
    /// factor of 2.
    pub fn shrink(&mut self, cache: &NodeCache<D>) {
        while self._shrink(cache).is_ok() {}
    }
    /// "Zooms in" the grid by a factor of 2.
    fn _shrink(&mut self, cache: &NodeCache<D>) -> Result<(), ()> {
        // Don't shrink past the base layer.
        let root = self.root().as_ref(cache).as_non_leaf().ok_or(())?;

        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        // Fetch the grandchildren of this node that are closest to the center.
        let new_children: Vec<_> = root
            .children()
            .enumerate()
            .map(|(child_index, child)| {
                // Invert the bits of the child index to get the index of the
                // opposite child, which is the one closest to the center (e.g.
                // SE child of NE child).
                let opposite_child_index = child_index ^ child_index_bitmask;
                // If any grandchild other than the one closest to the center is
                // non-empty, then we can't shrink any more.
                let mut grandchildren = child.subdivide().unwrap();
                for (i, grandchild) in grandchildren.iter().enumerate() {
                    if i != opposite_child_index && !grandchild.is_empty() {
                        return Err(());
                    }
                }
                // Return the grandchild closest to the center.
                Ok(grandchildren.remove(opposite_child_index))
            })
            .try_collect()?;
        let new_root = cache.join_nodes(new_children);
        self.set_root_centered(new_root);
        Ok(())
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, cache: &NodeCache<D>, pos: &BigVec<D>) -> u8 {
        if self.rect().contains(pos) {
            self.root()
                .as_ref(cache)
                .cell_at_pos(&(pos - self.offset()))
        } else {
            0
        }
    }
    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, cache: &NodeCache<D>, pos: &BigVec<D>, cell_state: u8) {
        self.expand_to(cache, &pos);
        let new_root = self
            .root
            .as_ref(cache)
            .set_cell(&(pos - self.offset()), cell_state);
        self.set_root_centered(new_root);
    }

    /// Returns an `NdTreeSlice` of the smallest node in the grid containing the
    /// given rectangle.
    ///
    /// The result may not directly correspond to an existing node; it may be
    /// centered on an existing node, and thus composed out of smaller existing
    /// nodes. The only guarantee is that the node will be either a leaf node or
    /// less than twice as big as it needs to be.
    pub fn slice_containing<'cache>(
        &self,
        cache: &'cache NodeCache<D>,
        rect: &BigRect<D>,
    ) -> NdTreeSlice<'cache, D> {
        // Grow the NdTree until it contains the desired rectangle.
        let mut tmp_ndtree = self.clone();
        tmp_ndtree.expand_to(cache, &rect.min());
        tmp_ndtree.expand_to(cache, &rect.max());
        let mut ret = tmp_ndtree.slice(cache);
        // "Zoom in" on either a corner, an edge/face, or the center until it
        // can't shrink any more. Each iteration of the loop "zooms in" by a
        // factor of 2.
        'outer: loop {
            // If the node is a leaf node, don't zoom in any more; this is small
            // enough.
            if ret.root.is_leaf() {
                break 'outer;
            }
            let root = ret.root.as_non_leaf().unwrap();

            // Since the root is not a leaf node, we know it is at least at
            // layer 2.
            let grandchild_layer = root.layer().child_layer().child_layer();
            // Get the rectangle of grandchildren of the current root that
            // includes the desired rectangle. Each axis is in the range from 0
            // to 3.
            let grandchild_rect: URect<D> =
                ((rect.clone() - &ret.offset) >> grandchild_layer.to_u32()).to_urect();
            let mut new_min = grandchild_rect.min();
            let mut new_max = grandchild_rect.max();
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
                    _ => {
                        break 'outer;
                    }
                }
            }
            let grandchild_square = URect::span(new_min, new_max);

            // Fetch the 2^NDIM nodes that comprise the new node by iterating
            // over the grandchild positions in `grandchild_square`.
            let new_children = grandchild_square.iter().map(|grandchild_pos| {
                root.grandchild_at_index(Layer(2).leaf_cell_index(grandchild_pos))
            });
            // Compute the new offset.
            ret.offset += grandchild_square.min().to_bigvec() << grandchild_layer.to_u32();
            // Create the new node.
            ret.root = cache.join_nodes(new_children);
        }
        ret
    }
}

#[cfg(test)]
mod tests;
