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

use itertools::{izip, Itertools};
use std::fmt;

pub mod aliases;
mod flat;
mod node;
mod region;
mod slice;

use crate::dim::*;
use crate::ndrect::{BigRect, CanContain, URect};
use crate::ndvec::BigVec;
use crate::num::BigInt;
pub use aliases::*;
pub use flat::{FlatNdTree, FlatNdTreeNode};
pub use node::*;
pub use region::Region;
pub use slice::NdTreeSlice;

/// An N-dimensional generalization of a quadtree.
///
/// Note that an `NdTree` cannot be smaller than a base node.
#[derive(Debug, Clone)]
pub struct NdTree<D: Dim> {
    /// The root node of this slice.
    root: ArcNode<D>,
    /// Position of the lowest corner of the root node.
    base_pos: BigVec<D>,
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
        fmt::Display::fmt(&self.as_slice(), f)
    }
}

impl<'a, D: Dim> From<NdTreeSlice<'a, D>> for NdTree<D> {
    fn from(slice: NdTreeSlice<'a, D>) -> Self {
        Self {
            root: ArcNode::from(slice.root.as_ref()),
            base_pos: slice.base_pos,
        }
    }
}

impl<D: Dim> NdTree<D> {
    /// Creates an empty ND-tree using a new node pool.
    #[inline]
    pub fn new() -> Self {
        Self::with_node_pool(SharedNodePool::new())
    }
    /// Creates an empty ND-tree using the given node pool.
    #[inline]
    pub fn with_node_pool(node_pool: SharedNodePool<D>) -> Self {
        Self::from_node_centered(node_pool.access().get_empty_base())
    }
    /// Creates an empty ND-tree centered at the given position.
    #[inline]
    pub fn with_center(center: BigVec<D>) -> Self {
        let mut ret = Self::new();
        ret.set_center_pos(center);
        ret
    }
    /// Creates an ND-tree containing the given node centered on the origin.
    ///
    /// # Panics
    ///
    /// This function panics if the node consists of only a single cell.
    #[inline]
    pub fn from_node_centered<'n>(node: impl NodeRefTrait<'n, D = D>) -> Self {
        assert!(
            node.layer() > Layer(0),
            "Root of ND-tree must be larger than a single cell",
        );
        Self {
            root: node.into(),
            base_pos: BigVec::repeat(-node.layer().child_layer().big_len()),
        }
    }
    /// Creates an ND-tree containing the given node centered on the given
    /// position.
    ///
    /// # Panics
    ///
    /// This function panics if the node consists of only a single cell.
    #[inline]
    pub fn from_node_centered_on<'n>(
        node: impl NodeRefTrait<'n, D = D>,
        base_pos: BigVec<D>,
    ) -> Self {
        assert!(
            node.layer() > Layer(0),
            "Root of ND-tree must be larger than a single cell",
        );
        Self {
            root: node.into(),
            base_pos: base_pos + BigVec::repeat(-node.layer().child_layer().big_len()),
        }
    }

    /// Returns the root node of the ND-tree.
    #[inline]
    pub fn root(&self) -> &ArcNode<D> {
        &self.root
    }
    /// Returns the root node of the ND-tree.
    #[inline]
    pub fn root_ref(&self) -> NodeRefWithGuard<'_, D> {
        self.root().as_ref_with_guard()
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

        self.base_pos += &old_node_layer.child_layer().big_len();
        self.base_pos -= &new_node_layer.child_layer().big_len();
    }

    /// Returns the node pool for the ND-tree.
    #[inline]
    pub fn pool(&self) -> &SharedNodePool<D> {
        self.root().pool()
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
    /// Returns the lowest coordinate of the root node.
    #[inline]
    pub fn base_pos(&self) -> &BigVec<D> {
        &self.base_pos
    }
    /// Sets the lowest coordinate of the root node. This changes cell
    /// coordinates.
    #[inline]
    pub fn set_base_pos(&mut self, base_pos: BigVec<D>) {
        self.base_pos = base_pos;
    }
    /// Returns the position of the center of the root node of the ND-tree.
    #[inline]
    pub fn center_pos(&self) -> BigVec<D> {
        self.base_pos.clone() + self.layer().child_layer().big_len()
    }
    /// Sets the center of the root node of the grid. This changes cell
    /// coordinates.
    #[inline]
    pub fn set_center_pos(&mut self, center_pos: BigVec<D>) {
        self.base_pos += center_pos.clone() - self.center_pos();
        assert_eq!(center_pos, self.center_pos());
    }
    /// Returns a rectangle encompassing the grid.
    #[inline]
    pub fn rect(&self) -> BigRect<D> {
        self.layer().big_rect() + self.base_pos()
    }

    /// "Zooms out" the grid by a factor of 2.
    ///
    /// This is done by replacing each child with a larger node containing it in
    /// the opposite corner. For example, the NE child is replaced with a new
    /// node twice the size containing that old NE child in its SW corner. The
    /// final result is that the entire tree contains the same contents as
    /// before, but with 25% padding along each edge.
    pub fn expand(&mut self) {
        let root = self.root_ref();
        let node_pool = root.pool();
        let empty_node = node_pool.get_empty(self.layer().child_layer());

        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        let new_root = node_pool.join_nodes(root.subdivide().unwrap().into_iter().enumerate().map(
            |(child_index, subcube)| {
                // Invert the bits of the child index to get the index
                // of the opposite child.
                let opposite_child_index = child_index ^ child_index_bitmask;
                // All children of this node will be empty ...
                let mut children = vec![empty_node; D::BRANCHING_FACTOR];
                // ... except for the opposite child, which will be
                // closest to the center of the NdTree.
                children[opposite_child_index] = subcube;
                node_pool.join_nodes(children)
            },
        ));

        let new_root = ArcNode::from(new_root);
        drop(root);
        self.set_root_centered(new_root);
    }
    /// "Zooms out" the grid by calling `NdTree::expand()` until the tree
    /// includes the given position or rectangle.
    pub fn expand_to<T>(&mut self, pos_or_rect: &T)
    where
        BigRect<D>: CanContain<T>,
    {
        self.expand_while(|this| !this.rect().contains(pos_or_rect))
    }
    /// "Zooms out" the grid by calling `NdTree::expand()` as long as the given
    /// predicate returns `true`.
    pub fn expand_while(&mut self, mut predicate: impl FnMut(&Self) -> bool) {
        while predicate(self) {
            self.expand();
        }
    }
    /// "Zooms in" the grid as much as possible without losing
    /// non-empty cells. Returns the number of times the tree was shrunk by a
    /// factor of 2.
    pub fn shrink(&mut self) {
        while self.try_shrink().is_ok() {}
    }
    /// "Zooms in" the grid by a factor of 2.
    fn try_shrink(&mut self) -> Result<(), Unshrinkable> {
        // Don't shrink past the base layer.
        let root = self.root_ref();
        let root_children = root
            .as_non_leaf()
            .ok_or(Unshrinkable::LayerTooSmall)?
            .children();

        let child_index_bitmask = D::BRANCHING_FACTOR - 1;
        // Fetch the grandchildren of this node that are closest to the center.
        let new_children: Vec<_> = root_children
            .enumerate()
            .map(|(child_index, child)| {
                // Invert the bits of the child index to get the index of the
                // opposite child, which is the one closest to the center (e.g.,
                // SE child of NE child).
                let opposite_child_index = child_index ^ child_index_bitmask;
                // If any grandchild other than the one closest to the center is
                // non-empty, then we can't shrink any more.
                let mut grandchildren = child.subdivide().unwrap();
                for (i, grandchild) in grandchildren.iter().enumerate() {
                    if i != opposite_child_index && !grandchild.is_empty() {
                        return Err(Unshrinkable::PatternTooBig);
                    }
                }
                // Return the grandchild closest to the center.
                Ok(grandchildren.remove(opposite_child_index))
            })
            .try_collect()?;
        let new_root = root.pool().join_nodes(new_children);

        let new_root = ArcNode::from(new_root);
        drop(root);
        self.set_root_centered(new_root);
        Ok(())
    }

    /// Returns the state of the cell at the given position.
    pub fn get_cell(&self, pos: &BigVec<D>) -> u8 {
        if self.rect().contains(pos) {
            self.root_ref().cell_at_pos(&(pos - self.base_pos()))
        } else {
            0
        }
    }
    /// Sets the state of the cell at the given position.
    pub fn set_cell(&mut self, pos: &BigVec<D>, cell_state: u8) {
        self.expand_to(pos);
        let root = self.root_ref();
        let new_root = root.set_cell(&(pos - self.base_pos()), cell_state);

        let new_root = ArcNode::from(new_root);
        drop(root);
        self.set_root_centered(new_root);
    }

    /// Returns an `NdTreeSlice` view into the grid.
    #[inline]
    pub fn as_slice(&self) -> NdTreeSlice<'_, D> {
        NdTreeSlice {
            root: self.root_ref(),
            base_pos: self.base_pos().clone(),
        }
    }
    /// Returns an `NdTreeSlice` of the smallest node in the grid containing the
    /// given rectangle.
    ///
    /// The result may not directly correspond to an existing node; it may be
    /// centered on an existing node, and thus composed out of smaller existing
    /// nodes. The only guarantee is that the node will be either a leaf node or
    /// less than twice as big as it needs to be.
    pub fn slice_containing<'a>(&'a self, rect: &BigRect<D>) -> NdTreeSlice<'a, D> {
        let node_pool = self.pool().access();

        // Grow the ND-tree until it contains the desired rectangle.
        let mut tmp_ndtree = self.clone();
        tmp_ndtree.expand_to(rect);
        let mut ret = tmp_ndtree.as_slice();

        // "Zoom in" on either a corner, an edge/face, or the center until it
        // can't shrink any more. Each iteration of the loop "zooms in" by a
        // factor of 2.
        'outer: loop {
            // If the node is a leaf node, don't zoom in any more; this is small
            // enough.
            if ret.root.is_leaf() {
                break 'outer;
            }
            let root = node_pool.extend_lifetime(&ret.root).as_non_leaf().unwrap();

            // Since the root is not a leaf node, we know it is at least at
            // layer 2.
            let grandchild_layer = root.layer().child_layer().child_layer();
            // Get the rectangle of grandchildren of the current root that
            // includes the desired rectangle. Each axis is in the range from 0
            // to 3.
            let grandchild_rect: URect<D> =
                ((rect.clone() - &ret.base_pos) >> grandchild_layer.to_u32()).to_urect();
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
            // Compute the new base position.
            ret.base_pos += grandchild_square.min().to_bigvec() << grandchild_layer.to_u32();
            // Create the new node.
            ret.root = NodeRefWithGuard::from(node_pool.join_nodes(new_children));
        }

        // Recreate the `NdTreeSlice` with a different lifetime.
        NdTreeSlice {
            root: NodeRefWithGuard::with_guard(self.pool().access(), ret.root.as_ref()),
            base_pos: ret.base_pos,
        }
    }

    /// Returns `true` if all cells within the rectangle are state #0.
    pub fn rect_is_empty(&self, rect: BigRect<D>) -> bool {
        self.root_ref().rect_is_empty(&(rect - self.base_pos()))
    }
    /// Returns the smallest rectangle containing all nonzero cells, or `None`
    /// if there are no live cells.
    pub fn bounding_rect(&self) -> Option<BigRect<D>> {
        self.root_ref()
            .min_nonzero_rect()
            .map(|r| r + self.base_pos())
    }
    /// Shrinks a rectangle as much as possible while still containing the same
    /// nonzero cells. Returns `None` if all cells in the rectangle are zero.
    pub fn shrink_nonzero_rect(&self, rect: BigRect<D>) -> Option<BigRect<D>> {
        self.root_ref()
            .shrink_nonzero_rect(&(rect - self.base_pos()))
            .map(|r| r + self.base_pos())
    }

    /// Recenters the ND-tree with the same cell contents at each position, but
    /// a different origin.
    ///
    /// This may take a lot of time for large patterns when the difference
    /// between `new_center` and the existing ND-tree center is not a multiple
    /// of a large power of 2 (relative to pattern size).
    pub fn recenter(&mut self, new_center: &BigVec<D>) {
        let delta = new_center - self.center_pos();
        let max_abs_delta = delta.abs().max_component().clone();

        self.base_pos += &delta;
        assert_eq!(*new_center, self.center_pos());

        // Expand until half the size of the root node is smaller than (or equal
        // to) the delta vector.
        self.expand_while(|this| max_abs_delta > this.len() / 2);
        let root = self.root_ref();
        let node_pool = root.pool();
        let layer = root.layer(); // Let's call this layer L.

        let mut source_lower_corner = vec![node_pool.get_empty(layer); D::BRANCHING_FACTOR];
        source_lower_corner[D::BRANCHING_FACTOR - 1] = root.as_ref();
        let source_lower_corner = node_pool.join_nodes(source_lower_corner);
        // `source_lower_corner` is now a node at layer L+1 with the tree's root
        // in the "upper" corner (more negative coordinates along all axes).
        // Here's a picture for 2D, where `#` represents the ND-tree's root node
        // and `.` represents an empty node of the same size:
        //
        // . #
        // . .

        let mut sources = vec![node_pool.get_empty(layer.parent_layer()); D::BRANCHING_FACTOR];
        sources[0] = source_lower_corner;
        // `sources` is now a vector of nodes which combined represent a node at
        // layer L+2 with the ND-tree's root node in the upper corner of the
        // lower corner. Here's a picture for 2D, with the same symbols:
        //
        // . . . .
        // . . . .
        // . # . .
        // . . . .
        //
        // We will use `get_offset_child`, which will extract a rectangle of
        // cells from that picture at one layer above the current root node.
        // This result will be the new root node.
        //
        // If we wanted to keep the same center (i.e., `delta` = 0) we would
        // pass an offset of `root.big_len() / 2` along each axis, because that
        // would center the `#` in the picture. To move the center any other
        // distance, we just add the `delta` vector.
        let new_root = node_pool.get_offset_child(&(delta + &(root.big_len() / 2)), sources);

        let new_root = ArcNode::from(new_root);
        drop(root);
        self.set_root_centered(new_root);
    }

    /// Sets a region of cells to state #0.
    pub fn clear_region(&mut self, region: Region<D>) {
        self.paste_custom(
            NdTree::with_node_pool(self.pool().new_ref()),
            region,
            |_original, empty| Some(empty),
            |_original, empty| empty,
        );
    }

    /// Returns a region of cells from this ND-tree as a new ND-tree.
    pub fn get_region(&self, region: Region<D>) -> NdTree<D> {
        let mut ret = NdTree::with_node_pool(self.pool().new_ref());
        ret.paste_custom(
            self.clone(),
            region,
            |_empty, original| Some(original),
            |_empty, original| original,
        );
        ret
    }

    /// Pastes a region from another ND-tree onto this one, using custom
    /// functions to paste a full node or cell. Each closure is passed the
    /// node/cell from `self` followed by the node/cell from `other`, and must
    /// return the new node/cell (or `None` to subdivide the nodes further). The
    /// closure is only passed nodes/cells that are completely covered by the
    /// region.
    ///
    /// Note that `self`, `other`, `mask`, and the nodes returned from the
    /// provided closure do **not** need to use the same node pool; the returned
    /// node will be from the same pool as `self`.
    ///
    /// # Panics
    ///
    /// This method panics if `self`, `other`, and `mask` are not all at the
    /// same layer, if `paste_full_node` returns a node at a different layer
    /// than the ones passed into it.
    pub fn paste_custom(
        &mut self,
        mut other: NdTree<D>,
        mask: Region<D>,
        mut paste_full_node: impl for<'node> FnMut(
            NodeRef<'node, D>,
            NodeRef<'node, D>,
        ) -> Option<NodeRef<'node, D>>,
        mut paste_cell: impl FnMut(u8, u8) -> u8,
    ) {
        // Ensure same center.
        other.recenter(&self.center_pos());
        let mut mask = mask.into_ndtree(self.center_pos());
        assert_eq!(self.center_pos(), other.center_pos());
        assert_eq!(self.center_pos(), mask.center_pos());

        // Ensure same layer.
        let common_layer = std::cmp::max(std::cmp::max(self.layer(), other.layer()), mask.layer());
        self.expand_while(|ndtree| ndtree.layer() < common_layer);
        other.expand_while(|ndtree| ndtree.layer() < common_layer);
        mask.expand_while(|ndtree| ndtree.layer() < common_layer);
        assert_eq!(self.layer(), other.layer());
        assert_eq!(self.layer(), mask.layer());

        let self_root = self.root_ref();
        let other_root = other.root_ref();
        let mask_root = mask.root_ref();

        let new_root = Self::_paste_custom(
            self_root.as_ref(),
            other_root.as_ref(),
            mask_root.as_ref(),
            &mut paste_full_node,
            &mut paste_cell,
        );
        assert_eq!(self.layer(), new_root.layer());

        let new_root = ArcNode::from(new_root);
        drop(self_root);
        self.set_root_centered(new_root);
        self.shrink();
    }
    fn _paste_custom<'dest, 'src, 'mask>(
        destination: NodeRef<'dest, D>,
        source: NodeRef<'src, D>,
        mask: NodeRef<'mask, D>,
        paste_full_node: &mut impl for<'node> FnMut(
            NodeRef<'node, D>,
            NodeRef<'node, D>,
        ) -> Option<NodeRef<'node, D>>,
        paste_cell: &mut impl FnMut(u8, u8) -> u8,
    ) -> NodeRef<'dest, D> {
        match mask.single_state() {
            // The mask completely excludes this node.
            Some(0_u8) => return destination,
            // The mask completely includes this node.
            Some(1_u8) => {
                if let Some(pasted_result) = paste_full_node(destination, source) {
                    return destination.pool().copy_from_other_pool(pasted_result);
                }
                // If `paste_full_node()` returned `None`, then we must recurse
                // further.
            }
            // If the mask only partially covers this node, we must recurse
            // further.
            _ => (),
        }

        // Recurse!
        let dest_node_pool = destination.pool();
        use NodeRefEnum::{Leaf, NonLeaf};
        match (destination.as_enum(), source.as_enum(), mask.as_enum()) {
            (Leaf(dest), Leaf(src), Leaf(mask)) => dest_node_pool.get_from_cells(
                izip!(dest.cells(), src.cells(), mask.cells())
                    .map(|(&dest_cell, &src_cell, &mask_cell)| {
                        if mask_cell == 0_u8 {
                            dest_cell
                        } else {
                            paste_cell(dest_cell, src_cell)
                        }
                    })
                    .collect_vec(),
            ),
            (NonLeaf(dest), NonLeaf(src), NonLeaf(mask)) => dest_node_pool.join_nodes(
                izip!(dest.children(), src.children(), mask.children()).map(
                    |(dest_child, src_child, mask_child)| {
                        Self::_paste_custom(
                            dest_child,
                            src_child,
                            mask_child,
                            paste_full_node,
                            paste_cell,
                        )
                    },
                ),
            ),
            _ => panic!("layer mismatch"),
        }
    }
}

/// Error returned when an ND-tree cannot be shrunk further.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Unshrinkable {
    LayerTooSmall,
    PatternTooBig,
}

#[cfg(test)]
mod tests;
