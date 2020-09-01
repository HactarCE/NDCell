//! References to nodes in a cache.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::{ArcNode, Layer, NodeCache, NodeCow, RawNode};
use crate::dim::Dim;
use crate::ndrect::{BigRect, URect};
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, BigUint};

/// Common functionality for ND-tree nodes.
pub trait NodeRefTrait<'node>: Copy {
    /// Number of dimensions.
    type D: Dim;

    /// Returns a reference to the raw node structure.
    fn as_raw(self) -> &'node Arc<RawNode<Self::D>>;
    /// Returns a `Node` of the node.
    fn as_ref(self) -> NodeRef<'node, Self::D>;
    /// Returns a `NodeRefEnum` of the node.
    fn as_enum(self) -> NodeRefEnum<'node, Self::D>;

    /// Returns the leaf node wrapped in `Some` if it is a leaf node, or `None`
    /// if it is not.
    #[inline]
    fn as_leaf(self) -> Option<LeafNodeRef<'node, Self::D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(node) => Some(node),
            NodeRefEnum::NonLeaf(_) => None,
        }
    }
    /// Returns the non-leaf node wrapped in `Some` if it is a non-leaf node, or
    /// `None` if it is a leaf node.
    #[inline]
    fn as_non_leaf(self) -> Option<NonLeafNodeRef<'node, Self::D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(_) => None,
            NodeRefEnum::NonLeaf(node) => Some(node),
        }
    }
    /// Returns `true` if the node is a leaf node.
    #[inline]
    fn is_leaf(self) -> bool {
        self.as_leaf().is_some()
    }
    /// Returns `true` if the node is a non-leaf node.
    #[inline]
    fn is_non_leaf(self) -> bool {
        self.as_non_leaf().is_some()
    }
    /// Returns `true` if all cells in the node are state #0.
    ///
    /// This is O(1) because each node stores a flag to indicate if it is empty.
    #[inline]
    fn is_empty(self) -> bool {
        self.as_raw().is_empty()
    }

    /// Returns the cache that the node is stored in.
    fn cache(self) -> &'node NodeCache<Self::D>;
    /// Asserts that both nodes are from the same cache.
    #[inline]
    fn assert_same_cache<'b>(self, other: impl NodeRefTrait<'b, D = Self::D>) {
        assert_eq!(
            self.cache(),
            other.cache(),
            "Attempt to operate on nodes from different caches",
        );
    }

    /// Returns the layer of the node.
    #[inline]
    fn layer(self) -> Layer {
        self.as_raw().layer()
    }
    /// Returns the node one layer below this one that results from simulating
    /// this node some fixed number of generations, or `None` if that result
    /// hasn't been computed yet.
    #[inline]
    fn result(self) -> Option<ArcNode<Self::D>> {
        self.as_raw()
            .result()
            .as_ref()
            .map(|res| unsafe { ArcNode::new(self.cache().clone(), Arc::clone(res)) })
    }
    /// Atomically sets the result of simulating this node for some fixed number
    /// of generations.
    ///
    /// # Panics
    ///
    /// Panics if `result` is from a different node cache.
    #[inline]
    fn set_result<'b>(self, result: Option<impl NodeRefTrait<'b, D = Self::D>>) {
        if let Some(res) = result {
            // Nodes must not contain pointers to nodes in other caches.
            self.assert_same_cache(res);
        }
        self.as_raw()
            .set_result(result.map(NodeRefTrait::as_raw).map(Arc::clone));
    }

    /// Returns the number of cells along each axis of the node as a `BigInt`.
    #[inline]
    fn big_len(self) -> BigInt {
        self.layer().big_len()
    }
    /// Returns the number of cells along each axis of the node as a `BigInt`.
    #[inline]
    fn big_num_cells(self) -> BigInt {
        self.layer().big_num_cells::<Self::D>()
    }
    /// Returns a rectangle the size of the node with the lower corner at the
    /// origin as a `BigRect`.
    #[inline]
    fn big_rect(self) -> BigRect<Self::D> {
        self.layer().big_rect()
    }

    /// Returns the given position modulo the size of the node along each axis.
    #[inline]
    fn modulo_pos(self, pos: &BigVec<Self::D>) -> BigVec<Self::D> {
        pos & &(self.big_len() - 1)
    }

    /// Returns the cell at the given position, modulo the node length along
    /// each axis.
    fn cell_at_pos(self, pos: &BigVec<Self::D>) -> u8 {
        match self.as_enum() {
            // If this is a leaf node, the position must be small enough to
            // convert to a UVec.
            NodeRefEnum::Leaf(node) => node.cells()[node.pos_to_cell_index(pos.to_uvec())],
            // If this is not a leaf node, delegate to one of the children.
            NodeRefEnum::NonLeaf(node) => node
                .child_at_index(node.child_index_with_pos(pos))
                .cell_at_pos(pos),
        }
    }

    /// Returns the population of the node.
    #[inline]
    fn population(self) -> BigUint {
        // TODO: cache population
        match self.as_enum() {
            NodeRefEnum::Leaf(node) => node.cells().iter().filter(|&&x| x != 0).count().into(),
            NodeRefEnum::NonLeaf(node) => node.children().map(NodeRefTrait::population).sum(),
        }
    }

    /// Returns one corner of the node at one layer lower. If the node is only a
    /// single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    ///
    /// This is equivalent to taking one element of the result of `subdivide()`.
    fn get_corner(self, index: usize) -> Result<NodeCow<'node, Self::D>, u8> {
        self.cache().get_corner(self.as_enum(), index)
    }
    /// Subdivides the node into 2^NDIM smaller nodes at one layer lower. If the
    /// node is only a single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    #[inline]
    fn subdivide(self) -> Result<Vec<NodeCow<'node, Self::D>>, u8> {
        self.cache().subdivide(self.as_enum())
    }
    /// Creates a node one layer lower containing the contents of the center of
    /// the node.
    #[inline]
    fn centered_inner(self) -> Result<ArcNode<Self::D>, ()> {
        self.cache().centered_inner(self.as_enum())
    }
    /// Creates an identical node except with the cell at the given position
    /// (modulo the node length along each axis) modified.
    #[inline]
    fn set_cell(self, pos: &BigVec<Self::D>, cell_state: u8) -> ArcNode<Self::D> {
        self.cache().set_cell(self.as_enum(), pos, cell_state)
    }
}

/// Enumeration of references to leaf or non-leaf nodes.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeRefEnum<'node, D: Dim> {
    /// Leaf node.
    Leaf(LeafNodeRef<'node, D>),
    /// Non-leaf node.
    NonLeaf(NonLeafNodeRef<'node, D>),
}
impl<'node, D: Dim> From<LeafNodeRef<'node, D>> for NodeRefEnum<'node, D> {
    #[inline]
    fn from(node: LeafNodeRef<'node, D>) -> Self {
        Self::Leaf(node)
    }
}
impl<'node, D: Dim> From<NonLeafNodeRef<'node, D>> for NodeRefEnum<'node, D> {
    #[inline]
    fn from(node: NonLeafNodeRef<'node, D>) -> Self {
        Self::NonLeaf(node)
    }
}
impl<'node, D: Dim> NodeRefTrait<'node> for NodeRefEnum<'node, D> {
    type D = D;

    #[inline]
    fn as_raw(self) -> &'node Arc<RawNode<D>> {
        self.as_ref().as_raw()
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'node, D> {
        match self {
            NodeRefEnum::Leaf(node) => node.as_ref(),
            NodeRefEnum::NonLeaf(node) => node.as_ref(),
        }
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, D> {
        self
    }
    #[inline]
    fn cache(self) -> &'node NodeCache<D> {
        self.as_ref().cache()
    }
}

/// Reference to a leaf node in a cache.
///
/// Cells are stored in a flattened array in row-major order.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LeafNodeRef<'node, D: Dim>(NodeRef<'node, D>);
impl<'node, D: Dim> NodeRefTrait<'node> for LeafNodeRef<'node, D> {
    type D = D;

    #[inline]
    fn as_raw(self) -> &'node Arc<RawNode<D>> {
        self.0.as_raw()
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'node, D> {
        self.0
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, D> {
        self.into()
    }
    #[inline]
    fn cache(self) -> &'node NodeCache<D> {
        self.0.cache()
    }
}
impl<'node, D: Dim> LeafNodeRef<'node, D> {
    /// Returns the flattened cell index corresponding to the given position in
    /// a leaf node, modulo the node length along each axis.
    #[inline]
    pub fn pos_to_cell_index(self, pos: UVec<D>) -> usize {
        self.layer().leaf_cell_index(pos)
    }
    /// Returns the vector position corresponding to the given cell index in a
    /// leaf node.
    ///
    /// Panics if the index is out of range.
    #[inline]
    pub fn cell_index_to_pos(self, index: usize) -> UVec<D> {
        self.layer().leaf_pos(index)
    }

    /// Returns the number of cells along each axis of the node.
    #[inline]
    pub fn len(self) -> usize {
        self.layer().len().unwrap_or_else(|| unreachable!())
    }
    /// Returns the total number of cells in the node.
    #[inline]
    pub fn num_cells(self) -> usize {
        self.cells().len()
    }
    /// Returns a rectangle the size of the node with the lower corner at the
    /// origin.
    #[inline]
    pub fn rect(self) -> URect<D> {
        self.layer().rect().unwrap_or_else(|| unreachable!())
    }

    /// Returns the strides of the cell array for a leaf node at this layer.
    ///
    /// See the documentation at the crate root for more details.
    #[inline]
    pub fn strides(self) -> UVec<D> {
        self.layer().leaf_strides()
    }

    /// Returns the cells of the node in a flattened array.
    ///
    /// See the documentation at the crate root for more details.
    #[inline]
    pub fn cells(self) -> &'node [u8] {
        self.as_raw().cell_slice().unwrap()
    }
}

/// Reference to a non-leaf node in a cache.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct NonLeafNodeRef<'node, D: Dim>(NodeRef<'node, D>);
impl<'node, D: Dim> NodeRefTrait<'node> for NonLeafNodeRef<'node, D> {
    type D = D;

    #[inline]
    fn as_raw(self) -> &'node Arc<RawNode<D>> {
        self.0.as_raw()
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'node, D> {
        self.0
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, D> {
        self.into()
    }
    #[inline]
    fn cache(self) -> &'node NodeCache<D> {
        self.0.cache()
    }
}
impl<'node, D: Dim> NonLeafNodeRef<'node, D> {
    /// Returns the index of the child containing the given position, module the
    /// node size along each axis.
    #[inline]
    pub fn child_index_with_pos(self, pos: &BigVec<D>) -> usize {
        self.layer().non_leaf_child_index(pos)
    }

    /// Returns the array of pointers to the node's children, each of which is
    /// one layer below the node.
    #[inline]
    pub fn raw_children(self) -> &'node [Arc<RawNode<D>>] {
        self.as_raw().children_slice().unwrap()
    }
    /// Returns an iterator over the node's children, each of which is one layer
    /// below this node.
    #[inline]
    pub fn children(self) -> impl 'node + Iterator<Item = NodeRef<'node, D>> {
        (0..D::BRANCHING_FACTOR).map(move |i| self.child_at_index(i))
    }
    /// Returns a reference to the child node at the given index.
    ///
    /// Panics if the index is not between 0 and 2^NDIM (exclusive).
    #[inline]
    pub fn child_at_index(self, index: usize) -> NodeRef<'node, D> {
        assert!(index < D::BRANCHING_FACTOR);
        unsafe { NodeRef::new(self.cache(), &self.raw_children()[index]) }
    }
    /// Returns a reference to the child node containing the given position,
    /// modulo the node length along each axis.
    #[inline]
    pub fn child_with_pos(self, pos: &BigVec<D>) -> NodeRef<'node, D> {
        self.child_at_index(self.layer().non_leaf_child_index(pos))
    }
    /// Returns a reference to the grandchild node at the given index.
    ///
    /// # Panics
    ///
    /// This method panics if the index is not between 0 and 4^NDIM (exclusive).
    #[inline]
    pub fn grandchild_at_index(self, index: usize) -> NodeCow<'node, D> {
        assert!(index < D::BRANCHING_FACTOR * D::BRANCHING_FACTOR);
        let (child_index, grandchild_index) = split_grandchild_index::<D>(index);
        let child = self.child_at_index(child_index);
        match child.as_ref().as_enum() {
            // TODO: optimize by only creating the child requested, not all
            // 2^NDIM
            NodeRefEnum::Leaf(node) => node.subdivide().unwrap().remove(grandchild_index),
            NodeRefEnum::NonLeaf(node) => node.child_at_index(grandchild_index).into(),
        }
    }
}

/// Reference to a node in a cache.
///
/// Hashing and equality are based on the pointer to the node and to the cache,
/// rather than the contents of the node. This relies on only a single
/// "canonical" instance of each unique node in the cache.
#[derive(Copy, Clone)]
pub struct NodeRef<'node, D: Dim> {
    cache: &'node NodeCache<D>,
    raw_node: &'node Arc<RawNode<D>>,
}
impl<D: Dim> fmt::Debug for NodeRef<'_, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node({:p})", self.raw_node)
    }
}
impl<D: Dim> PartialEq for NodeRef<'_, D> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.raw_node, &other.raw_node) && self.cache == other.cache
    }
}
impl<D: Dim> Eq for NodeRef<'_, D> {}
impl<D: Dim> Hash for NodeRef<'_, D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cache.hash(state);
        std::ptr::hash(Arc::as_ptr(&self.raw_node), state);
    }
}
impl<'node, D: Dim> NodeRefTrait<'node> for NodeRef<'node, D> {
    type D = D;

    #[inline]
    fn as_raw(self) -> &'node Arc<RawNode<D>> {
        &self.raw_node
    }
    #[inline]
    fn as_ref(self) -> NodeRef<'node, D> {
        self
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, D> {
        if self.layer().is_leaf::<D>() {
            LeafNodeRef(self).into()
        } else {
            NonLeafNodeRef(self).into()
        }
    }
    #[inline]
    fn cache(self) -> &'node NodeCache<D> {
        self.cache
    }
}
impl<'node, D: Dim> NodeRef<'node, D> {
    /// Creates a reference to a raw node.
    ///
    /// # Safety
    ///
    /// `raw_node` must be a valid node obtained from `cache`.
    pub unsafe fn new(cache: &'node NodeCache<D>, raw_node: &'node Arc<RawNode<D>>) -> Self {
        Self { cache, raw_node }
    }
}

/// Splits a grandchild index (in the range from 0 to 4^NDIM, exclusive) into
/// two child indices (each in the range from 0 to 2^NDIM, exclusive). The first
/// is the index into a node to get the child, and the second is the index into
/// that child to get the grandchild.
fn split_grandchild_index<D: Dim>(index: usize) -> (usize, usize) {
    let grandchild_pos = Layer(2).leaf_pos::<D>(index);

    // Use the upper bit along each axis.
    let child_index = Layer(1).leaf_cell_index(grandchild_pos.clone() >> 1);
    // Use the lower bit along each axis.
    let grandchild_index = Layer(1).leaf_cell_index(grandchild_pos & 1);

    (child_index, grandchild_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dim::Dim3D;

    /// Tests `split_grandchild_index()`.
    #[test]
    fn test_ndtree_node_split_grandchild_index() {
        for (i, pos) in Layer(2).rect::<Dim3D>().unwrap().iter().enumerate() {
            let (child_index, grandchild_index) = split_grandchild_index::<Dim3D>(i);
            println!(
                "{:?}; pos {:?}; ch {:?}; grch {:?}",
                i, pos, child_index, grandchild_index
            );
            for &ax in Dim3D::axes() {
                if pos[ax] < 2 {
                    assert_eq!(0, (child_index >> ax as usize) & 1);
                } else {
                    assert_eq!(1, (child_index >> ax as usize) & 1);
                }

                assert_eq!(pos[ax] & 1, (grandchild_index >> ax as usize) & 1);
            }
        }
    }
}
