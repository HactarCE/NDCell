//! References to nodes in a pool.

use itertools::{Either, Itertools};
use parking_lot::RwLockReadGuard;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::Ordering::Relaxed;

use super::{Layer, LayerTooSmall, NodePool, RawNode, SimCacheGuard};
use crate::axis::Axis;
use crate::dim::Dim;
use crate::ndrect::{BigRect, CanContain, URect};
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, BigUint};

// Make `NodeRefTrait` a "sealed trait."
// https://rust-lang.github.io/api-guidelines/future-proofing.html#c-sealed
mod private {
    use super::*;

    pub trait Sealed {}
    impl<D: Dim> Sealed for NodeRefEnum<'_, D> {}
    impl<D: Dim> Sealed for LeafNodeRef<'_, D> {}
    impl<D: Dim> Sealed for NonLeafNodeRef<'_, D> {}
    impl<D: Dim> Sealed for NodeRef<'_, D> {}
    impl<D: Dim> Sealed for &NodeRefWithGuard<'_, D> {}
}

/// Common functionality for ND-tree nodes that requires shared access to the
/// node pool.
pub trait NodeRefTrait<'pool>: Copy + private::Sealed {
    /// Number of dimensions.
    type D: Dim;

    /// Returns a `NodeRef` of the node.
    fn as_ref(self) -> NodeRef<'pool, Self::D>;
    /// Returns a `NodeRefEnum` of the node.
    fn as_enum(self) -> NodeRefEnum<'pool, Self::D>;

    /// Returns the pool that the node is stored in.
    fn pool(self) -> &'pool NodePool<Self::D>;
    /// Asserts that both nodes are from the same pool.
    #[inline]
    fn assert_same_pool<'b>(self, other: impl NodeRefTrait<'b, D = Self::D>) {
        assert_eq!(
            self.pool(),
            other.pool(),
            "Attempt to operate on nodes from different pools",
        );
    }

    /// Returns the leaf node wrapped in `Some` if it is a leaf node, or `None`
    /// if it is not.
    #[inline]
    fn as_leaf(self) -> Option<LeafNodeRef<'pool, Self::D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(node) => Some(node),
            NodeRefEnum::NonLeaf(_) => None,
        }
    }
    /// Returns the non-leaf node wrapped in `Some` if it is a non-leaf node, or
    /// `None` if it is a leaf node.
    #[inline]
    fn as_non_leaf(self) -> Option<NonLeafNodeRef<'pool, Self::D>> {
        match self.as_enum() {
            NodeRefEnum::Leaf(_) => None,
            NodeRefEnum::NonLeaf(node) => Some(node),
        }
    }

    /// Returns the layer of the node.
    #[inline]
    fn layer(self) -> Layer {
        self.as_ref().as_raw().layer()
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

    /// Returns the number of cells along each axis of the node as a `BigInt`.
    #[inline]
    fn big_len(self) -> BigInt {
        self.layer().big_len()
    }
    /// Returns the number of cells along each axis of the node as a `BigInt`.
    #[inline]
    fn big_num_cells(self) -> BigUint {
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

    /// Returns `true` if all cells in the node are state #0.
    ///
    /// This is O(1) because each node tracks this information.
    #[inline]
    fn is_empty(self) -> bool {
        self.as_ref().as_raw().is_empty()
    }
    /// Returns the state of all cells in the node, if they are the same, or
    /// `None` otherwise.
    ///
    /// This is O(1) because each node tracks this information.
    #[inline]
    fn single_state(self) -> Option<u8> {
        self.as_ref().as_raw().single_state()
    }

    /// Returns the node one layer below this one that results from simulating
    /// this node some fixed number of generations, or `None` if that result
    /// hasn't been computed yet.
    #[inline]
    fn result(self, guard: &SimCacheGuard<'_, Self::D>) -> Option<NodeRef<'pool, Self::D>> {
        guard.pool().assert_owns_node(self);
        self.as_ref()
            .as_raw()
            .result()
            .map(|res| unsafe { NodeRef::new(self.pool(), res) })
    }
    /// Atomically sets the result of simulating this node for some fixed number
    /// of generations.
    ///
    /// # Panics
    ///
    /// Panics if `result` is from a different node pool.
    #[inline]
    fn set_result<'b>(self, result: Option<impl NodeRefTrait<'b, D = Self::D>>) {
        if let Some(res) = result {
            // Nodes must not contain pointers to nodes in other pools.
            self.assert_same_pool(res);
        }
        unsafe {
            self.as_ref()
                .as_raw()
                .set_result(result.map(|r| r.as_ref().as_raw()))
        };
    }

    /// Returns the cell at the given position, modulo the node length along
    /// each axis.
    fn cell_at_pos(self, pos: &BigVec<Self::D>) -> u8 {
        match self.as_enum() {
            // If this is a leaf node, get the individual cell.
            NodeRefEnum::Leaf(node) => {
                node.leaf_cell_at_pos((pos & BigInt::from(usize::MAX)).to_uvec())
            }
            // If this is not a leaf node, delegate to one of the children.
            NodeRefEnum::NonLeaf(node) => node
                .child_at_index(node.child_index_with_pos(pos))
                .cell_at_pos(pos),
        }
    }

    /// Returns one corner of the node at one layer lower. If the node is only a
    /// single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    ///
    /// This is equivalent to taking one element of the result of `subdivide()`.
    fn get_corner(self, index: usize) -> Result<NodeRef<'pool, Self::D>, u8> {
        self.pool().get_corner(self.as_enum(), index)
    }
    /// Subdivides the node into 2^NDIM smaller nodes at one layer lower. If the
    /// node is only a single cell, returns `Err()` containing that cell state.
    ///
    /// The return value is borrowed if `node` is a non-leaf and owned if `node`
    /// is a leaf.
    #[inline]
    fn subdivide(self) -> Result<Vec<NodeRef<'pool, Self::D>>, u8> {
        self.pool().subdivide(self.as_enum())
    }
    /// Creates a node one layer lower containing the contents of the center of
    /// the node.
    #[inline]
    fn centered_inner(self) -> Result<NodeRef<'pool, Self::D>, LayerTooSmall> {
        self.pool().centered_inner(self.as_enum())
    }
    /// Creates an identical node except with the cell at the given position
    /// (modulo the node length along each axis) modified.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn set_cell(self, pos: &BigVec<Self::D>, cell_state: u8) -> NodeRef<'pool, Self::D> {
        self.pool().set_cell(self.as_enum(), pos, cell_state)
    }

    /// Returns the population of the node.
    #[inline]
    fn population(self) -> BigUint {
        // Record the difference in memory usage.
        let old_heap_size = self.as_ref().as_raw().heap_size();
        let ret = self.as_ref().as_raw().calc_population().into();
        let new_heap_size = self.as_ref().as_raw().heap_size();
        // If the heap size didn't change, don't touch the atomic variable.
        if old_heap_size != new_heap_size {
            // += new_heap_size - old_heap_size
            self.pool()
                .node_heap_size
                .fetch_add(new_heap_size.wrapping_sub(old_heap_size), Relaxed);
        }
        ret
    }

    /// Generates a new node from this one by calling one of the given closures
    /// on its children, recursing if the closure returns `None`. All coordinate
    /// values are with respect to the original root node.
    ///
    /// # Panics
    ///
    /// This method panics if `modify_node` returns a node at a different layer
    /// from the one passed into it.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn recursive_modify(
        self,
        mut modify_node: impl FnMut(
            &BigVec<Self::D>,
            NodeRef<'pool, Self::D>,
        ) -> Option<NodeRef<'pool, Self::D>>,
        mut modify_cell: impl FnMut(&BigVec<Self::D>, u8) -> u8,
    ) -> NodeRef<'pool, Self::D> {
        self.recursive_modify_with_offset(&BigVec::origin(), &mut modify_node, &mut modify_cell)
    }
    /// Same as `recursive_modify()`, but `offset` is added to all coordinate
    /// values before being passed to either closure.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn recursive_modify_with_offset(
        self,
        offset: &BigVec<Self::D>,
        modify_node: &mut impl FnMut(
            &BigVec<Self::D>,
            NodeRef<'pool, Self::D>,
        ) -> Option<NodeRef<'pool, Self::D>>,
        modify_cell: &mut impl FnMut(&BigVec<Self::D>, u8) -> u8,
    ) -> NodeRef<'pool, Self::D> {
        match self.as_enum() {
            NodeRefEnum::Leaf(node) => self.pool().get_from_cells(
                node.cells_with_positions()
                    .map(|(pos, cell)| modify_cell(&(pos.to_bigvec() + offset), cell))
                    .collect_vec(),
            ),
            NodeRefEnum::NonLeaf(node) => {
                self.pool()
                    .join_nodes(node.children().enumerate().map(|(index, old_child)| {
                        let child_offset = self.layer().big_child_offset(index) + offset;
                        let new_child =
                            modify_node(&child_offset, old_child).unwrap_or_else(|| {
                                old_child.recursive_modify_with_offset(
                                    &child_offset,
                                    modify_node,
                                    modify_cell,
                                )
                            });
                        assert_eq!(
                            old_child.layer(),
                            new_child.layer(),
                            "Invalid layer for node in recursive_modify_with_offset()",
                        );
                        new_child
                    }))
            }
        }
    }

    /// Returns `true` if all cells in the node within the rectangle are state
    /// #0, or false otherwise. Returns `true` if the rectangle does not
    /// intersect the node.
    fn rect_is_empty(self, rect: &BigRect<Self::D>) -> bool {
        // Are there even any cells in `self` at all?
        if self.is_empty() {
            return true;
        }

        // Is `self` entirely outside of `rect`?
        let self_rect = self.big_rect();
        let rect = match self_rect.intersection(rect) {
            Some(r) => r,
            None => return true,
        };

        // Is `self` entirely included in `rect`?
        if rect == self_rect {
            // We already checked that `self` is non-empty.
            return false;
        }

        match self.as_enum() {
            // Check cells individually.
            NodeRefEnum::Leaf(node) => {
                // The rectangle is empty if all cells are #0. It is safe to
                // convert to `URect` because this is a leaf node and `rect` is
                // restricted to the bounds of `self`.
                rect.to_urect()
                    .iter()
                    .all(|pos| node.leaf_cell_at_pos(pos) == 0_u8)
            }
            // Delegate to children.
            NodeRefEnum::NonLeaf(node) => {
                // The rectangle is empty if it is empty in all children.
                node.children().enumerate().all(|(index, child)| {
                    child.rect_is_empty(&(rect.clone() - self.layer().big_child_offset(index)))
                })
            }
        }
    }
    /// Returns the smallest rectangle containing all nonzero cells, or `None`
    /// if there are no live cells.
    fn min_nonzero_rect(self) -> Option<BigRect<Self::D>> {
        self.shrink_nonzero_rect(&self.big_rect())
    }
    /// Shrinks a rectangle as much as possible while still containing the same
    /// nonzero cells. Returns `None` if all cells in the rectangle are zero.
    fn shrink_nonzero_rect(self, rect: &BigRect<Self::D>) -> Option<BigRect<Self::D>> {
        Some(BigRect::span(
            BigVec::try_from_fn(|ax| shrink_nonzero_lower_bound(self, rect, ax))?,
            BigVec::try_from_fn(|ax| shrink_nonzero_upper_bound(self, rect, ax))?,
        ))
    }
}

macro_rules! impl_node_ref_eq {
    ($node_ref_type:ident) => {
        impl<'other, D: Dim, T> PartialEq<T> for $node_ref_type<'_, D>
        where
            T: NodeRefTrait<'other, D = D>,
        {
            #[inline]
            fn eq(&self, other: &T) -> bool {
                std::ptr::eq(self.as_ref().as_raw(), other.as_ref().as_raw())
            }
        }
        impl<D: Dim> Eq for $node_ref_type<'_, D> {}
    };
}

/// Enumeration of references to leaf or non-leaf nodes.
#[derive(Debug, Copy, Clone, Hash)]
pub enum NodeRefEnum<'pool, D: Dim> {
    /// Leaf node.
    Leaf(LeafNodeRef<'pool, D>),
    /// Non-leaf node.
    NonLeaf(NonLeafNodeRef<'pool, D>),
}
impl_node_ref_eq!(NodeRefEnum);
impl<'pool, D: Dim> From<LeafNodeRef<'pool, D>> for NodeRefEnum<'pool, D> {
    #[inline]
    fn from(node: LeafNodeRef<'pool, D>) -> Self {
        Self::Leaf(node)
    }
}
impl<'pool, D: Dim> From<NonLeafNodeRef<'pool, D>> for NodeRefEnum<'pool, D> {
    #[inline]
    fn from(node: NonLeafNodeRef<'pool, D>) -> Self {
        Self::NonLeaf(node)
    }
}
impl<'pool, D: Dim> NodeRefTrait<'pool> for NodeRefEnum<'pool, D> {
    type D = D;

    #[inline]
    fn as_ref(self) -> NodeRef<'pool, D> {
        match self {
            NodeRefEnum::Leaf(node) => node.as_ref(),
            NodeRefEnum::NonLeaf(node) => node.as_ref(),
        }
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'pool, D> {
        self
    }
    #[inline]
    fn pool(self) -> &'pool NodePool<D> {
        self.as_ref().pool()
    }
}

/// Reference to a leaf node in a pool.
///
/// Cells are stored in a flattened array in row-major order.
#[derive(Debug, Copy, Clone, Hash)]
pub struct LeafNodeRef<'pool, D: Dim>(NodeRef<'pool, D>);
impl_node_ref_eq!(LeafNodeRef);
impl<'pool, D: Dim> NodeRefTrait<'pool> for LeafNodeRef<'pool, D> {
    type D = D;

    #[inline]
    fn as_ref(self) -> NodeRef<'pool, D> {
        self.0
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'pool, D> {
        self.into()
    }
    #[inline]
    fn pool(self) -> &'pool NodePool<D> {
        self.0.pool()
    }
}
impl<'pool, D: Dim> LeafNodeRef<'pool, D> {
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
    pub fn cells(self) -> &'pool [u8] {
        self.as_ref().as_raw().cell_slice().unwrap()
    }
    /// Returns an iterator over the cells of the node, along with their
    /// positions.
    #[inline]
    pub fn cells_with_positions(self) -> impl 'pool + Iterator<Item = (UVec<D>, u8)> {
        self.rect().iter().zip(self.cells().iter().copied())
    }
    /// Returns the cell at the given position, modulo the node length along
    /// each axis.
    ///
    /// Unlike `cell_at_pos()`, this safely takes a `UVec`.
    #[inline]
    pub fn leaf_cell_at_pos(self, pos: UVec<D>) -> u8 {
        self.cells()[self.pos_to_cell_index(pos)]
    }
}

/// Reference to a non-leaf node in a pool.
#[derive(Debug, Copy, Clone, Hash)]
pub struct NonLeafNodeRef<'pool, D: Dim>(NodeRef<'pool, D>);
impl_node_ref_eq!(NonLeafNodeRef);
impl<'pool, D: Dim> NodeRefTrait<'pool> for NonLeafNodeRef<'pool, D> {
    type D = D;

    #[inline]
    fn as_ref(self) -> NodeRef<'pool, D> {
        self.0
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'pool, D> {
        self.into()
    }
    #[inline]
    fn pool(self) -> &'pool NodePool<D> {
        self.0.pool()
    }
}
impl<'pool, D: Dim> NonLeafNodeRef<'pool, D> {
    /// Returns the index of the child containing the given position, module the
    /// node size along each axis.
    #[inline]
    pub fn child_index_with_pos(self, pos: &BigVec<D>) -> usize {
        self.layer().non_leaf_child_index(pos)
    }

    /// Returns the array of pointers to the node's children, each of which is
    /// one layer below the node.
    #[inline]
    fn raw_children(self) -> &'pool [&'pool RawNode<D>] {
        self.as_ref().as_raw().children_slice().unwrap()
    }
    /// Returns an iterator over the node's children, each of which is one layer
    /// below this node.
    #[inline]
    pub fn children(self) -> impl 'pool + Iterator<Item = NodeRef<'pool, D>> {
        (0..D::BRANCHING_FACTOR).map(move |i| self.child_at_index(i))
    }
    /// Returns a reference to the child node at the given index.
    ///
    /// Panics if the index is not between 0 and 2^NDIM (exclusive).
    #[inline]
    pub fn child_at_index(self, index: usize) -> NodeRef<'pool, D> {
        assert!(index < D::BRANCHING_FACTOR);
        unsafe { NodeRef::new(self.pool(), self.raw_children()[index]) }
    }
    /// Returns a reference to the child node containing the given position,
    /// modulo the node length along each axis.
    #[inline]
    pub fn child_with_pos(self, pos: &BigVec<D>) -> NodeRef<'pool, D> {
        self.child_at_index(self.layer().non_leaf_child_index(pos))
    }
    /// Returns a reference to the grandchild node at the given index.
    ///
    /// # Panics
    ///
    /// This method panics if the index is not between 0 and 4^NDIM (exclusive).
    #[inline]
    pub fn grandchild_at_index(self, index: usize) -> NodeRef<'pool, D> {
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

/// Reference to a node in a pool.
///
/// Hashing and equality are based on the pointer to the node and to the pool,
/// rather than the contents of the node. This relies on only a single
/// "canonical" instance of each unique node in the pool.
#[derive(Copy, Clone)]
pub struct NodeRef<'pool, D: Dim> {
    pool: &'pool NodePool<D>,
    raw_node: &'pool RawNode<D>,
}
impl_node_ref_eq!(NodeRef);
impl<D: Dim> fmt::Debug for NodeRef<'_, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node({:p})", self.raw_node)
    }
}
impl<D: Dim> fmt::Display for NodeRef<'_, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "<empty>")?;
        } else if let Some(single_state) = self.single_state() {
            write!(f, "<#{}>", single_state)?;
        } else {
            match self.as_enum() {
                NodeRefEnum::Leaf(node) => {
                    write!(f, "Leaf{:?}", node.cells())?;
                }
                NodeRefEnum::NonLeaf(node) => {
                    write!(f, "NonLeaf[")?;
                    let mut first = true;
                    for child in node.children() {
                        if first {
                            first = false;
                        } else {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", child)?;
                    }
                    write!(f, "]")?;
                }
            }
        }
        Ok(())
    }
}
impl<D: Dim> Hash for NodeRef<'_, D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pool.hash(state);
        std::ptr::hash(self.raw_node, state);
    }
}
impl<'pool, D: Dim> NodeRefTrait<'pool> for NodeRef<'pool, D> {
    type D = D;

    #[inline]
    fn as_ref(self) -> NodeRef<'pool, D> {
        self
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'pool, D> {
        if self.layer().is_leaf::<D>() {
            LeafNodeRef(self).into()
        } else {
            NonLeafNodeRef(self).into()
        }
    }
    #[inline]
    fn pool(self) -> &'pool NodePool<D> {
        self.pool
    }
}
impl<'pool, D: Dim> NodeRef<'pool, D> {
    /// Creates a reference to a raw node, extending the lifetime of the node
    /// reference to the lifetime of the node pool reference.
    ///
    /// # Safety
    ///
    /// `raw_node` must be a valid node obtained from `pool`.
    pub(super) unsafe fn new(pool: &'pool NodePool<D>, raw_node: &'_ RawNode<D>) -> Self {
        let raw_node = std::mem::transmute::<&'_ RawNode<D>, &'pool RawNode<D>>(raw_node);
        Self { pool, raw_node }
    }

    /// Returns a reference to the raw node structure.
    pub(super) fn as_raw(self) -> &'pool RawNode<D> {
        &self.raw_node
    }
}

/// Reference to a node in a pool, along with either a read guard or a normal
/// reference for that pool.
pub struct NodeRefWithGuard<'pool, D: Dim> {
    pool: NodePoolGuard<'pool, D>,
    raw_node: *const RawNode<D>,
}
impl<D: Dim> PartialEq for NodeRefWithGuard<'_, D> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.raw_node, other.raw_node)
    }
}
impl_node_ref_eq!(NodeRefWithGuard);
impl<D: Dim> fmt::Debug for NodeRefWithGuard<'_, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&NodeRefTrait::as_ref(self), f)
    }
}
impl<'pool, 'node: 'pool, D: Dim> NodeRefTrait<'node> for &'node NodeRefWithGuard<'pool, D> {
    type D = D;

    #[inline]
    fn as_ref(self) -> NodeRef<'node, Self::D> {
        unsafe { NodeRef::new(self.pool(), self.raw_node.as_ref().unwrap()) }
    }
    #[inline]
    fn as_enum(self) -> NodeRefEnum<'node, Self::D> {
        self.as_ref().as_enum()
    }
    #[inline]
    fn pool(self) -> &'node NodePool<Self::D> {
        &*self.pool
    }
}
impl<'pool, D: Dim, N: NodeRefTrait<'pool, D = D>> From<N> for NodeRefWithGuard<'pool, D> {
    fn from(node: N) -> Self {
        Self {
            pool: NodePoolGuard::Ref(node.pool()),
            raw_node: node.as_ref().as_raw() as *const _,
        }
    }
}
impl<'pool, D: Dim> NodeRefWithGuard<'pool, D> {
    /// Creates a reference to a node, extending the lifetime of the original
    /// node reference to the lifetime of the node pool guard.
    ///
    /// # Safety
    ///
    /// `raw_node` must be a valid node obtained from the pool of `pool_guard`.
    pub(super) unsafe fn from_ptr_with_guard(
        pool_guard: RwLockReadGuard<'pool, NodePool<D>>,
        raw_node: &'_ RawNode<D>,
    ) -> Self {
        Self {
            pool: NodePoolGuard::ReadGuard(pool_guard),
            raw_node: raw_node as *const _,
        }
    }

    /// Creates a reference to a node, extending the lifetime of the original
    /// node reference to the lifetime of the node pool guard.
    pub fn with_guard<'node>(
        pool_guard: RwLockReadGuard<'pool, NodePool<D>>,
        node_ref: impl NodeRefTrait<'node, D = D>,
    ) -> Self {
        pool_guard.assert_owns_node(node_ref);
        unsafe { Self::from_ptr_with_guard(pool_guard, node_ref.as_ref().as_raw()) }
    }
}

#[allow(missing_debug_implementations, missing_docs)]
enum NodePoolGuard<'a, D: Dim> {
    ReadGuard(RwLockReadGuard<'a, NodePool<D>>),
    Ref(&'a NodePool<D>),
}
impl<'a, D: Dim> std::ops::Deref for NodePoolGuard<'a, D> {
    type Target = NodePool<D>;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::ReadGuard(p) => p,
            Self::Ref(p) => p,
        }
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

/// Returns the exact lower bound for coordinates of live cells within a
/// rectangle, or `None` if there are no live cells within the rectangle.
fn shrink_nonzero_lower_bound<'pool, D: Dim>(
    node: impl NodeRefTrait<'pool, D = D>,
    rect: &BigRect<D>,
    axis: Axis,
) -> Option<BigInt> {
    let mut hashset = HashSet::new();
    hashset.insert((node.as_ref(), rect.clone()));
    shrink_nonzero_bound::<D, Minimize>(hashset, axis)
}
/// Returns the exact upper bound for coordinates of live cells within a
/// rectangle, or `None` if there are no live cells within the rectangle.
fn shrink_nonzero_upper_bound<'pool, D: Dim>(
    node: impl NodeRefTrait<'pool, D = D>,
    rect: &BigRect<D>,
    axis: Axis,
) -> Option<BigInt> {
    let mut hashset = HashSet::new();
    hashset.insert((node.as_ref(), rect.clone()));
    shrink_nonzero_bound::<D, Maximize>(hashset, axis)
}

/// Utility function for `shrink_nonzero_lower_bound()` and
/// `shrink_nonzero_upper_bound()` that is generalized over lower/upper bounds.
fn shrink_nonzero_bound<D: Dim, M: MinMax>(
    edge_nodes: HashSet<(NodeRef<'_, D>, BigRect<D>)>,
    axis: Axis,
) -> Option<BigInt> {
    // This algorithm is based on the one used by Golly:
    // https://github.com/AlephAlpha/golly/blob/497a432cfbd58e4182eae6b1a95658732c734a09/gollybase/hlifedraw.cpp#L419-L598

    if edge_nodes.is_empty() {
        return None;
    }

    // If `edge_nodes` is empty, then there are no live cells and therefore
    // there is no lower/upper bound. If there is at least one node, get its
    // layer. All nodes in `edge_nodes` must have the same layer.
    let layer = edge_nodes.iter().next()?.0.layer();

    if layer.is_leaf::<D>() {
        // Try to find the "best" value. For example, if we are searching for
        // the minimum X bound, the "best" value is the lowest X coordinate of
        // any nonzero cell.
        edge_nodes
            .into_iter()
            // For each node ...
            .map(|(node, rect_within_node)| {
                assert_eq!(layer, node.layer(), "Node layer mismatch");
                // Return `None` if it's empty.
                if node.is_empty() {
                    return None;
                }
                let rect_within_node = rect_within_node.to_urect();
                node.as_leaf()
                    .unwrap()
                    .cells_with_positions()
                    // Only consider nonzero cells.
                    .filter(|(_pos, cell)| *cell != 0_u8)
                    // Only consider cells inside the rectangle.
                    .filter(|(pos, _cell)| rect_within_node.contains(pos))
                    // Only consider the axis we care about.
                    .map(|(pos, _cell)| pos[axis])
                    // Pick the "best" value.
                    .fold1(M::pick_best)
            })
            // Pick the "best" value from all of those nodes.
            .fold(None, |a, b| match (a, b) {
                (Some(a), Some(b)) => Some(M::pick_best(a, b)),
                (None, x) | (x, None) => x,
            })
            // And convert to `BigInt`.
            .map(BigInt::from)
    } else {
        // Subdivide each node into its children, and partition them into two
        // sets depending on their position along `axis`. If a node in
        // `better_set` contains any cell within the rectangle, is will always
        // be closer to the edge than any cell of a node in `worse_set`.
        let (better_set, worse_set): (HashSet<_>, HashSet<_>) = edge_nodes
            .into_iter()
            .flat_map(|(node, rect_within_node)| {
                assert_eq!(layer, node.layer(), "Node layer mismatch");
                node.as_non_leaf()
                    .unwrap()
                    .children()
                    .enumerate()
                    // Compute the intersection of the rectangle inside the child.
                    .filter_map(move |(index, child)| {
                        let child_offset = layer.big_child_offset(index);
                        let rect_within_child = child
                            .big_rect()
                            .intersection(&(rect_within_node.clone() - &child_offset))?;
                        Some((index, child, rect_within_child))
                    })
            })
            .partition_map(|(index, child, rect_within_child)| {
                if M::is_better(index, index ^ axis.bit()) {
                    // This node goes into the "better" set.
                    Either::Left((child, rect_within_child))
                } else {
                    // This node goes into the "worse" set.
                    Either::Right((child, rect_within_child))
                }
            });
        if let Some(result) = shrink_nonzero_bound::<D, M>(better_set, axis) {
            Some(result + layer.child_layer().big_len() * M::pick_best(0, 1))
        } else if let Some(result) = shrink_nonzero_bound::<D, M>(worse_set, axis) {
            Some(result + layer.child_layer().big_len() * M::pick_worst(0, 1))
        } else {
            None
        }

        // // We will split each edge node into its children. Those that are closer
        // // to the edge we want (e.g. if looking for minimum X bound, those with
        // // a more negative X coordinate) will go in `new_edge_nodes`; those that
        // // are farther from the edge we want (e.g. if looking for a minimum X
        // // bound, those with a more positive X coordinate) will go in
        // // `fallback_edge_nodes`.
        // let mut new_edge_nodes = HashSet::new();
        // let mut new_fallback_edge_nodes = HashSet::new();
        // for (node, rect_within_node) in descendents_along_edge {
        //     assert_eq!(layer, node.layer(), "Node layer mismatch");
        //     node.as_non_leaf().unwrap();
        //     match node.as_enum() {
        //         // Base case: check cells individually.
        //         NodeRefEnum::Leaf(node) => node.cells_with_positions(),
        //         NodeRefEnum::NonLeaf(node) => {}
        //     }
        // }
    }
}

trait MinMax {
    type Opposite: MinMax;

    fn pick_best<T: std::cmp::Ord>(a: T, b: T) -> T;
    fn pick_worst<T: std::cmp::Ord>(a: T, b: T) -> T {
        Self::Opposite::pick_best(a, b)
    }
    fn is_better<T: std::cmp::Ord>(a: T, b: T) -> bool;
}
struct Minimize;
struct Maximize;
impl MinMax for Minimize {
    type Opposite = Maximize;

    fn pick_best<T: std::cmp::Ord>(a: T, b: T) -> T {
        std::cmp::min(a, b)
    }
    fn is_better<T: std::cmp::Ord>(a: T, b: T) -> bool {
        a < b
    }
}
impl MinMax for Maximize {
    type Opposite = Minimize;

    fn pick_best<T: std::cmp::Ord>(a: T, b: T) -> T {
        std::cmp::max(a, b)
    }
    fn is_better<T: std::cmp::Ord>(a: T, b: T) -> bool {
        a > b
    }
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
                i, pos, child_index, grandchild_index,
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
