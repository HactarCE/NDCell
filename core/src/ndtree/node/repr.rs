//! Representation of nodes and wrapper type representing a node layer.
//!
//! The `Layer` wrapper type is here because its methods are very much related
//! to how nodes are stored rather than dealing with their actual contents; thus
//! it's mostly related to the representation of ND-tree nodes. Also `raw.rs`
//! would become much longer than this file and it doesn't need another struct.

use std::marker::PhantomData;

use crate::dim::Dim;
use crate::ndrect::{BigRect, URect};
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, One};

/// Arbitrary limit of 64 bytes to represent the cells in each leaf node.
///
/// Since each cell takes up a single byte, this results in the following leaf
/// node size for each number of dimensions:
///
/// - 1D => 64
/// - 2D => 8x8
/// - 3D => 4x4x4
/// - 4D => 2x2x2x2
/// - 5D => 2x2x2x2x2
/// - 6D => 2x2x2x2x2x2
///
/// Even if this value is lowered, 2x2 (or rather, 2^N) is always the minimum
/// size of a leaf node.
const MAX_CELLS_PER_LEAF: usize = 64;

/// Precomputed description of the format used for NdTree nodes.
///
/// This is shared for a whole node cache so that individual nodes and node
/// pointers can omit this information.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct NodeRepr<D: Dim> {
    /// Number of cell states.
    state_count: usize,
    /// Largest layer at which nodes contain cells rather than smaller nodes. In
    /// a large enough ND-tree, this is the minimum node layer.
    base_layer: Layer,
    /// Phantom data for polymorphism.
    _marker: PhantomData<D>,
}
impl<D: Dim> NodeRepr<D> {
    /// Creates the node representation for a simulation with the given number
    /// of dimensions and cell states.
    ///
    /// # Panics
    ///
    /// This function panics if the state count is not within the range `1..=256`.
    pub fn new(state_count: usize) -> Self {
        // Fit as many cells as possible without exceeding MAX_BYTES_PER_LEAF.
        let base_layer = (2..)
            .map(Layer)
            .take_while(|&base_layer| base_layer.num_cells::<D>().unwrap() < MAX_CELLS_PER_LEAF)
            .last()
            // If all the options are too big, use a 2x2 (layer = 1) base node.
            .unwrap_or(Layer(1));

        Self {
            base_layer,
            state_count,
            _marker: PhantomData,
        }
    }

    /// Returns the number of possible cell states, which is always in the range
    /// `1..=256`.
    #[inline]
    pub fn state_count(self) -> usize {
        self.state_count
    }

    /// Returns the largest layer at which ND-tree nodes should be leaf nodes
    /// containing raw cell data; all larger nodes should be non-leaf nodes
    /// containing 2^NDIM smaller nodes.
    #[inline]
    pub fn base_layer(self) -> Layer {
        self.base_layer
    }
}

/// Layer of a node (32-bit unsigned integer).
///
/// The ND-tree is composed of leaf nodes and non-leaf nodes. Every node has a
/// `Layer`, which is the base-2 log of the number of cells along each axis in
/// the node. For example, a 3D node at `Layer(5)` contains a 32x32x32 cube of
/// cells. In the case of leaf nodes, those cells are stored in one array (see
/// the crate's root documentation for more details). Non-leaf nodes contain
/// pointers to 2^NDIM nodes one layer lower.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Layer(pub u32);
impl Into<u32> for Layer {
    #[inline]
    fn into(self) -> u32 {
        self.to_u32()
    }
}
impl Into<usize> for Layer {
    #[inline]
    fn into(self) -> usize {
        self.to_usize()
    }
}
impl std::ops::Add for Layer {
    type Output = Layer;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl std::ops::Sub for Layer {
    type Output = Layer;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}
impl Layer {
    /// Returns the layer number as a `u32`.
    #[inline]
    pub fn to_u32(self) -> u32 {
        self.0
    }
    /// Returns the layer number as a `usize`.
    #[inline]
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
    /// Returns the layer with the given total number of cells exactly, or
    /// `None` if one does not exist.
    #[inline]
    pub fn from_num_cells<D: Dim>(num_cells: usize) -> Option<Self> {
        let len_log_2 = num_cells.trailing_zeros() / D::NDIM as u32;
        if 1 << len_log_2 == num_cells {
            Some(Self(len_log_2))
        } else {
            None
        }
    }

    /// Returns the layer of this node's children.
    #[inline]
    pub fn child_layer(self) -> Layer {
        Layer(
            self.0
                .checked_sub(1)
                .expect("Tried to get layer below minimum"),
        )
    }
    /// Returns the layer of this node's parent.
    #[inline]
    pub fn parent_layer(self) -> Layer {
        Layer(
            self.0
                .checked_add(1)
                .expect("NdTree node layer exceeds maximum"),
        )
    }

    /// Returns the number of cells along each axis of a node at this layer, or
    /// `None` if it does not fit in a `usize`.
    #[inline]
    pub fn len(self) -> Option<usize> {
        crate::math::try_pow_2(self.to_usize())
    }
    /// Returns the number of cells along each axis of a node at this layer as a
    /// `BigInt`.
    #[inline]
    pub fn big_len(self) -> BigInt {
        BigInt::one() << self.to_usize()
    }

    /// Returns the total number of cells of a node at this layer, or `None` if
    /// it does not fit in a `usize`.
    #[inline]
    pub fn num_cells<D: Dim>(self) -> Option<usize> {
        crate::math::try_pow_2(self.to_usize() * D::NDIM)
    }
    /// Returns the number of cells along each axis of a node at this layer as a
    /// `BigInt`.
    #[inline]
    pub fn big_num_cells<D: Dim>(self) -> BigInt {
        BigInt::one() << (self.to_usize() * D::NDIM)
    }

    /// Returns a rectangle the size of a node at this layer with the lower
    /// corner at the origin, or `None` if it does not fit in a `URect`.
    #[inline]
    pub fn rect<D: Dim>(self) -> Option<URect<D>> {
        self.len()
            .map(|len| URect::span(UVec::origin(), UVec::repeat(len - 1)))
    }
    /// Returns a rectangle the size of a node at this layer with the lower
    /// corner at the origin as a `BigRect`.
    #[inline]
    pub fn big_rect<D: Dim>(self) -> BigRect<D> {
        BigRect::span(BigVec::origin(), BigVec::repeat(self.big_len() - 1))
    }

    /// Returns the index of the child of a node at this layer that contains the
    /// given position, modulo the node length along each axis.
    ///
    /// # Panics
    ///
    /// This method panics if the layer is `Layer(0)`, which does not have children.
    #[inline]
    pub fn non_leaf_child_index<D: Dim>(self, pos: &BigVec<D>) -> usize {
        // Extract the single bit of each component that is relevant for this
        // layer.
        let child_layer = self.child_layer();
        let child_index_vector = ((pos >> child_layer.to_u32()) & &BigInt::one()).to_uvec();
        // Pretend this node is just a 2x2 leaf node (layer = 1) but instead of
        // cells, the children are other nodes.
        Layer(1).leaf_cell_index(child_index_vector)
    }

    /// Returns the strides of the cell array for a leaf node at this layer.
    ///
    /// See the documentation at the crate root for more details.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is too high to be a leaf node layer.
    #[inline]
    pub fn leaf_strides<D: Dim>(self) -> UVec<D> {
        UVec::from_fn(|ax| 1 << (self.to_usize() * ax as usize))
    }
    /// Returns the index of the cell at the given position in a leaf node at
    /// this layer, modulo the node length along each axis.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is too high to be a leaf node layer.
    #[inline]
    pub fn leaf_cell_index<D: Dim>(self, pos: UVec<D>) -> usize {
        let pos = pos & (self.len().unwrap() - 1);
        (pos * self.leaf_strides::<D>()).sum()
    }
    /// Returns the vector position of the cell at the given index in a leaf
    /// node at this layer.
    ///
    /// # Panics
    ///
    /// This method panics if `index` is out of range and may panic if the layer
    /// is too high to be a leaf node layer.
    #[inline]
    pub fn leaf_pos<D: Dim>(self, index: usize) -> UVec<D> {
        assert!(index < self.num_cells::<D>().unwrap());
        let pow = self.to_usize();
        let mask = self.len().unwrap() - 1;
        UVec::from_fn(|ax| (index >> (pow * ax as usize)) & mask)
    }

    /// Returns the given position modulo the size of a node at this layer along
    /// each axis.
    #[inline]
    pub fn modulo_pos<D: Dim>(self, pos: &BigVec<D>) -> BigVec<D> {
        pos & &(self.big_len() - 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dim::Dim3D;
    use crate::ndrect::{IRect3D, URect3D};
    use crate::ndvec::{IVec3D, NdVec, UVec3D};

    /// Tests `Layer::non_leaf_child_index()`.
    #[test]
    fn test_ndtree_node_child_index() {
        let layer = Layer(3);
        assert_eq!(8, layer.len().unwrap());
        // This is ~2^16 iterations, which shouldn't take too long to enumerate.
        for pos in IRect3D::centered(IVec3D::origin(), 20).iter() {
            let NdVec([x, y, z]) = pos.div_floor(&4) & 1;
            let expected = (x + 2 * y + 4 * z) as usize;
            let actual = layer.non_leaf_child_index(&pos.to_bigvec());
            assert_eq!(expected, actual);
        }
    }

    /// Tests `Layer::leaf_cell_index()`.
    #[test]
    fn test_ndtree_node_leaf_cell_index() {
        let layer = Layer(3);
        assert_eq!(8, layer.len().unwrap());
        // This is ~2^15 iterations, which shouldn't take too long to enumerate.
        for pos in URect3D::span(UVec3D::origin(), UVec3D::repeat(30)).iter() {
            let NdVec([x, y, z]) = pos & 7;
            let expected = (x + 8 * y + 64 * z) as usize;
            let actual = layer.leaf_cell_index(pos);
            println!("{:?}", pos);
            assert_eq!(expected, actual);
        }
    }

    /// Tests `Layer::leaf_pos()`.
    #[test]
    fn test_ndtree_node_leaf_pos() {
        let layer = Layer(3);
        assert_eq!(8, layer.len().unwrap());

        let rect: URect3D = layer.rect().unwrap();
        assert_eq!(NdVec::repeat(0), rect.min());
        assert_eq!(NdVec::repeat(7), rect.max());
        assert_eq!(rect.count(), layer.num_cells::<Dim3D>().unwrap());

        for (i, pos) in rect.iter().enumerate() {
            assert_eq!(i, layer.leaf_cell_index(pos));
            assert_eq!(pos, layer.leaf_pos(i));
        }
    }
}
