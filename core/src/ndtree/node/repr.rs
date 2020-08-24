// TODO: document this module and explain somewhere (crate root?) that ALL
// n-dimensional arrays use row-major or column-major or whatever order it is;
// also explain strides

use std::marker::PhantomData;
use std::num::NonZeroUsize;

use crate::axis::Axis;
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
    pub fn new(state_count: usize) -> Self {
        // Fit as many cells as possible without exceeding MAX_BYTES_PER_LEAF.
        let base_layer = (2..)
            .map(Layer)
            .take_while(|&base_layer| {
                base_layer.num_cells::<D>().unwrap().get() < MAX_CELLS_PER_LEAF
            })
            .last()
            // If all the options are too big, use a 2x2 (layer = 1) base node.
            .unwrap_or(Layer(1));

        Self {
            base_layer,
            state_count,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn state_count(self) -> usize {
        self.state_count
    }

    #[inline]
    pub fn base_layer(self) -> Layer {
        self.base_layer
    }
}

/// Layer of a node (32-bit unsigned integer).
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Layer(pub u32);
impl Into<u32> for Layer {
    fn into(self) -> u32 {
        self.to_u32()
    }
}
impl Into<usize> for Layer {
    fn into(self) -> usize {
        self.to_usize()
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

    /// Returns the layer below.
    #[inline]
    pub fn child_layer(self) -> Layer {
        Layer(
            self.0
                .checked_sub(1)
                .expect("Tried to get layer below minimum"),
        )
    }
    /// Returns the layer above.
    #[inline]
    pub fn parent_layer(self) -> Layer {
        Layer(
            self.0
                .checked_add(1)
                .expect("NdTree node layer exceeds maximum"),
        )
    }

    /// Returns the number of cells along each axis for a node at this layer, or
    /// `None` if it does not fit in a `usize`.
    #[inline]
    pub fn len(self) -> Option<NonZeroUsize> {
        crate::math::try_pow_2(self.to_usize())
    }
    /// Returns the number of cells along each axis for a node at this layer as
    /// a `BigInt`.
    #[inline]
    pub fn big_len(self) -> BigInt {
        BigInt::one() << self.to_usize()
    }

    /// Returns the total number of cells for a node at this layer, or `None` if
    /// it does not fit in a `usize`.
    #[inline]
    pub fn num_cells<D: Dim>(self) -> Option<NonZeroUsize> {
        crate::math::try_pow_2(self.to_usize() * D::NDIM)
    }
    /// Returns the number of cells along each axis for a node at this layer as
    /// a `BigInt`.
    #[inline]
    pub fn big_num_cells<D: Dim>(self) -> BigInt {
        BigInt::one() << (self.to_usize() * D::NDIM)
    }

    /// Returns a rectangle the size of a node at this layer with the lower
    /// corner at the origin, or `None` if it does not fit in a `URect`.
    #[inline]
    pub fn rect<D: Dim>(self) -> Option<URect<D>> {
        self.len()
            .map(|len| URect::span(UVec::origin(), UVec::repeat(len.get() - 1)))
    }
    /// Returns a rectangle the size of a node at this layer with the lower
    /// corner at the origin as `BigRect`.
    #[inline]
    pub fn big_rect<D: Dim>(self) -> BigRect<D> {
        BigRect::span(BigVec::origin(), BigVec::repeat(self.big_len() - 1))
    }

    /// Returns the index of the child of a node at this layer that contains the
    /// given position, modulo the node length along each axis.
    #[inline]
    pub fn non_leaf_child_index<D: Dim>(self, pos: &BigVec<D>) -> usize {
        todo!("test this");
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
    /// Each element of a strides vector is the number of elements to traverse
    /// to increment that axis by one. To use a strides vector, multiply it
    /// elementwise by a position vector and then add up the components to get a
    /// flat index into the array.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is too high to be a leaf node layer.
    #[inline]
    pub fn leaf_strides<D: Dim>(self) -> UVec<D> {
        UVec::from_fn(|ax| 1 << (self.to_usize() * ax as usize))
    }
    /// Returns the index of the cell at the given position in a leaf node at
    /// this layer.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is too high to be a leaf node layer.
    #[inline]
    pub fn leaf_cell_index<D: Dim>(self, pos: UVec<D>) -> usize {
        todo!("test this");
        let pos = pos & (self.len().unwrap().get() - 1);
        (pos * self.leaf_strides::<D>()).sum()
    }
    /// Returns the vector position of the cell at the given index in a leaf
    /// node at this layer.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is too high to be a leaf node layer.
    #[inline]
    pub fn leaf_pos<D: Dim>(self, index: usize) -> UVec<D> {
        todo!("test this");
        assert!(index < self.num_cells::<D>().unwrap().get());
        UVec::from_fn(|ax| {
            let mask = if ax == Axis::X {
                1 << self.to_usize()
            } else {
                (1 << (self.to_usize() * ax as usize))
                    - (1 << (self.to_usize() * (ax as usize - 1)))
            };
            index & mask
        })
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
