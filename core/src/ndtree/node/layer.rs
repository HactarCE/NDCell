//! Wrapper type representing the layer of a node.

use crate::dim::Dim;
use crate::ndrect::{BigRect, URect};
use crate::ndvec::{BigVec, UVec};
use crate::num::{BigInt, BigUint, One, ToPrimitive};

/// Layer of a node (32-bit unsigned integer).
///
/// The ND-tree is composed of leaf nodes and non-leaf nodes. Every node has a
/// `Layer`, which is the base-2 log of the number of cells along each axis in
/// the node. For example, a 3D node at `Layer(5)` contains a 32x32x32 cube of
/// cells. In the case of leaf nodes, those cells are stored in one array (see
/// the crate's root documentation for more details). Non-leaf nodes contain
/// pointers to 2^NDIM nodes one layer lower.
#[derive(Debug, Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Layer(pub u32);
impl From<Layer> for u32 {
    #[inline]
    fn from(l: Layer) -> u32 {
        l.to_u32()
    }
}
impl From<Layer> for usize {
    #[inline]
    fn from(l: Layer) -> usize {
        l.to_usize()
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
    pub const fn to_u32(self) -> u32 {
        self.0
    }
    /// Returns the layer number as a `usize`.
    #[inline]
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }
    /// Returns the layer with the given total number of cells exactly, or
    /// `None` if one does not exist.
    #[inline]
    pub fn from_num_cells<D: Dim>(num_cells: usize) -> Option<Self> {
        let ret = Self(num_cells.trailing_zeros() / D::NDIM as u32);
        if num_cells == ret.num_cells::<D>().unwrap() {
            Some(ret)
        } else {
            None
        }
    }

    /// Returns the highest layer at which leaf nodes are allowed.
    pub fn base<D: Dim>() -> Self {
        // Let's set a limit of 64 bytes to represent the cells in each leaf
        // node, so that jemalloc can fit each one in a single cache line. Since
        // each cell takes up a single byte, this results in the following
        // maximum leaf node size for each dimensionality:
        match_ndim!(match D {
            1 => Layer(6), // 64          = 64 bytes
            2 => Layer(3), // 8x8         = 64 bytes
            3 => Layer(2), // 4x4x4       = 64 bytes
            4 => Layer(1), // 2x2x2x2     = 16 bytes
            5 => Layer(1), // 2x2x2x2x2   = 32 bytes
            6 => Layer(1), // 2x2x2x2x2x2 = 64 bytes
        })
        // Some informal performance testing on CGOL simulation suggests that
        // layer 3 is indeed ideal for 2-state 2D CA.
    }
    /// Returns `true` if a node at this layer would be a leaf node.
    pub fn is_leaf<D: Dim>(self) -> bool {
        self <= Self::base::<D>()
    }
    /// Returns `true` if a node at this layer would be a non-leaf node.
    pub fn is_non_leaf<D: Dim>(self) -> bool {
        !self.is_leaf::<D>()
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
                .expect("ND-tree node layer exceeds maximum"),
        )
    }

    /// Returns the number of cells along each axis of a node at this layer, or
    /// `None` if it does not fit in a `usize`.
    #[inline]
    pub fn len(self) -> Option<usize> {
        try_pow_2(self.to_usize())
    }
    /// Returns the number of cells along each axis of a node at this layer as a
    /// `BigInt`.
    #[inline]
    pub fn big_len(self) -> BigInt {
        BigInt::one() << self.to_usize()
    }
    /// Returns the number of cells along each axis of a node at the base layer.
    #[inline]
    pub fn base_len<D: Dim>() -> usize {
        Self::base::<D>().len().unwrap()
    }

    /// Returns the total number of cells of a node at this layer, or `None` if
    /// it does not fit in a `usize`.
    #[inline]
    pub fn num_cells<D: Dim>(self) -> Option<usize> {
        try_pow_2(self.to_usize() * D::NDIM)
    }
    /// Returns the total number of cells of a node at this layer as a `BigInt`.
    #[inline]
    pub fn big_num_cells<D: Dim>(self) -> BigUint {
        BigUint::one() << (self.to_usize() * D::NDIM)
    }
    /// Returns the total number of cells of a node at the base layer.
    #[inline]
    pub fn base_num_cells<D: Dim>() -> usize {
        Self::base::<D>().num_cells::<D>().unwrap()
    }

    /// Returns a rectangle the size of a node at this layer with the lower
    /// corner at the origin, or `None` if it does not fit in a `URect`.
    #[inline]
    pub fn rect<D: Dim>(self) -> Option<URect<D>> {
        self.len()
            .map(|len| URect::span(UVec::zero(), UVec::repeat(len - 1)))
    }
    /// Returns a rectangle the size of a node at this layer with the lower
    /// corner at the origin as a `BigRect`.
    #[inline]
    pub fn big_rect<D: Dim>(self) -> BigRect<D> {
        BigRect::span(BigVec::zero(), BigVec::repeat(self.big_len() - 1))
    }

    /// Returns the cell offset of the child of a node at this layer with the
    /// given index, or `None` if it does not fit in a `UVec`.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is `Layer(0)`, which does not have
    /// children.
    #[inline]
    pub fn child_offset<D: Dim>(self, index: usize) -> Option<UVec<D>> {
        if 1 << self.child_layer().to_usize() == 0 {
            None
        } else {
            Some(Layer(1).leaf_pos(index) << self.child_layer().to_usize())
        }
    }
    /// Returns the cell offset of the child of a node at this layer with the
    /// given index.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is `Layer(0)`, which does not have
    /// children.
    #[inline]
    pub fn big_child_offset<D: Dim>(self, index: usize) -> BigVec<D> {
        Layer(1).leaf_pos(index).to_bigvec() << self.child_layer().0
    }

    /// Returns the index of the child of a node at this layer that contains the
    /// given position, modulo the node length along each axis.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is `Layer(0)`, which does not have
    /// children.
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
    /// Same as `non_leaf_child_index()`, but takes a `UVec` instead of a
    /// `BigVec`.
    ///
    /// # Panics
    ///
    /// This method may panic if the layer is `Layer(0)`, which does not have
    /// children, or if the length of a node at this layer does not fit in a
    /// `usize`.
    #[inline]
    pub fn small_non_leaf_child_index<D: Dim>(self, pos: UVec<D>) -> usize {
        // See `non_leaf_child_index()` for explanation.
        Layer(1).leaf_cell_index((pos >> self.child_layer().to_u32()) & 1)
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
    /// This method may panic if `index` is out of range and may panic if the
    /// layer is too high to be a leaf node layer.
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
    /// Rounds the given rectangle outward to the next node boundaries at this
    /// layer.
    #[inline]
    pub fn round_rect<D: Dim>(self, rect: &BigRect<D>) -> BigRect<D> {
        let len = self.big_len();
        rect.div_outward(&len) * &len
    }
    /// Rounds the given rectangle outward to the next node boundaries at this
    /// layer, given a global ND-tree base position.
    #[inline]
    pub fn round_rect_with_base_pos<D: Dim>(
        self,
        rect: BigRect<D>,
        base_pos: &BigVec<D>,
    ) -> BigRect<D> {
        self.round_rect(&(rect - base_pos)) + base_pos
    }

    /// Returns the layer of the largest layer boundary a vector points to, or
    /// `None` if it is the origin vector (largest aligned layer would be
    /// infinity).
    #[inline]
    pub fn largest_aligned<D: Dim>(pos: &BigVec<D>) -> Option<Layer> {
        D::axes()
            .iter()
            // `trailing_zeros()` gives the largest power-of-2 multiple, or
            // `None` if the number is zero (infinite trailing zeros).
            .filter_map(|&ax| pos[ax].trailing_zeros())
            // Convert to `u32`, or `None` if it is too large (shouldn't
            // actually happen).
            .filter_map(|n| n.to_u32())
            .map(Layer)
            // Now the iterator gives the largest aligned layer along each axis;
            // return the smallest of these, because the layer must be aligned
            // on all axes.
            .min()
    }
}

/// Error returned when an operation fails because a node or layer is too small.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct LayerTooSmall;

/// Returns `1 << pow` if the result fits in a `usize`, or `None` if it does
/// not.
fn try_pow_2(pow: usize) -> Option<usize> {
    if pow < 8 * std::mem::size_of::<usize>() {
        Some(1 << pow)
    } else {
        None
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
        for pos in IRect3D::centered(IVec3D::zero(), 20).iter() {
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
        for pos in URect3D::span(UVec3D::zero(), UVec3D::repeat(30)).iter() {
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

    /// Tests `Layer::largest_aligned()`.
    #[test]
    fn test_ndtree_node_largest_aligned_layer() {
        use crate::dim::Dim2D;

        let gcl = |x, y| Layer::largest_aligned::<Dim2D>(&NdVec::big([x, y]));

        assert_eq!(gcl(0, 0), None);
        assert_eq!(gcl(1, 0), Some(Layer(0)));
        assert_eq!(gcl(2, 0), Some(Layer(1))); // 2
        assert_eq!(gcl(3, 0), Some(Layer(0)));
        assert_eq!(gcl(4, 4), Some(Layer(2))); // 4
        assert_eq!(gcl(5, 5), Some(Layer(0)));
        assert_eq!(gcl(0, 6), Some(Layer(1))); // 2
        assert_eq!(gcl(0, 7), Some(Layer(0)));
        assert_eq!(gcl(0, 8), Some(Layer(3))); // 8

        assert_eq!(gcl(2, 8), Some(Layer(1))); // 2
        assert_eq!(gcl(8, 2), Some(Layer(1))); // 2
    }

    #[test]
    fn test_try_pow_2() {
        let bits = 8 * std::mem::size_of::<usize>();
        for pow in 0..bits {
            assert_eq!(Some(1 << pow), try_pow_2(pow));
        }
        assert_eq!(None, try_pow_2(bits));
        assert_eq!(None, try_pow_2(bits + 1));
        assert_eq!(None, try_pow_2(bits + 2));
    }
}
