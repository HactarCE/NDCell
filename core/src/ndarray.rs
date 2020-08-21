//! N-dimensional arrays.

use itertools::Itertools;
use std::ops::{Index, IndexMut};

use super::*;

use crate::num::*;

/// `D`-dimensional array of values of type `T`.
///
/// The minimum coordinate of the array is always 0 along all axes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NdArray<T, D: Dim> {
    size: UVec<D>,
    data: Box<[T]>,
}

impl<'a, D: Dim> From<NodeRef<'a, D>> for NdArray<u8, D> {
    /// Creates an `NdArray` from the cells in an `ndtree::NodeRef`.
    fn from(node: NodeRef<'a, D>) -> Self {
        let count = node
            .big_num_cells()
            .to_usize()
            .expect("Cannot make NdArray using such a large node");
        let size = UVec::repeat(node.big_len().to_usize().unwrap());

        let mut data = Vec::with_capacity(count);
        for idx in 0..count {
            // TODO: Do this in a way that doesn't recompute indices and revisit
            // nodes and then benchmark to see if that's faster, since this
            // function is called during simulation.
            data.push(node.cell_at_pos(&unflatten_idx(size.clone(), idx).to_bigvec()));
        }
        assert_eq!(count, data.len());
        let data = data.into_boxed_slice();

        Self { size, data }
    }
}

impl<T, D: Dim> Index<UVec<D>> for NdArray<T, D> {
    type Output = T;

    #[inline]
    fn index(&self, pos: UVec<D>) -> &T {
        &self.data[self.flatten_idx(pos)]
    }
}
impl<T, D: Dim> IndexMut<UVec<D>> for NdArray<T, D> {
    #[inline]
    fn index_mut(&mut self, pos: UVec<D>) -> &mut T {
        let idx = self.flatten_idx(pos);
        &mut self.data[idx]
    }
}

impl<T, D: Dim> NdArray<T, D> {
    /// Creates an `NdArray` from a flat vector.
    ///
    /// # Panics
    ///
    /// This function panics if the length of `data` does not match `size`.
    #[inline]
    pub fn from_flat_slice(size: UVec<D>, data: impl Into<Box<[T]>>) -> Self {
        let data = data.into();
        assert_eq!(size.product(), data.len(), "Wrong size for NdArray");
        Self { size, data }
    }

    /// Returns the flat data behind the array.
    #[inline]
    pub fn into_flat_slice(self) -> Box<[T]> {
        self.data
    }

    /// Returns the size of the array along each axis.
    #[inline]
    pub fn size(&self) -> &UVec<D> {
        &self.size
    }

    /// Returns the rectangular bounds of the array.
    #[inline]
    pub fn rect(&self) -> URect<D> {
        URect::with_size(UVec::origin(), self.size.clone())
    }

    /// Returns an iterator over all the elements in the array, enumerated by
    /// their positions.
    #[inline]
    pub fn iter_enumerated<'a>(&'a self) -> impl 'a + Iterator<Item = (UVec<D>, &T)> {
        self.rect().iter().zip(&*self.data)
    }

    /// Creates a new array of the same size and shape by applying a function to
    /// every element in the array.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn map<U>(&self, f: impl FnMut(&T) -> U) -> NdArray<U, D> {
        NdArray::from_flat_slice(self.size.clone(), self.data.iter().map(f).collect_vec())
    }

    /// Returns the index into `data` corresponding to a `UVec` position.
    fn flatten_idx(&self, pos: UVec<D>) -> usize {
        flatten_idx(self.size.clone(), pos)
    }
}

/// Converts a "flattened" `usize` array index into a `UVec` position for an
/// `NdArray` with the given size.
fn unflatten_idx<D: Dim>(size: UVec<D>, mut idx: usize) -> UVec<D> {
    let mut ret = UVec::origin();
    assert!(idx < size.product() as usize);
    for &ax in D::axes() {
        ret[ax] = idx.rem_euclid(size[ax]);
        idx = idx.div_euclid(size[ax]);
    }
    ret
}

/// Converts a `UVec` position into a "flattened" `usize` array index for an
/// `NdArray` with the given size.
fn flatten_idx<D: Dim>(size: UVec<D>, pos: UVec<D>) -> usize {
    let mut ret = 0;
    let mut stride = 1;
    assert!(pos < size);
    for &ax in D::axes() {
        assert!(pos[ax] < size[ax]);
        ret += pos[ax] * stride;
        stride *= size[ax];
    }
    ret
}

/// 1D array of values of type `T`.
pub type Array1D<T> = NdArray<T, Dim1D>;
/// 2D array of values of type `T`.
pub type Array2D<T> = NdArray<T, Dim2D>;
/// 3D array of values of type `T`.
pub type Array3D<T> = NdArray<T, Dim3D>;
/// 4D array of values of type `T`.
pub type Array4D<T> = NdArray<T, Dim4D>;
/// 5D array of values of type `T`.
pub type Array5D<T> = NdArray<T, Dim5D>;
/// 6D array of values of type `T`.
pub type Array6D<T> = NdArray<T, Dim6D>;

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests `flatten_idx()` and `unflatten_idx()`.
    #[test]
    fn test_ndarray_flatten_unflatten_idx() {
        let size: UVec4D = NdVec([4, 5, 6, 7]);
        let rect: URect4D = NdRect::with_size(NdVec::origin(), size);
        assert_eq!(rect.size(), size);
        let count = rect.count() as usize;
        let mut last_index = None;
        for pos in rect.iter() {
            let flat_idx: usize = flatten_idx(size, pos);
            assert!(flat_idx < count);
            assert_eq!(pos, unflatten_idx(size, flat_idx));
            if let Some(last) = last_index {
                assert_eq!(flat_idx, last + 1);
            }
            last_index = Some(flat_idx);
        }
    }

    /// Tests `NdArray::from::<NodeRef>()`.
    #[test]
    fn test_ndarray_from_noderef() {
        // Create a 2D 4x4 node that looks like this:
        //
        // . . . .
        // . # # .
        // # # . .
        // . . # .
        // (+x to the right, +y up)
        let flats_cells = [
            0, 0, 1, 0, // Y = 0
            1, 1, 0, 0, // Y = 1
            0, 1, 1, 0, // Y = 2
            0, 0, 0, 0, // Y = 3
        ];
        let cache = NodeCache::<Dim2D>::default();
        let node_access = cache.node_access();
        let node = node_access.get_from_cells(flats_cells);
        let array = NdArray::from(node);
        assert_eq!(*array.data, flats_cells);
        for pos in node.big_rect().iter() {
            assert_eq!(node.cell_at_pos(&pos), array[pos.to_uvec()]);
        }
    }
}
