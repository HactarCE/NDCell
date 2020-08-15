//! Basic N-dimensional arrays for smaller patterns.

use itertools::Itertools;
use num::ToPrimitive;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use super::*;

/// A basic N-dimensional array, implemented using a flat Vec<T>.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NdArray<T, D: Dim> {
    size: UVec<D>,
    data: Box<[T]>,
}

// Create an NdArray from the cells in an NdTree NodeRef.
impl<'a, D: Dim> From<NodeRef<'a, D>> for NdArray<u8, D> {
    fn from(node: NodeRef<'a, D>) -> Self {
        let count = node
            .big_num_cells()
            .to_usize()
            .expect("Cannot make NdArray using such a large node");
        let size = UVec::repeat(node.big_len().to_usize().unwrap());

        let mut data = Vec::with_capacity(count);
        for idx in 0..count {
            // TODO: do this in a way that doesn't revisit nodes
            data.push(node.cell_at_pos(&unflatten_idx(size.clone(), idx).convert()));
        }
        assert_eq!(count, data.len());
        let data = data.into_boxed_slice();

        Self { size, data }
    }
}

// Get or set an element in an NdArray.
impl<T, D: Dim> Index<UVec<D>> for NdArray<T, D> {
    type Output = T;
    fn index(&self, pos: UVec<D>) -> &T {
        &self.data[self.flatten_idx(pos)]
    }
}
impl<T, D: Dim> IndexMut<UVec<D>> for NdArray<T, D> {
    fn index_mut(&mut self, pos: UVec<D>) -> &mut T {
        let idx = self.flatten_idx(pos);
        &mut self.data[idx]
    }
}

impl<T, D: Dim> NdArray<T, D> {
    /// Creates an NdArray from a flat vector. Panics if the vector is not the
    /// right length.,
    pub fn from_flat_slice(size: UVec<D>, data: impl Into<Box<[T]>>) -> Self {
        let data = data.into();
        assert_eq!(size.product(), data.len(), "Wrong size for NdArray");
        Self { size, data }
    }
    /// Returns the flat data behind this array.
    pub fn into_flat_slice(self) -> Box<[T]> {
        self.data
    }

    /// Returns an NdArrayView of this NdArray with no offset.
    pub fn slice(self: Rc<Self>) -> NdArrayView<T, D> {
        self.into()
    }
    /// Returns an NdArrayView of this NdArray with the given offset.
    pub fn offset_slice(self: Rc<Self>, offset: IVec<D>) -> NdArrayView<T, D> {
        let array = self;
        NdArrayView { array, offset }
    }
    /// Returns the size vector of this NdArray.
    pub fn size(&self) -> &UVec<D> {
        &self.size
    }
    /// Returns the rectangle of this NdArray.
    pub fn rect(&self) -> URect<D> {
        URect::new(UVec::origin(), self.size.clone())
    }
    /// Returns an iterator over all the elements in this array, enumerated by
    /// their positions.
    pub fn iter_enumerated<'a>(&'a self) -> impl 'a + Iterator<Item = (UVec<D>, &T)> {
        self.data
            .iter()
            .enumerate()
            .map(move |(idx, item)| (unflatten_idx(self.size.clone(), idx), item))
    }
    /// Applies a function to every element in the array, returning a new array
    /// of the same size and shape.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn map<U>(&self, f: impl FnMut(&T) -> U) -> NdArray<U, D> {
        NdArray::from_flat_slice(self.size.clone(), self.data.iter().map(f).collect_vec())
    }

    fn flatten_idx(&self, pos: UVec<D>) -> usize {
        flatten_idx(self.size.clone(), pos)
    }
}

/// An offset immutable slice of a NdArray.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NdArrayView<T, D: Dim> {
    array: Rc<NdArray<T, D>>,
    offset: IVec<D>,
}
impl<T, D: Dim> AsRef<NdArray<T, D>> for NdArrayView<T, D> {
    fn as_ref(&self) -> &NdArray<T, D> {
        &self.array
    }
}
impl<T, D: Dim> From<Rc<NdArray<T, D>>> for NdArrayView<T, D> {
    fn from(array: Rc<NdArray<T, D>>) -> Self {
        let offset = IVec::origin();
        Self { array, offset }
    }
}

impl<T, D: Dim> Index<IVec<D>> for NdArrayView<T, D> {
    type Output = T;
    fn index(&self, pos: IVec<D>) -> &T {
        &self.array[(pos - self.offset.clone()).to_uvec()]
    }
}

impl<T, D: Dim> NdArrayView<T, D> {
    /// Returns the rectangle of this NdArrayView.
    pub fn rect(&self) -> IRect<D> {
        IRect::new(
            -self.offset.clone(),
            IVec::from_fn(|ax| self.array.size[ax] as isize),
        )
    }
}

/// Converts a usize array index into an NdVec position.
fn unflatten_idx<D: Dim>(size: UVec<D>, mut idx: usize) -> UVec<D> {
    let mut ret = UVec::origin();
    assert!(idx < size.product() as usize);
    for &ax in D::axes() {
        ret[ax] = idx.rem_euclid(size[ax]);
        idx = idx.div_euclid(size[ax]);
    }
    ret
}

/// Converts an NdVec position into a usize array index.
fn flatten_idx<D: Dim>(size: UVec<D>, pos: UVec<D>) -> usize {
    let mut ret = 0;
    let mut stride = 1;
    for &ax in D::axes() {
        assert!(0 <= pos[ax] && pos[ax] < size[ax]);
        ret += pos[ax] * stride;
        stride *= size[ax];
    }
    ret
}

/// A 1D array.
pub type Array1D<T> = NdArray<T, Dim1D>;
/// A 2D array.
pub type Array2D<T> = NdArray<T, Dim2D>;
/// A 3D array.
pub type Array3D<T> = NdArray<T, Dim3D>;
/// A 4D array.
pub type Array4D<T> = NdArray<T, Dim4D>;
/// A 5D array.
pub type Array5D<T> = NdArray<T, Dim5D>;
/// A 6D array.
pub type Array6D<T> = NdArray<T, Dim6D>;

/// An offset slice of a 1D array.
pub type ArrayView1D<T> = NdArrayView<T, Dim1D>;
/// An offset slice of a 2D array.
pub type ArrayView2D<T> = NdArrayView<T, Dim2D>;
/// An offset slice of a 3D array.
pub type ArrayView3D<T> = NdArrayView<T, Dim3D>;
/// An offset slice of a 4D array.
pub type ArrayView4D<T> = NdArrayView<T, Dim4D>;
/// An offset slice of a 5D array.
pub type ArrayView5D<T> = NdArrayView<T, Dim5D>;
/// An offset slice of a 6D array.
pub type ArrayView6D<T> = NdArrayView<T, Dim6D>;

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests that flatten_idx() and unflatten_idx() are inverses and never
    /// return values out of range.
    #[test]
    fn test_flatten_unflatten_ndarray_idx() {
        let size: UVec4D = NdVec([4, 5, 6, 7]);
        let rect: URect4D = NdRect::new(NdVec::origin(), size - 1);
        let count = rect.count() as usize;
        for pos in rect.iter() {
            let flat_idx: usize = flatten_idx(size, pos);
            assert!(flat_idx < count);
            assert_eq!(pos, unflatten_idx(size, flat_idx));
        }
    }
}
