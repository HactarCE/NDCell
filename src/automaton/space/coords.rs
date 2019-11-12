use ndarray::prelude::*;
use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;

use super::{ndim_axes, Axis};

// A set of coordinates for a given dimensionality.
//
// Unlike ndarray's NdIndex, this uses isize and so supports negative numbers.
pub struct Coords<D>(D);

/// Any 1D Coords vector.
pub type Coords1D = Coords<[isize; 1]>;
/// Any 2D Coords vector.
pub type Coords2D = Coords<[isize; 2]>;
/// Any 3D Coords vector.
pub type Coords3D = Coords<[isize; 3]>;
/// Any 4D Coords vector.
pub type Coords4D = Coords<[isize; 4]>;
/// Any 5D Coords vector.
pub type Coords5D = Coords<[isize; 5]>;
/// Any 6D Coords vector.
pub type Coords6D = Coords<[isize; 6]>;

/// A vector of a given dimensionality; mostly used as a type argument to convey
/// the number of dimensions something has.
///
/// This is basically exactly the same as ndarray's Dimension trait, except it
/// uses isize instead of usize (which is crucial for this application).
pub trait Dim: Debug + Clone + Eq + Hash + Copy {
    /// The number of dimensions as an ndarray type.
    type NdarrayDim: Dimension;

    /// The number of dimensions (number of axes).
    const NDIM: usize;

    /// The number of bits to use to index a chunk of this many dimensions.
    const CHUNK_BITS: usize = get_chunk_bits_for_ndim(Self::NDIM);

    /// The size (length along one axis) of a chunk of this many dimensions.
    const CHUNK_SIZE: usize = 1 << Self::CHUNK_BITS;

    /// The bitmask to apply to an index to get the corresponding chunk index.
    const CHUNK_BITMASK: isize = Self::CHUNK_SIZE as isize - 1;

    /// Returns a Vector of the axes of this many dimensions.
    fn axes() -> Vec<Axis> {
        ndim_axes(Self::NDIM)
    }

    /// Returns the coordinate along the given axis.
    fn get(&self, axis: Axis) -> &isize;

    /// Returns a mutable reference to the coordinate along the given axis.
    fn get_mut(&mut self, axis: Axis) -> &mut isize;

    /// Sets the coordinate along the given axis.
    fn set(&mut self, axis: Axis, value: isize);

    /// Returns whether these coordinates consists entirely of zeros.
    fn is_zero(&self) -> bool {
        for ax in Self::axes() {
            if self.get(ax) != &0 {
                return false;
            }
        }
        true
    }

    /// Returns the coordinates of the origin (i.e. all zeros).
    fn origin() -> Self;
}

impl Dim for Coords1D {
    type NdarrayDim = Ix1;
    const NDIM: usize = 1;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Coords2D {
    type NdarrayDim = Ix2;
    const NDIM: usize = 2;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Coords3D {
    type NdarrayDim = Ix3;
    const NDIM: usize = 3;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Coords4D {
    type NdarrayDim = Ix4;
    const NDIM: usize = 4;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Coords5D {
    type NdarrayDim = Ix5;
    const NDIM: usize = 5;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
impl Dim for Coords6D {
    type NdarrayDim = Ix6;
    const NDIM: usize = 6;
    fn get(&self, axis: Axis) -> &isize {
        &self[axis as usize]
    }
    fn get_mut(&mut self, axis: Axis) -> &mut isize {
        &mut self[axis as usize]
    }
    fn set(&mut self, axis: Axis, value: isize) {
        self[axis as usize] = value;
    }
    fn origin() -> Self {
        [0; Self::NDIM]
    }
}
