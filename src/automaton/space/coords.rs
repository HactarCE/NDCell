use ndarray::prelude::*;
use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;

use super::{ndim_axes, Axis};

/// Any 1D Coords vector.
pub type Coords1D = [isize; 1];
/// Any 2D Coords vector.
pub type Coords2D = [isize; 2];
/// Any 3D Coords vector.
pub type Coords3D = [isize; 3];
/// Any 4D Coords vector.
pub type Coords4D = [isize; 4];
/// Any 5D Coords vector.
pub type Coords5D = [isize; 5];
/// Any 6D Coords vector.
pub type Coords6D = [isize; 6];

/// Computes the "recommended" number of bits in each axis of a chunk index for
/// a given dimension count. The chunk size (along each axis) is 1 << (chunk
/// bits).
///
/// This is based on trying to keep the chunk size big, but still reasonable
/// (such that a full chunk is at most 4k) and always a power of 2.
///
/// Using a flat chunk size would either result in stupidly small chunks at
/// lower dimensions (16 is silly for 1D CA that often densely span thousands of
/// cells) or stupidly huge chunks at higher dimensions (even a 32^4 chunk in 4D
/// would be 1 MiB, which is rather large to be copying around constantly).
///
/// Here are the values that this function outputs:
///
/// - 1D => 12 -> 4096 = 4k
/// - 2D => 12 -> 64^2 = 4k
/// - 3D => 12 -> 16^3 = 4k
/// - 4D => 12 ->  8^4 = 4k
/// - 5D => 12 ->  4^5 = 1k (8^5 would be 32k)
/// - 6D => 12 ->  4^6 = 4k
const fn get_chunk_bits_for_ndim(ndim: usize) -> usize {
    let max_bits = 12; // 2^12 = 4096
    max_bits / ndim
}

/// A set of coordinates for a given dimensionality which allows negative
/// values, unlike NdIndex.
pub trait Coords: Debug + Clone + Eq + Hash + Copy {
    /// The number of dimensions as an ndarray type.
    type D: Dimension;

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

impl Coords for Coords1D {
    type D = Ix1;
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

impl Coords for Coords2D {
    type D = Ix2;
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
impl Coords for Coords3D {
    type D = Ix3;
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
impl Coords for Coords4D {
    type D = Ix4;
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
impl Coords for Coords5D {
    type D = Ix5;
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
impl Coords for Coords6D {
    type D = Ix6;
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
