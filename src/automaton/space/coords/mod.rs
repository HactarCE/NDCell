use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Index, IndexMut};

mod axis;
mod dim;
mod ops_scalar;
mod ops_vector;

pub use axis::*;
pub use dim::*;

// A set of coordinates for a given dimensionality.
//
// Unlike ndarray's NdIndex, this uses isize and so supports negative numbers.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Coords<D: Dim>(D);

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

// Implement generic vector operations.
impl<D: Dim> Coords<D> {
    const NDIM: usize = D::NDIM;

    fn origin() -> Self {
        Self(D::origin())
    }
}

// Implement indexing by usize.
impl<D: Dim> Index<Axis> for Coords<D> {
    type Output = isize;
    fn index(&self, axis: Axis) -> &isize {
        self.0.get(axis)
    }
}
impl<D: Dim> IndexMut<Axis> for Coords<D> {
    fn index_mut(&mut self, axis: Axis) -> &mut isize {
        self.0.get_mut(axis)
    }
}
