use num::BigInt;
use std::borrow::BorrowMut;

use super::*;

/// A vector of a given dimensionality; mostly used as a type argument to convey
/// the number of dimensions something has.
///
/// This is basically exactly the same as ndarray's Dimension trait, except it
/// uses any number type instead of usize (which is crucial for this
/// application). Similar to ndarray's Dimension trait, this trait should not
/// and cannot be implemented outside of this crate.
pub trait Dim: Debug + Default + Copy + Eq + Hash + private::Sealed {
    /// The number of dimensions (number of axes).
    const NDIM: usize;

    type BigIntArray: Debug + Default + Clone + Eq + Hash + AsRef<[BigInt]> + AsMut<[BigInt]>;
    type F32Array: Debug + Default + Copy + PartialEq + AsRef<[f32]> + AsMut<[f32]>;
    type IsizeArray: Debug + Default + Copy + Eq + Hash + AsRef<[isize]> + AsMut<[isize]>;
    type U8Array: Debug + Default + Copy + Eq + Hash + AsRef<[u8]> + AsMut<[u8]>;
    type UsizeArray: Debug + Default + Copy + Eq + Hash + AsRef<[usize]> + AsMut<[usize]>;

    /// Returns a Vector of the axes of this many dimensions.
    fn axes() -> &'static [Axis] {
        ndim_axes(Self::NDIM)
    }

    // /// Returns the coordinate along the given axis.
    // fn get(&self, axis: Axis) -> &N;

    // /// Returns a mutable reference to the coordinate along the given axis.
    // fn get_mut(&mut self, axis: Axis) -> &mut N;

    // /// Sets the coordinate along the given axis.
    // fn set(&mut self, axis: Axis, value: N);

    // /// Returns whether these coordinates consists entirely of zeros.
    // fn is_zero(&self) -> bool {
    //     for &ax in Self::axes() {
    //         if self.get(ax) != N::zero() {
    //             return false;
    //         }
    //     }
    //     true
    // }

    // /// Returns the coordinates of the origin (i.e. all zeros).
    // fn origin() -> Self;

    // /// Returns true if the given axis belongs to this dimensionality.
    // fn contains(axis: Axis) -> bool {
    //     (axis as usize) < Self::NDIM
    // }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim1D;
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim2D;
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim3D;
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim4D;
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim5D;
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim6D;

// /// A basic 1D vector type.
// pub type Dim1D<N> = [N; 1];
// /// A basic 2D vector type.
// pub type Dim2D<N> = [N; 2];
// /// A basic 3D vector type.
// pub type Dim3D<N> = [N; 3];
// /// A basic 4D vector type.
// pub type Dim4D<N> = [N; 4];
// /// A basic 5D vector type.
// pub type Dim5D<N> = [N; 5];
// /// A basic 6D vector type.
// pub type Dim6D<N> = [N; 6];

impl Dim for Dim1D {
    const NDIM: usize = 1;
    type BigIntArray = [BigInt; 1];
    type F32Array = [f32; 1];
    type IsizeArray = [isize; 1];
    type U8Array = [u8; 1];
    type UsizeArray = [usize; 1];
}
impl Dim for Dim2D {
    const NDIM: usize = 2;
    type BigIntArray = [BigInt; 2];
    type F32Array = [f32; 2];
    type IsizeArray = [isize; 2];
    type U8Array = [u8; 2];
    type UsizeArray = [usize; 2];
}
impl Dim for Dim3D {
    const NDIM: usize = 3;
    type BigIntArray = [BigInt; 3];
    type F32Array = [f32; 3];
    type IsizeArray = [isize; 3];
    type U8Array = [u8; 3];
    type UsizeArray = [usize; 3];
}
impl Dim for Dim4D {
    const NDIM: usize = 4;
    type BigIntArray = [BigInt; 4];
    type F32Array = [f32; 4];
    type IsizeArray = [isize; 4];
    type U8Array = [u8; 4];
    type UsizeArray = [usize; 4];
}
impl Dim for Dim5D {
    const NDIM: usize = 5;
    type BigIntArray = [BigInt; 5];
    type F32Array = [f32; 5];
    type IsizeArray = [isize; 5];
    type U8Array = [u8; 5];
    type UsizeArray = [usize; 5];
}
impl Dim for Dim6D {
    const NDIM: usize = 6;
    type BigIntArray = [BigInt; 6];
    type F32Array = [f32; 6];
    type IsizeArray = [isize; 6];
    type U8Array = [u8; 6];
    type UsizeArray = [usize; 6];
}

// Make Dim a "sealed trait" https://rust-lang.github.io/api-guidelines/future-proofing.html#c-sealed
mod private {
    use super::*;

    pub trait Sealed {}
    impl Sealed for Dim1D {}
    impl Sealed for Dim2D {}
    impl Sealed for Dim3D {}
    impl Sealed for Dim4D {}
    impl Sealed for Dim5D {}
    impl Sealed for Dim6D {}
}
