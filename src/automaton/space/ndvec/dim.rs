use num::BigInt;

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

    /// Returns true if the given axis belongs to this dimensionality.
    fn contains(axis: Axis) -> bool {
        (axis as usize) < Self::NDIM
    }
}

/// A type representing 1D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim1D;
/// A type representing 2D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim2D;
/// A type representing 3D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim3D;
/// A type representing 4D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim4D;
/// A type representing 5D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim5D;
/// A type representing 6D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Dim6D;

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
