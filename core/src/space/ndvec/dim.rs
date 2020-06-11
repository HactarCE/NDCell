use noisy_float::prelude::R64;
use num::BigInt;

use super::*;

/// A vector of a given dimensionality; mostly used as a type argument to convey
/// the number of dimensions something has.
///
/// This is basically exactly the same as ndarray's Dimension trait, except it
/// uses any number type instead of usize (which is crucial for this
/// application). Similar to ndarray's Dimension trait, this trait should not
/// and cannot be implemented outside of this crate.
pub trait Dim:
    DimFor<BigInt> + DimFor<R64> + DimFor<isize> + DimFor<usize> + DimFor<u8> + Send
{
    /// The number of dimensions (number of axes).
    const NDIM: usize;

    /// The number of branches for each node in an NdTree of this
    /// dimensionality.
    const TREE_BRANCHES: usize = 1 << Self::NDIM;

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
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim1D;
/// A type representing 2D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim2D;
/// A type representing 3D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim3D;
/// A type representing 4D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim4D;
/// A type representing 5D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim5D;
/// A type representing 6D things.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim6D;

impl Dim for Dim1D {
    const NDIM: usize = 1;
}
impl Dim for Dim2D {
    const NDIM: usize = 2;
}
impl Dim for Dim3D {
    const NDIM: usize = 3;
}
impl Dim for Dim4D {
    const NDIM: usize = 4;
}
impl Dim for Dim5D {
    const NDIM: usize = 5;
}
impl Dim for Dim6D {
    const NDIM: usize = 6;
}

/// A trait providing an array type to create a generic N-length array.
///
/// Once generic associated types come along, this can be merged into Dim to
/// simplify things.
pub trait DimFor<T: Default + Clone + Eq>:
    Debug + Default + Copy + Eq + Hash + Sync + private::Sealed
{
    /// The pure Dim type associated with this DimFor (i.e. Self)
    type Dim: Dim;
    /// The array type used for vectors.
    type Array: Debug + Default + Clone + Eq + Hash + Send + AsRef<[T]> + AsMut<[T]>;
}

impl<T: Debug + Default + Clone + Eq + Hash + Send> DimFor<T> for Dim1D {
    type Dim = Dim1D;
    type Array = [T; 1];
}
impl<T: Debug + Default + Clone + Eq + Hash + Send> DimFor<T> for Dim2D {
    type Dim = Dim2D;
    type Array = [T; 2];
}
impl<T: Debug + Default + Clone + Eq + Hash + Send> DimFor<T> for Dim3D {
    type Dim = Dim3D;
    type Array = [T; 3];
}
impl<T: Debug + Default + Clone + Eq + Hash + Send> DimFor<T> for Dim4D {
    type Dim = Dim4D;
    type Array = [T; 4];
}
impl<T: Debug + Default + Clone + Eq + Hash + Send> DimFor<T> for Dim5D {
    type Dim = Dim5D;
    type Array = [T; 5];
}
impl<T: Debug + Default + Clone + Eq + Hash + Send> DimFor<T> for Dim6D {
    type Dim = Dim6D;
    type Array = [T; 6];
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
