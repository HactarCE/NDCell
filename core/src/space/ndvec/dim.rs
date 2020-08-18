//! Dimensionality trait, which provides a number of dimensions.
//!
//! Until generic associated types are stable (see rust-lang#44265), we have to
//! use a hacky `DimFor` trait to get generic vectors with different numeric
//! types to work.

use super::*;

/// Dimensionality of a vector.
///
/// This trait is only implemented for the six structs `Dim1D`, `Dim2D`,
/// `Dim3D`, `Dim4D`, `Dim5D`, and `Dim6D`. (NDCell does not and will not
/// support more than six dimensions.)
pub trait Dim:
    'static + DimFor<BigInt> + DimFor<FixedPoint> + DimFor<R64> + DimFor<isize> + DimFor<usize> + Send
{
    /// Number of dimensions.
    const NDIM: usize;

    /// Branching factor of ND-trees with this dimensionality.
    const BRANCHING_FACTOR: usize = 1 << Self::NDIM;

    /// Returns an array of the axes of this many dimensions.
    fn axes() -> &'static [Axis] {
        ndim_axes(Self::NDIM)
    }

    /// Returns true if this dimensionality includes the given axis.
    fn contains(axis: Axis) -> bool {
        (axis as usize) < Self::NDIM
    }
}

/// 1 dimension.
#[derive(fmt::Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim1D;
/// 2 dimensions.
#[derive(fmt::Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim2D;
/// 3 dimensions.
#[derive(fmt::Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim3D;
/// 4 dimensions.
#[derive(fmt::Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim4D;
/// 5 dimensions.
#[derive(fmt::Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Dim5D;
/// 6 dimensions.
#[derive(fmt::Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

/// Trait providing an array type to create a generic N-length array.
///
/// Once generic associated types come along, this can be merged into `Dim`.
pub trait DimFor<T: Default + Clone + Eq>:
    fmt::Debug + Default + Copy + Eq + Hash + Sync + private::Sealed
{
    /// Pure `Dim` type associated with this DimFor (i.e. `Self`).
    type Dim: Dim;
    /// Array type used for vectors.
    type Array: fmt::Debug + Default + Clone + Eq + Hash + Send + AsRef<[T]> + AsMut<[T]>;
}

impl<T: NdVecNum> DimFor<T> for Dim1D {
    type Dim = Self;
    type Array = [T; 1];
}
impl<T: NdVecNum> DimFor<T> for Dim2D {
    type Dim = Self;
    type Array = [T; 2];
}
impl<T: NdVecNum> DimFor<T> for Dim3D {
    type Dim = Self;
    type Array = [T; 3];
}
impl<T: NdVecNum> DimFor<T> for Dim4D {
    type Dim = Self;
    type Array = [T; 4];
}
impl<T: NdVecNum> DimFor<T> for Dim5D {
    type Dim = Self;
    type Array = [T; 5];
}
impl<T: NdVecNum> DimFor<T> for Dim6D {
    type Dim = Self;
    type Array = [T; 6];
}

// Make `Dim` a "sealed trait."
// https://rust-lang.github.io/api-guidelines/future-proofing.html#c-sealed
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
