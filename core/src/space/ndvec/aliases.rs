//! Type aliases for `NdVec`s.

use super::*;

/// 1D vector wiith coordinates of type `N`.
pub type Vec1D<N> = NdVec<Dim1D, N>;
/// 2D vector wiith coordinates of type `N`.
pub type Vec2D<N> = NdVec<Dim2D, N>;
/// 3D vector wiith coordinates of type `N`.
pub type Vec3D<N> = NdVec<Dim3D, N>;
/// 4D vector wiith coordinates of type `N`.
pub type Vec4D<N> = NdVec<Dim4D, N>;
/// 5D vector wiith coordinates of type `N`.
pub type Vec5D<N> = NdVec<Dim5D, N>;
/// 6D vector wiith coordinates of type `N`.
pub type Vec6D<N> = NdVec<Dim6D, N>;

/// `D`-dimensional vector with `BigInt` coordinates.
pub type BigVec<D> = NdVec<D, BigInt>;
/// `D`-dimensional vector with `Fixed` coordinates.
pub type FixedVec<D> = NdVec<D, FixedPoint>;
/// `D`-dimensional vector with `R64` coordinates.
pub type FVec<D> = NdVec<D, R64>;
/// `D`-dimensional vector with `isize` coordinates.
pub type IVec<D> = NdVec<D, isize>;
/// `D`-dimensional vector with `usize` coordinates.
pub type UVec<D> = NdVec<D, usize>;

/// Vector with any dimensionality and `BigInt` coordinates.
pub type AnyDimBigVec = AnyDimVec<BigInt>;
/// Vector with any dimensionality and `R64` coordinates.
pub type AnyDimFVec = AnyDimVec<R64>;
/// Vector with any dimensionality and `isize` coordinates.
pub type AnyDimIVec = AnyDimVec<isize>;
/// Vector with any dimensionality and `usize` coordinates.
pub type AnyDimUVec = AnyDimVec<usize>;
/// Vector with any dimensionality and `u8` coordinates.
pub type AnyDimByteVec = AnyDimVec<u8>;

/// 1D vector with `BigInt` coordinates.
pub type BigVec1D = BigVec<Dim1D>;
/// 2D vector with `BigInt` coordinates.
pub type BigVec2D = BigVec<Dim2D>;
/// 3D vector with `BigInt` coordinates.
pub type BigVec3D = BigVec<Dim3D>;
/// 4D vector with `BigInt` coordinates.
pub type BigVec4D = BigVec<Dim4D>;
/// 5D vector with `BigInt` coordinates.
pub type BigVec5D = BigVec<Dim5D>;
/// 6D vector with `BigInt` coordinates.
pub type BigVec6D = BigVec<Dim6D>;

/// 1D vector with `FixedPoint` coordinates.
pub type FixedVec1D = FixedVec<Dim1D>;
/// 2D vector with `FixedPoint` coordinates.
pub type FixedVec2D = FixedVec<Dim2D>;
/// 3D vector with `FixedPoint` coordinates.
pub type FixedVec3D = FixedVec<Dim3D>;
/// 4D vector with `FixedPoint` coordinates.
pub type FixedVec4D = FixedVec<Dim4D>;
/// 5D vector with `FixedPoint` coordinates.
pub type FixedVec5D = FixedVec<Dim5D>;
/// 6D vector with `FixedPoint` coordinates.
pub type FixedVec6D = FixedVec<Dim6D>;

/// 1D vector with `R64` coordinates.
pub type FVec1D = FVec<Dim1D>;
/// 2D vector with `R64` coordinates.
pub type FVec2D = FVec<Dim2D>;
/// 3D vector with `R64` coordinates.
pub type FVec3D = FVec<Dim3D>;
/// 4D vector with `R64` coordinates.
pub type FVec4D = FVec<Dim4D>;
/// 5D vector with `R64` coordinates.
pub type FVec5D = FVec<Dim5D>;
/// 6D vector with `R64` coordinates.
pub type FVec6D = FVec<Dim6D>;

/// 1D vector with `isize` coordinates.
pub type IVec1D = IVec<Dim1D>;
/// 2D vector with `isize` coordinates.
pub type IVec2D = IVec<Dim2D>;
/// 3D vector with `isize` coordinates.
pub type IVec3D = IVec<Dim3D>;
/// 4D vector with `isize` coordinates.
pub type IVec4D = IVec<Dim4D>;
/// 5D vector with `isize` coordinates.
pub type IVec5D = IVec<Dim5D>;
/// 6D vector with `isize` coordinates.
pub type IVec6D = IVec<Dim6D>;

/// 1D vector with `usize` coordinates.
pub type UVec1D = UVec<Dim1D>;
/// 2D vector with `usize` coordinates.
pub type UVec2D = UVec<Dim2D>;
/// 3D vector with `usize` coordinates.
pub type UVec3D = UVec<Dim3D>;
/// 4D vector with `usize` coordinates.
pub type UVec4D = UVec<Dim4D>;
/// 5D vector with `usize` coordinates.
pub type UVec5D = UVec<Dim5D>;
/// 6D vector with `usize` coordinates.
pub type UVec6D = UVec<Dim6D>;
