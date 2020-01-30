//! Type aliases for NdVecs.

use super::*;

/// A 1D vector.
pub type Vec1D<N> = NdVec<Dim1D, N>;
/// A 2D vector.
pub type Vec2D<N> = NdVec<Dim2D, N>;
/// A 3D vector.
pub type Vec3D<N> = NdVec<Dim3D, N>;
/// A 4D vector.
pub type Vec4D<N> = NdVec<Dim4D, N>;
/// A 5D vector.
pub type Vec5D<N> = NdVec<Dim5D, N>;
/// A 6D vector.
pub type Vec6D<N> = NdVec<Dim6D, N>;

/// An N-dimensional vector with BigInt coordinates.
pub type BigVec<D> = NdVec<D, BigInt>;
/// An N-dimensional vector with R64 coordinates.
pub type FVec<D> = NdVec<D, R64>;
/// An N-dimensional vector with isize coordinates.
pub type IVec<D> = NdVec<D, isize>;
/// An N-dimensional vector with usize coordinates.
pub type UVec<D> = NdVec<D, usize>;
/// An N-dimensional vector with u8 coordinates.
pub type ByteVec<D> = NdVec<D, u8>;

/// A vector of unknown dimensionality with BigInt coordinates.
pub type AnyDimBigVec = AnyDimVec<BigInt>;
/// A vector of unknown dimensionality with R64 coordinates.
pub type AnyDimFVec = AnyDimVec<R64>;
/// A vector of unknown dimensionality with isize coordinates.
pub type AnyDimIVec = AnyDimVec<isize>;
/// A vector of unknown dimensionality with usize coordinates.
pub type AnyDimUVec = AnyDimVec<usize>;
/// A vector of unknown dimensionality with u8 coordinates.
pub type AnyDimByteVec = AnyDimVec<u8>;

/// A 1D vector with BigInt coordinates.
pub type BigVec1D = BigVec<Dim1D>;
/// A 2D vector with BigInt coordinates.
pub type BigVec2D = BigVec<Dim2D>;
/// A 3D vector with BigInt coordinates.
pub type BigVec3D = BigVec<Dim3D>;
/// A 4D vector with BigInt coordinates.
pub type BigVec4D = BigVec<Dim4D>;
/// A 5D vector with BigInt coordinates.
pub type BigVec5D = BigVec<Dim5D>;
/// A 6D vector with BigInt coordinates.
pub type BigVec6D = BigVec<Dim6D>;

/// A 1D vector with R64 coordinates.
pub type FVec1D = FVec<Dim1D>;
/// A 2D vector with R64 coordinates.
pub type FVec2D = FVec<Dim2D>;
/// A 3D vector with R64 coordinates.
pub type FVec3D = FVec<Dim3D>;
/// A 4D vector with R64 coordinates.
pub type FVec4D = FVec<Dim4D>;
/// A 5D vector with R64 coordinates.
pub type FVec5D = FVec<Dim5D>;
/// A 6D vector with R64 coordinates.
pub type FVec6D = FVec<Dim6D>;

/// A 1D vector with isize coordinates.
pub type IVec1D = IVec<Dim1D>;
/// A 2D vector with isize coordinates.
pub type IVec2D = IVec<Dim2D>;
/// A 3D vector with isize coordinates.
pub type IVec3D = IVec<Dim3D>;
/// A 4D vector with isize coordinates.
pub type IVec4D = IVec<Dim4D>;
/// A 5D vector with isize coordinates.
pub type IVec5D = IVec<Dim5D>;
/// A 6D vector with isize coordinates.
pub type IVec6D = IVec<Dim6D>;

/// A 1D vector with u8 coordinates.
pub type ByteVec1D = ByteVec<Dim1D>;
/// A 2D vector with u8 coordinates.
pub type ByteVec2D = ByteVec<Dim2D>;
/// A 3D vector with u8 coordinates.
pub type ByteVec3D = ByteVec<Dim3D>;
/// A 4D vector with u8 coordinates.
pub type ByteVec4D = ByteVec<Dim4D>;
/// A 5D vector with u8 coordinates.
pub type ByteVec5D = ByteVec<Dim5D>;
/// A 6D vector with u8 coordinates.
pub type ByteVec6D = ByteVec<Dim6D>;

/// A 1D vector with usize coordinates.
pub type UVec1D = UVec<Dim1D>;
/// A 2D vector with usize coordinates.
pub type UVec2D = UVec<Dim2D>;
/// A 3D vector with usize coordinates.
pub type UVec3D = UVec<Dim3D>;
/// A 4D vector with usize coordinates.
pub type UVec4D = UVec<Dim4D>;
/// A 5D vector with usize coordinates.
pub type UVec5D = UVec<Dim5D>;
/// A 6D vector with usize coordinates.
pub type UVec6D = UVec<Dim6D>;
