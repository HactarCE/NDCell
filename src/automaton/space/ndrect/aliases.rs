use super::*;
use noisy_float::prelude::R64;
use num::BigInt;

/// A 1D hyperrectangle (a linear range).
pub type Rect1D<N> = NdRect<Dim1D, N>;
/// A 2D hyperrectangle (a rectangle).
pub type Rect2D<N> = NdRect<Dim2D, N>;
/// A 3D hyperrectangle (a rectangular prism).
pub type Rect3D<N> = NdRect<Dim3D, N>;
/// A 4D hyperrectangle (a rectangular duoprism).
pub type Rect4D<N> = NdRect<Dim4D, N>;
/// A 5D hyperrectangle.
pub type Rect5D<N> = NdRect<Dim5D, N>;
/// A 6D hyperrectangle.
pub type Rect6D<N> = NdRect<Dim6D, N>;

/// An N-dimensional hyperrectangle with BigInt coordinates.
pub type BigRect<D> = NdRect<D, BigInt>;
/// An N-dimensional hyperrectangle with R64 coordinates.
pub type FRect<D> = NdRect<D, R64>;
/// An N-dimensional hyperrectangle with isize coordinates.
pub type IRect<D> = NdRect<D, isize>;
/// An N-dimensional hyperrectangle with usize coordinates.
pub type URect<D> = NdRect<D, usize>;
/// An N-dimensional hyperrectangle with u8 coordinates.
pub type ByteRect<D> = NdRect<D, u8>;

/// A 1D hyperrectangle (a linear range) with BigInt coordinates.
pub type BigRect1D = BigRect<Dim1D>;
/// A 2D hyperrectangle (a rectangle) with BigInt coordinates.
pub type BigRect2D = BigRect<Dim2D>;
/// A 3D hyperrectangle (a rectangular prism) with BigInt coordinates.
pub type BigRect3D = BigRect<Dim3D>;
/// A 4D hyperrectangle (a rectangular duoprism) with BigInt coordinates.
pub type BigRect4D = BigRect<Dim4D>;
/// A 5D hyperrectangle with BigInt coordinates.
pub type BigRect5D = BigRect<Dim5D>;
/// A 6D hyperrectangle with BigInt coordinates.
pub type BigRect6D = BigRect<Dim6D>;

/// A 1D hyperrectangle (a linear range) with R64 coordinates.
pub type FRect1D = FRect<Dim1D>;
/// A 2D hyperrectangle (a rectangle) with R64 coordinates.
pub type FRect2D = FRect<Dim2D>;
/// A 3D hyperrectangle (a rectangular prism) with R64 coordinates.
pub type FRect3D = FRect<Dim3D>;
/// A 4D hyperrectangle (a rectangular duoprism) with R64 coordinates.
pub type FRect4D = FRect<Dim4D>;
/// A 5D hyperrectangle with R64 coordinates.
pub type FRect5D = FRect<Dim5D>;
/// A 6D hyperrectangle with R64 coordinates.
pub type FRect6D = FRect<Dim6D>;

/// A 1D hyperrectangle (a linear range) with isize coordinates.
pub type IRect1D = IRect<Dim1D>;
/// A 2D hyperrectangle (a rectangle) with isize coordinates.
pub type IRect2D = IRect<Dim2D>;
/// A 3D hyperrectangle (a rectangular prism) with isize coordinates.
pub type IRect3D = IRect<Dim3D>;
/// A 4D hyperrectangle (a rectangular duoprism) with isize coordinates.
pub type IRect4D = IRect<Dim4D>;
/// A 5D hyperrectangle with isize coordinates.
pub type IRect5D = IRect<Dim5D>;
/// A 6D hyperrectangle with isize coordinates.
pub type IRect6D = IRect<Dim6D>;

/// A 1D hyperrectangle (a linear range) with usize coordinates.
pub type URect1D = URect<Dim1D>;
/// A 2D hyperrectangle (a rectangle) with usize coordinates.
pub type URect2D = URect<Dim2D>;
/// A 3D hyperrectangle (a rectangular prism) with usize coordinates.
pub type URect3D = URect<Dim3D>;
/// A 4D hyperrectangle (a rectangular duoprism) with usize coordinates.
pub type URect4D = URect<Dim4D>;
/// A 5D hyperrectangle with usize coordinates.
pub type URect5D = URect<Dim5D>;
/// A 6D hyperrectangle with usize coordinates.
pub type URect6D = URect<Dim6D>;

/// A 1D hyperrectangle (a linear range) with u8 coordinates.
pub type ByteRect1D = ByteRect<Dim1D>;
/// A 2D hyperrectangle (a rectangle) with u8 coordinates.
pub type ByteRect2D = ByteRect<Dim2D>;
/// A 3D hyperrectangle (a rectangular prism) with u8 coordinates.
pub type ByteRect3D = ByteRect<Dim3D>;
/// A 4D hyperrectangle (a rectangular duoprism) with u8 coordinates.
pub type ByteRect4D = ByteRect<Dim4D>;
/// A 5D hyperrectangle with u8 coordinates.
pub type ByteRect5D = ByteRect<Dim5D>;
/// A 6D hyperrectangle with u8 coordinates.
pub type ByteRect6D = ByteRect<Dim6D>;
