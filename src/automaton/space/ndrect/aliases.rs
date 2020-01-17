use super::*;
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
/// An N-dimensional hyperrectangle with f32 coordinates.
pub type FRect<D> = NdRect<D, f32>;
/// An N-dimensional hyperrectangle with isize coordinates.
pub type IRect<D> = NdRect<D, isize>;
/// An N-dimensional hyperrectangle with usize coordinates.
pub type URect<D> = NdRect<D, usize>;
/// An N-dimensional hyperrectangle with u8 coordinates.
pub type ByteRect<D> = NdRect<D, u8>;

/// A 1D hyperrectangle (a linear range) with BigInt coordinates.
pub type BigRect1D = NdRect<Dim1D, BigInt>;
/// A 2D hyperrectangle (a rectangle) with BigInt coordinates.
pub type BigRect2D = NdRect<Dim2D, BigInt>;
/// A 3D hyperrectangle (a rectangular prism) with BigInt coordinates.
pub type BigRect3D = NdRect<Dim3D, BigInt>;
/// A 4D hyperrectangle (a rectangular duoprism) with BigInt coordinates.
pub type BigRect4D = NdRect<Dim4D, BigInt>;
/// A 5D hyperrectangle with BigInt coordinates.
pub type BigRect5D = NdRect<Dim5D, BigInt>;
/// A 6D hyperrectangle with BigInt coordinates.
pub type BigRect6D = NdRect<Dim6D, BigInt>;

/// A 1D hyperrectangle (a linear range) with f32 coordinates.
pub type FRect1D = NdRect<Dim1D, f32>;
/// A 2D hyperrectangle (a rectangle) with f32 coordinates.
pub type FRect2D = NdRect<Dim2D, f32>;
/// A 3D hyperrectangle (a rectangular prism) with f32 coordinates.
pub type FRect3D = NdRect<Dim3D, f32>;
/// A 4D hyperrectangle (a rectangular duoprism) with f32 coordinates.
pub type FRect4D = NdRect<Dim4D, f32>;
/// A 5D hyperrectangle with f32 coordinates.
pub type FRect5D = NdRect<Dim5D, f32>;
/// A 6D hyperrectangle with f32 coordinates.
pub type FRect6D = NdRect<Dim6D, f32>;

/// A 1D hyperrectangle (a linear range) with isize coordinates.
pub type IRect1D = NdRect<Dim1D, isize>;
/// A 2D hyperrectangle (a rectangle) with isize coordinates.
pub type IRect2D = NdRect<Dim2D, isize>;
/// A 3D hyperrectangle (a rectangular prism) with isize coordinates.
pub type IRect3D = NdRect<Dim3D, isize>;
/// A 4D hyperrectangle (a rectangular duoprism) with isize coordinates.
pub type IRect4D = NdRect<Dim4D, isize>;
/// A 5D hyperrectangle with isize coordinates.
pub type IRect5D = NdRect<Dim5D, isize>;
/// A 6D hyperrectangle with isize coordinates.
pub type IRect6D = NdRect<Dim6D, isize>;

/// A 1D hyperrectangle (a linear range) with usize coordinates.
pub type URect1D = NdRect<Dim1D, usize>;
/// A 2D hyperrectangle (a rectangle) with usize coordinates.
pub type URect2D = NdRect<Dim2D, usize>;
/// A 3D hyperrectangle (a rectangular prism) with usize coordinates.
pub type URect3D = NdRect<Dim3D, usize>;
/// A 4D hyperrectangle (a rectangular duoprism) with usize coordinates.
pub type URect4D = NdRect<Dim4D, usize>;
/// A 5D hyperrectangle with usize coordinates.
pub type URect5D = NdRect<Dim5D, usize>;
/// A 6D hyperrectangle with usize coordinates.
pub type URect6D = NdRect<Dim6D, usize>;

/// A 1D hyperrectangle (a linear range) with u8 coordinates.
pub type ByteRect1D = NdRect<Dim1D, u8>;
/// A 2D hyperrectangle (a rectangle) with u8 coordinates.
pub type ByteRect2D = NdRect<Dim2D, u8>;
/// A 3D hyperrectangle (a rectangular prism) with u8 coordinates.
pub type ByteRect3D = NdRect<Dim3D, u8>;
/// A 4D hyperrectangle (a rectangular duoprism) with u8 coordinates.
pub type ByteRect4D = NdRect<Dim4D, u8>;
/// A 5D hyperrectangle with u8 coordinates.
pub type ByteRect5D = NdRect<Dim5D, u8>;
/// A 6D hyperrectangle with u8 coordinates.
pub type ByteRect6D = NdRect<Dim6D, u8>;
