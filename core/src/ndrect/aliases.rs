//! Type aliases for `NdRect`s.

use super::NdRect;
use crate::dim::*;
use crate::num::{BigInt, FixedPoint, R64};

/// 1D hyperrectangle (linear range) with coordinates of type `N`.
pub type Rect1D<N> = NdRect<Dim1D, N>;
/// 2D hyperrectangle (rectangle) with coordinates of type `N`.
pub type Rect2D<N> = NdRect<Dim2D, N>;
/// 3D hyperrectangle (rectangular prism) with coordinates of type `N`.
pub type Rect3D<N> = NdRect<Dim3D, N>;
/// 4D hyperrectangle (rectangular duoprism) with coordinates of type `N`.
pub type Rect4D<N> = NdRect<Dim4D, N>;
/// 5D hyperrectangle with coordinates of type `N`.
pub type Rect5D<N> = NdRect<Dim5D, N>;
/// 6D hyperrectangle with coordinates of type `N`.
pub type Rect6D<N> = NdRect<Dim6D, N>;

/// N-dimensional hyperrectangle with `BigInt` coordinates.
pub type BigRect<D> = NdRect<D, BigInt>;
/// N-dimensional hyperrectangle with `FixedPoint` coordinates.
pub type FixedRect<D> = NdRect<D, FixedPoint>;
/// N-dimensional hyperrectangle with `R64` coordinates.
pub type FRect<D> = NdRect<D, R64>;
/// N-dimensional hyperrectangle with `isize` coordinates.
pub type IRect<D> = NdRect<D, isize>;
/// N-dimensional hyperrectangle with `usize` coordinates.
pub type URect<D> = NdRect<D, usize>;

/// 1D hyperrectangle (linear range) with `BigInt` coordinates.
pub type BigRect1D = BigRect<Dim1D>;
/// 2D hyperrectangle (rectangle) with `BigInt` coordinates.
pub type BigRect2D = BigRect<Dim2D>;
/// 3D hyperrectangle (rectangular prism) with `BigInt` coordinates.
pub type BigRect3D = BigRect<Dim3D>;
/// 4D hyperrectangle (rectangular duoprism) with `BigInt` coordinates.
pub type BigRect4D = BigRect<Dim4D>;
/// 5D hyperrectangle with `BigInt` coordinates.
pub type BigRect5D = BigRect<Dim5D>;
/// 6D hyperrectangle with `BigInt` coordinates.
pub type BigRect6D = BigRect<Dim6D>;

/// 1D hyperrectangle (linear range) with `FixedPoint` coordinates.
pub type FixedRect1D = FixedRect<Dim1D>;
/// 2D hyperrectangle (rectangle) with `FixedPoint` coordinates.
pub type FixedRect2D = FixedRect<Dim2D>;
/// 3D hyperrectangle (rectangular prism) with `FixedPoint` coordinates.
pub type FixedRect3D = FixedRect<Dim3D>;
/// 4D hyperrectangle (rectangular duoprism) with `FixedPoint` coordinates.
pub type FixedRect4D = FixedRect<Dim4D>;
/// 5D hyperrectangle with `FixedPoint` coordinates.
pub type FixedRect5D = FixedRect<Dim5D>;
/// 6D hyperrectangle with `FixedPoint` coordinates.
pub type FixedRect6D = FixedRect<Dim6D>;

/// 1D hyperrectangle (linear range) with `R64` coordinates.
pub type FRect1D = FRect<Dim1D>;
/// 2D hyperrectangle (rectangle) with `R64` coordinates.
pub type FRect2D = FRect<Dim2D>;
/// 3D hyperrectangle (rectangular prism) with `R64` coordinates.
pub type FRect3D = FRect<Dim3D>;
/// 4D hyperrectangle (rectangular duoprism) with `R64` coordinates.
pub type FRect4D = FRect<Dim4D>;
/// 5D hyperrectangle with `R64` coordinates.
pub type FRect5D = FRect<Dim5D>;
/// 6D hyperrectangle with `R64` coordinates.
pub type FRect6D = FRect<Dim6D>;

/// 1D hyperrectangle (linear range) with `isize` coordinates.
pub type IRect1D = IRect<Dim1D>;
/// 2D hyperrectangle (rectangle) with `isize` coordinates.
pub type IRect2D = IRect<Dim2D>;
/// 3D hyperrectangle (rectangular prism) with `isize` coordinates.
pub type IRect3D = IRect<Dim3D>;
/// 4D hyperrectangle (rectangular duoprism) with `isize` coordinates.
pub type IRect4D = IRect<Dim4D>;
/// 5D hyperrectangle with `isize` coordinates.
pub type IRect5D = IRect<Dim5D>;
/// 6D hyperrectangle with `isize` coordinates.
pub type IRect6D = IRect<Dim6D>;

/// 1D hyperrectangle (linear range) with `usize` coordinates.
pub type URect1D = URect<Dim1D>;
/// 2D hyperrectangle (rectangle) with `usize` coordinates.
pub type URect2D = URect<Dim2D>;
/// 3D hyperrectangle (rectangular prism) with `usize` coordinates.
pub type URect3D = URect<Dim3D>;
/// 4D hyperrectangle (rectangular duoprism) with `usize` coordinates.
pub type URect4D = URect<Dim4D>;
/// 5D hyperrectangle with `usize` coordinates.
pub type URect5D = URect<Dim5D>;
/// 6D hyperrectangle with `usize` coordinates.
pub type URect6D = URect<Dim6D>;
