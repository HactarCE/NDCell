//! N-dimensional vectors.
//!
//! Until generic associated types work (see rust-lang#44265), this module and
//! ndrect are kind of a mess. There's a ton of macros in order to implement all
//! the various operations on NdVecs without writing thousands of lines of
//! repetitive code.
//!
//! Note that we use noisy_float's R64 type here instead of f64 so that we don't
//! have to deal with infinities and NaN, which really should NEVER show up in
//! NdVecs.

use noisy_float::types::R64;
use num::{BigInt, Num, One, ToPrimitive, Zero};
use std::cmp::Eq;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::*;

mod aliases;
mod axis;
mod dim;
mod ops;

pub use aliases::*;
pub use axis::Axis::{U, V, W, X, Y, Z};
pub use axis::*;
pub use dim::*;

/// A "trait alias" for types that can be used as coordinates in an NdVec.
pub trait NdVecNum:
    Debug + Default + Clone + Eq + Hash + Ord + Num + AddAssign + MulAssign
{
    /// The minimum size for an NdRect using this number type as coordinates.
    /// For integers, this is 1; for floats, this is 0.
    fn get_min_rect_size() -> Self;
}
impl NdVecNum for BigInt {
    fn get_min_rect_size() -> Self {
        Self::one()
    }
}
impl NdVecNum for R64 {
    fn get_min_rect_size() -> Self {
        Self::zero()
    }
}
impl NdVecNum for isize {
    fn get_min_rect_size() -> Self {
        1
    }
}
impl NdVecNum for usize {
    fn get_min_rect_size() -> Self {
        1
    }
}
impl NdVecNum for u8 {
    fn get_min_rect_size() -> Self {
        1
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
/// A set of coordinates for a given dimensionality.
pub struct NdVec<D: DimFor<N>, N: NdVecNum>(pub D::Array);

// Implement Copy when coordinate type is Copy.
//
// Unfortunately, for a number of subtle reasons, this only works when the
// dimensionality is known, not when it is a type parameter. This is still
// useful enough that it's worth including.
impl<D: DimFor<N>, N: NdVecNum + Copy> Copy for NdVec<D, N> where D::Array: Copy {}

// Implement indexing using Axis.
impl<D: DimFor<N>, N: NdVecNum> Index<Axis> for NdVec<D, N> {
    type Output = N;
    fn index(&self, axis: Axis) -> &N {
        &self.0.as_ref()[axis as usize]
    }
}
impl<D: DimFor<N>, N: NdVecNum> IndexMut<Axis> for NdVec<D, N> {
    fn index_mut(&mut self, axis: Axis) -> &mut N {
        &mut self.0.as_mut()[axis as usize]
    }
}

impl<D: DimFor<N>, N: NdVecNum> NdVec<D, N> {
    pub fn origin() -> Self {
        Self::default()
    }
    pub fn is_zero(&self) -> bool {
        *self == Self::default()
    }
    pub fn unit(axis: Axis) -> Self {
        let mut ret = Self::default();
        ret[axis] = N::one();
        ret
    }

    pub fn from_fn<F: FnMut(Axis) -> N>(mut generator: F) -> Self {
        let mut ret: Self = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = generator(ax);
        }
        ret
    }
    pub fn map_fn<F: FnMut(Axis, &mut N)>(&mut self, mut f: F) {
        for &ax in D::Dim::axes() {
            f(ax, &mut self[ax]);
        }
    }

    pub fn convert<N2: NdVecNum>(&self) -> NdVec<D, N2>
    where
        D: DimFor<N2>,
        N2: From<N>,
    {
        NdVec::from_fn(|ax| N2::from(self[ax].clone().into()))
    }

    pub fn min(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::min(&v1[ax], &v2[ax]).clone();
        }
        ret
    }
    pub fn max(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::max(&v1[ax], &v2[ax]).clone();
        }
        ret
    }

    /// Multiplies together all the coordinates of the vector.
    pub fn product(&self) -> N {
        let mut ret = N::one();
        for &ax in D::Dim::axes() {
            ret *= self[ax].clone();
        }
        ret
    }
}

pub trait AsUVec<D: Dim> {
    /// Converts the NdVec to a UVec, panicking if it does not fit.
    fn as_uvec(&self) -> UVec<D>;
}
pub trait AsIVec<D: Dim> {
    /// Converts the NdVec to an IVec, panicking if it does not fit.
    fn as_ivec(&self) -> IVec<D>;
}

impl<D: Dim> AsUVec<D> for IVec<D> {
    fn as_uvec(&self) -> UVec<D> {
        UVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this IVec into a UVec")
        })
    }
}
impl<D: Dim> AsIVec<D> for UVec<D> {
    fn as_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
}

impl<D: Dim> AsIVec<D> for BigVec<D> {
    fn as_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .to_isize()
                .expect("Cannot convert such a large BigVec into an IVec")
        })
    }
}

impl<D: DimFor<N>, N: NdVecNum, X: Num + Into<N>> From<X> for NdVec<D, N> {
    fn from(value: X) -> Self {
        let value = value.into();
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = value.clone();
        }
        ret
    }
}

#[cfg(test)]
mod tests;
