//! N-dimensional vectors.
//!
//! Until generic associated types are stable (see rust-lang#44265), this module
//! and `ndrect` are kind of a mess. We have to use a hacky `DimFor` trait to
//! get generic-dimensioned vectors to work, and generic-dimensioned vectors
//! can't implement `Copy` (although they probably couldn't with GATs either).
//!
//! Note that we use `noisy_float`'s `R64` type instead of `f64` so that we
//! don't have to deal with infinities and NaN, which should never appear in
//! `NdVec`s.

use itertools::Itertools;
use std::cmp::Eq;
use std::fmt;
use std::hash::Hash;
use std::ops::*;

mod aliases;
mod any;
mod convert;
mod ops;

pub use aliases::*;
pub use any::AnyDimVec;

use crate::dim::{Dim, DimFor};
use crate::num::{BigInt, NdVecNum};
use crate::Axis;

/// `D`-dimensional vector with coordinates of type `N`.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct NdVec<D: DimFor<N>, N: NdVecNum>(pub D::Array);

// Implement `Copy` when `N` is `Copy`.
//
// Unfortunately this only works when `D` is known, and not in generic contexts,
// but this is useful enough regardless that it's worth including. Just keep in
// mind whenever you see `some_ndvec.clone()` that it is "free" (no heap
// allocation) if the type parameter `N` is `Copy`.
impl<D: DimFor<N>, N: NdVecNum + Copy> Copy for NdVec<D, N> where D::Array: Copy {}

impl<D: DimFor<N>, N: NdVecNum + fmt::Display> fmt::Display for NdVec<D, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for &ax in D::Dim::axes() {
            if ax != Axis::X {
                write!(f, ", ")?;
            }
            fmt::Display::fmt(&self[ax], f)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

// Implement indexing using `Axis`.
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
    /// Creates a vector consisting of all zeros.
    pub fn origin() -> Self {
        Self::default()
    }
    /// Returns true if the vector is all zeros, or false otherwise.
    pub fn is_zero(&self) -> bool {
        *self == Self::default()
    }
    /// Creates a unit vector pointing along `axis`.
    pub fn unit(axis: Axis) -> Self {
        let mut ret = Self::default();
        ret[axis] = N::one();
        ret
    }

    /// Creates a vector by evaluating `generator` for each axis.
    pub fn from_fn(mut generator: impl FnMut(Axis) -> N) -> Self {
        let mut ret: Self = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = generator(ax);
        }
        ret
    }
    /// Creates a vector by evaluating `f` on each each component of this one.
    pub fn map_fn(&mut self, mut f: impl FnMut(Axis, &mut N)) {
        for &ax in D::Dim::axes() {
            f(ax, &mut self[ax]);
        }
    }
    /// Creates a vector using `value` for all components.
    pub fn repeat(value: N) -> Self {
        Self::from_fn(|_| value.clone())
    }

    /// Creates a vector by taking the minimum of the corresponding components
    /// in `v1` and `v2`.
    pub fn min(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::min(&v1[ax], &v2[ax]).clone();
        }
        ret
    }
    /// Creates a vector by taking the maximum of the corresponding components
    /// in `v1` and `v2`.
    pub fn max(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::max(&v1[ax], &v2[ax]).clone();
        }
        ret
    }

    /// Returns the sum of the components of the vector.
    pub fn sum(&self) -> N {
        let mut ret = N::zero();
        for &ax in D::Dim::axes() {
            ret += self[ax].clone();
        }
        ret
    }
    /// Returns the product of the components of the vector.
    pub fn product(&self) -> N {
        let mut ret = N::one();
        for &ax in D::Dim::axes() {
            ret *= self[ax].clone();
        }
        ret
    }

    /// Returns the `Axis` of the component that is the most positive (or one of
    /// them, if there is a tie).
    pub fn max_axis<X: std::cmp::Ord>(&self, key: impl Fn(Axis, &N) -> X) -> Axis {
        *D::Dim::axes()
            .into_iter()
            .max_by_key(|&&ax| key(ax, &self[ax]))
            .unwrap()
    }

    /// Returns the `Axis` of the component that is the most negative (or one of
    /// them, if there is a tie).
    pub fn min_axis<X: std::cmp::Ord>(&self, key: impl Fn(Axis, &N) -> X) -> Axis {
        *D::Dim::axes()
            .into_iter()
            .min_by_key(|&&ax| key(ax, &self[ax]))
            .unwrap()
    }
}

impl<D: Dim> BigVec<D> {
    /// Constructs a new BigVec using isize components.
    pub fn big(isize_array: <D as DimFor<isize>>::Array) -> Self {
        NdVec::<D, isize>(isize_array).to_bigvec()
    }
}

#[cfg(test)]
mod tests;
