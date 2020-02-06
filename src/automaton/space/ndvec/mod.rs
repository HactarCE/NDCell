//! N-dimensional vectors.
//!
//! Until generic associated types work (see rust-lang#44265), this module and
//! ndrect are kind of a mess. We have to use a hacky DimFor trait to get
//! generic vectors to work, and generic-dimensioned vectors can't implement
//! Copy.
//!
//! Note that we use noisy_float's R64 type here instead of f64 so that we don't
//! have to deal with infinities and NaN, which really should NEVER show up in
//! NdVecs.

use noisy_float::prelude::R64;
use num::{BigInt, Num, One, Zero};
use std::cmp::Eq;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::*;

mod aliases;
mod any;
mod axis;
mod convert;
mod dim;
mod ops;

pub use aliases::*;
pub use any::*;
pub use axis::Axis::{U, V, W, X, Y, Z};
pub use axis::*;
pub use convert::*;
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
// Unfortunately this only works when the dimensionality is known, not when it
// is a type parameter. It's useful enough regardless that it's worth including.
impl<D: DimFor<N>, N: NdVecNum + Copy> Copy for NdVec<D, N> where D::Array: Copy {}

impl<D: DimFor<N>, N: NdVecNum + Display> Display for NdVec<D, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for &ax in D::Dim::axes() {
            if ax != X {
                write!(f, ", ")?;
            }
            write!(f, "{}", self[ax])?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

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
    /// Returns a vector consisting of all zeros.
    pub fn origin() -> Self {
        Self::default()
    }
    /// Returns true if the vector is all zeros, or false otherwise.
    pub fn is_zero(&self) -> bool {
        *self == Self::default()
    }
    /// Returns the unit vector pointing along the given axis.
    pub fn unit(axis: Axis) -> Self {
        let mut ret = Self::default();
        ret[axis] = N::one();
        ret
    }

    /// Constructs an NdVec using a function of an axis to generate each
    /// component.
    pub fn from_fn<F: FnMut(Axis) -> N>(mut generator: F) -> Self {
        let mut ret: Self = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = generator(ax);
        }
        ret
    }
    /// Applies a function to each component of this vector, constructing a new
    /// NdVec.
    pub fn map_fn<F: FnMut(Axis, &mut N)>(&mut self, mut f: F) {
        for &ax in D::Dim::axes() {
            f(ax, &mut self[ax]);
        }
    }
    /// Constructs an NdVec using the given value for all components.
    pub fn repeat<X: Into<N>>(value: X) -> Self {
        let value = value.into();
        Self::from_fn(|_| value.clone())
    }

    /// Converts an NdVec from one number type to another using
    /// std::convert::Into.
    pub fn convert<N2: NdVecNum>(&self) -> NdVec<D, N2>
    where
        D: DimFor<N2>,
        N: Into<N2>,
    {
        NdVec::from_fn(|ax| N2::from(self[ax].clone().into()))
    }

    /// Constructs an NdVec where each component is the minimum of the
    /// corresponding components in the two given vectors.
    pub fn min(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::min(&v1[ax], &v2[ax]).clone();
        }
        ret
    }
    /// Constructs an NdVec where each component is the maximum of the
    /// corresponding components in the two given vectors.
    pub fn max(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::max(&v1[ax], &v2[ax]).clone();
        }
        ret
    }

    /// Adds together all the components of this vector.
    pub fn sum(&self) -> N {
        let mut ret = N::zero();
        for &ax in D::Dim::axes() {
            ret += self[ax].clone();
        }
        ret
    }
    /// Multiplies together all the components of this vector.
    pub fn product(&self) -> N {
        let mut ret = N::one();
        for &ax in D::Dim::axes() {
            ret *= self[ax].clone();
        }
        ret
    }
}

impl<D: Dim> BigVec<D> {
    /// Constructs a new BigVec using isize components.
    pub fn big(isize_array: <D as DimFor<isize>>::Array) -> Self {
        NdVec::<D, isize>(isize_array).convert()
    }
}

#[cfg(test)]
mod tests;
