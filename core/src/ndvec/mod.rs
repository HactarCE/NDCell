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

pub mod aliases;
mod any;
mod convert;
mod ops;

pub use aliases::*;
pub use any::AnyDimVec;

use crate::axis::Axis;
use crate::dim::{Dim, DimFor};
use crate::num::{BigInt, FixedPoint, NdVecNum, Signed};

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
    #[inline]
    fn index(&self, axis: Axis) -> &N {
        &self.0.as_ref()[axis as usize]
    }
}
impl<D: DimFor<N>, N: NdVecNum> IndexMut<Axis> for NdVec<D, N> {
    #[inline]
    fn index_mut(&mut self, axis: Axis) -> &mut N {
        &mut self.0.as_mut()[axis as usize]
    }
}

impl<D: DimFor<N>, N: NdVecNum> NdVec<D, N> {
    /// Creates a vector consisting of all zeros.
    #[inline]
    pub fn origin() -> Self {
        Self::default()
    }
    /// Returns true if the vector is all zeros, or false otherwise.
    #[inline]
    pub fn is_zero(&self) -> bool {
        *self == Self::default()
    }
    /// Creates a unit vector pointing along `axis`.
    #[inline]
    pub fn unit(axis: Axis) -> Self {
        let mut ret = Self::default();
        ret[axis] = N::one();
        ret
    }

    /// Creates a vector by evaluating `generator` for each axis.
    #[inline]
    pub fn from_fn(mut generator: impl FnMut(Axis) -> N) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = generator(ax);
        }
        ret
    }
    /// Creates a vector by evaluating `generator` for each axis, and returns
    /// `None` if any component is `None`.
    #[inline]
    pub fn try_from_fn(mut generator: impl FnMut(Axis) -> Option<N>) -> Option<Self> {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = generator(ax)?;
        }
        Some(ret)
    }
    /// Creates a vector using `value` for all components.
    #[inline]
    pub fn repeat(value: N) -> Self {
        Self::from_fn(|_| value.clone())
    }

    /// Creates a vector by taking the absolute value of each component of this
    /// one.
    #[inline]
    pub fn abs(&self) -> Self
    where
        N: Signed,
    {
        Self::from_fn(|ax| self[ax].abs())
    }

    /// Creates a vector by taking the minimum of the corresponding components
    /// in `v1` and `v2`.
    #[inline]
    pub fn min(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::min(&v1[ax], &v2[ax]).clone();
        }
        ret
    }
    /// Creates a vector by taking the maximum of the corresponding components
    /// in `v1` and `v2`.
    #[inline]
    pub fn max(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::max(&v1[ax], &v2[ax]).clone();
        }
        ret
    }

    /// Returns the sum of the components of the vector.
    #[inline]
    pub fn sum(&self) -> N {
        let mut ret = N::zero();
        for &ax in D::Dim::axes() {
            ret += &self[ax];
        }
        ret
    }
    /// Returns the product of the components of the vector.
    #[inline]
    pub fn product(&self) -> N {
        let mut ret = N::one();
        for &ax in D::Dim::axes() {
            ret *= &self[ax];
        }
        ret
    }

    /// Returns the `Axis` of the component that is the most positive (or one of
    /// them, if there is a tie).
    #[inline]
    pub fn max_axis<'a>(&'a self) -> Axis {
        *D::Dim::axes()
            .into_iter()
            .max_by_key(|&&ax| &self[ax])
            .unwrap()
    }
    /// Returns the `Axis` of the component that is the most negative (or one of
    /// them, if there is a tie).
    #[inline]
    pub fn min_axis<'a>(&'a self) -> Axis {
        *D::Dim::axes()
            .into_iter()
            .min_by_key(|&&ax| &self[ax])
            .unwrap()
    }

    /// Returns the component that is the most positive.
    #[inline]
    pub fn max_component<'a>(&'a self) -> &N {
        &self[self.max_axis()]
    }
    /// Returns the component that is the most negative.
    #[inline]
    pub fn min_component<'a>(&'a self) -> &N {
        &self[self.min_axis()]
    }
}

impl<D: Dim> BigVec<D> {
    #[inline]
    /// Constructs a new BigVec using isize components.
    pub fn big(isize_array: <D as DimFor<isize>>::Array) -> Self {
        NdVec::<D, isize>(isize_array).to_bigvec()
    }
}

#[cfg(test)]
pub fn proptest_ivec<'a, D: Dim, S: 'a + proptest::strategy::Strategy<Value = i32>>(
    component_strategy: S,
) -> impl 'a + proptest::strategy::Strategy<Value = IVec<D>> {
    use proptest::strategy::Strategy;

    proptest::collection::vec(component_strategy, D::NDIM)
        .prop_map(|v| NdVec::from_fn(|ax| v[ax as usize] as isize))
}

#[cfg(test)]
pub fn proptest_bigvec<'a, D: Dim, S: 'a + proptest::strategy::Strategy<Value = i32>>(
    component_strategy: S,
) -> impl 'a + proptest::strategy::Strategy<Value = BigVec<D>> {
    use proptest::strategy::Strategy;

    proptest::collection::vec(component_strategy, D::NDIM)
        .prop_map(|v| NdVec::from_fn(|ax| v[ax as usize].into()))
}

#[cfg(test)]
pub fn proptest_ivec2d<'a, S: 'a + proptest::strategy::Strategy<Value = i32>>(
    component_strategy: S,
) -> impl 'a + proptest::strategy::Strategy<Value = IVec2D> {
    proptest_ivec(component_strategy)
}

#[cfg(test)]
pub fn proptest_ivec3d<'a, S: 'a + proptest::strategy::Strategy<Value = i32>>(
    component_strategy: S,
) -> impl 'a + proptest::strategy::Strategy<Value = IVec2D> {
    proptest_ivec(component_strategy)
}

#[cfg(test)]
impl proptest::arbitrary::Arbitrary for IVec3D {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    fn arbitrary_with(_: ()) -> Self::Strategy {
        use proptest::strategy::Strategy;

        proptest_ivec(-100..=100).boxed()
    }
}

#[cfg(test)]
mod tests;
