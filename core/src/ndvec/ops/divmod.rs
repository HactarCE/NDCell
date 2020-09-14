//! Division and modulo/remainder operations between an `NdVec` and a scalar.

use std::ops::*;

use super::*;
use crate::num::{Integer, R64};

// Implement floored and ceiled euclidean division between an NdVec and a scalar
// (i.e. divide each coordinate by the scalar). Note that these are not the same
// as Rust's normal integer division operator.
//
// These are the only operations not implemented as traits.
impl<D: DimFor<N>, N: NdVecNum + Integer> NdVec<D, N> {
    /// Floored integer division.
    #[inline]
    pub fn div_floor(&self, other: &N) -> Self {
        Self::from_fn(|ax| self[ax].div_floor(other))
    }

    /// Ceiled integer division.
    #[inline]
    pub fn div_ceil(&self, other: &N) -> Self {
        Self::from_fn(|ax| self[ax].div_ceil(other))
    }

    /// Floored integer modulo.
    #[inline]
    pub fn mod_floor(&self, other: &N) -> Self {
        Self::from_fn(|ax| self[ax].mod_floor(other))
    }

    /// Integer division that rounds toward zero.
    ///
    /// This is not implemented using `std::ops::Div` because most of the time
    /// we actually want floor division, and it would be too easy to
    /// accidentally use this kind of division in the wrong place.
    #[inline]
    pub fn div_to_zero<'a, X: Copy>(&'a self, other: X) -> Self
    where
        &'a N: Div<X, Output = N>,
    {
        Self::from_fn(|ax| &self[ax] / other)
    }

    /// Integer modulo that rounds toward zero.
    ///
    /// This is not implemented using `std::ops::Rem` for the same reason as
    /// `div_to_zero()`.
    #[inline]
    pub fn rem_to_zero<'a, X: Copy>(&'a self, other: X) -> Self
    where
        &'a N: Rem<X, Output = N>,
    {
        Self::from_fn(|ax| &self[ax] % other)
    }
}

// Implement division and modulo between an NdVec and a scalar (i.e. divide each
// coordinate by the scalar). This is only implemented for non-integer NdVecs
// because integers will usually want `div_floor()` instead.

impl_multi_ndvec_ops!(impl / for FixedPoint);
impl_multi_ndvec_ops!(impl % for FixedPoint);
impl_multi_ndvec_ops!(impl / f64 for FixedPoint);
impl_multi_ndvec_ops!(impl % f64 for FixedPoint);

impl<D: DimFor<R64>, X: Copy> Div<X> for FVec<D>
where
    R64: Div<X, Output = R64>,
{
    type Output = FVec<D>;

    #[inline]
    fn div(self, other: X) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] / other)
    }
}
impl<'a, D: DimFor<R64>, X: Copy> Div<X> for &'a FVec<D>
where
    R64: Div<X, Output = R64>,
{
    type Output = FVec<D>;

    #[inline]
    fn div(self, other: X) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] / other)
    }
}
impl<D: DimFor<R64>> Div<FVec<D>> for FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn div(self, other: FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] / other[ax])
    }
}
impl<'a, D: DimFor<R64>> Div<FVec<D>> for &'a FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn div(self, other: FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] / other[ax])
    }
}
impl<'a, D: DimFor<R64>> Div<&'a FVec<D>> for FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn div(self, other: &'a FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] / other[ax])
    }
}
impl<'a, D: DimFor<R64>> Div<&'a FVec<D>> for &'a FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn div(self, other: &'a FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] / other[ax])
    }
}
impl<D: DimFor<R64>, X> DivAssign<X> for FVec<D>
where
    Self: Div<X, Output = Self>,
{
    fn div_assign(&mut self, other: X) {
        // This clone is actually free because `FVec` is `Copy`.
        *self = self.clone() / other;
    }
}

impl<D: DimFor<R64>, X: Copy> Rem<X> for FVec<D>
where
    R64: Rem<X, Output = R64>,
{
    type Output = FVec<D>;

    #[inline]
    fn rem(self, other: X) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] % other)
    }
}
impl<'a, D: DimFor<R64>, X: Copy> Rem<X> for &'a FVec<D>
where
    R64: Rem<X, Output = R64>,
{
    type Output = FVec<D>;

    #[inline]
    fn rem(self, other: X) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] % other)
    }
}
impl<D: DimFor<R64>> Rem<FVec<D>> for FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn rem(self, other: FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] % other[ax])
    }
}
impl<'a, D: DimFor<R64>> Rem<FVec<D>> for &'a FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn rem(self, other: FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] % other[ax])
    }
}
impl<'a, D: DimFor<R64>> Rem<&'a FVec<D>> for FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn rem(self, other: &'a FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] % other[ax])
    }
}
impl<'a, D: DimFor<R64>> Rem<&'a FVec<D>> for &'a FVec<D> {
    type Output = FVec<D>;

    #[inline]
    fn rem(self, other: &'a FVec<D>) -> Self::Output {
        NdVec::from_fn(|ax| self[ax] % other[ax])
    }
}
impl<D: DimFor<R64>, X> RemAssign<X> for FVec<D>
where
    Self: Rem<X, Output = Self>,
{
    fn rem_assign(&mut self, other: X) {
        // This clone is actually free because `FVec` is `Copy`.
        *self = self.clone() % other;
    }
}
