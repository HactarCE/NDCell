//! Division and modulo/remainder operations between an `NdVec` and a scalar.

use std::ops::*;

use super::*;
use crate::num::{Float, Integer};

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
    /// This is not implemented using std::ops::Div because most of the time we
    /// actually want floor division, and I don't want to accidentally use this
    /// kind of division in the wrong place.
    #[inline]
    pub fn div_to_zero(&self, other: &N) -> Self {
        Self::from_fn(|ax| self[ax].clone() / other.clone())
    }

    /// Integer modulo that rounds toward zero.
    ///
    /// This is not implemented using std::ops::Rem for the same reason as
    /// div_to_zero().
    #[inline]
    pub fn rem_to_zero(&self, other: &N) -> Self {
        Self::from_fn(|ax| self[ax].clone() % other.clone())
    }
}

// Implement division and modulo between an NdVec and a scalar (i.e. divide each
// coordinate by the scalar). This is only implemented for floating point NdVecs
// because integers will usually want div_floor instead.
impl<D: DimFor<N>, N: NdVecNum + Float, X> Div<X> for NdVec<D, N>
where
    NdVec<D, N>: DivAssign<X>,
{
    type Output = Self;

    #[inline]
    fn div(self, other: X) -> Self {
        let mut ret = self;
        ret /= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum + Float, X> Div<X> for &'a NdVec<D, N>
where
    NdVec<D, N>: DivAssign<X>,
{
    type Output = NdVec<D, N>;

    #[inline]
    fn div(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret /= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum + Float, X: Copy> DivAssign<X> for NdVec<D, N>
where
    N: DivAssign<X>,
{
    #[inline]
    fn div_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret /= other);
    }
}
impl<D: DimFor<N>, N: NdVecNum + Float, X> Rem<X> for NdVec<D, N>
where
    NdVec<D, N>: RemAssign<X>,
{
    type Output = Self;

    #[inline]
    fn rem(self, other: X) -> Self {
        let mut ret = self;
        ret %= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum + Float, X> Rem<X> for &'a NdVec<D, N>
where
    NdVec<D, N>: RemAssign<X>,
{
    type Output = NdVec<D, N>;

    #[inline]
    fn rem(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret %= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum + Float, X: Copy> RemAssign<X> for NdVec<D, N>
where
    N: RemAssign<X>,
{
    #[inline]
    fn rem_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret %= other);
    }
}
