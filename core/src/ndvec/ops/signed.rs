//! Operations between two `NdVec`s (also unary negation).

use std::ops::*;

use super::*;
use crate::num::Signed;

impl<D: DimFor<N>, N: NdVecNum + Signed> NdVec<D, N> {
    /// Returns a vector consisting of the sign of each member of this one. See
    /// num::Signed::signum() for more details.

    #[inline]
    pub fn signum(&self) -> Self {
        Self::from_fn(|ax| self[ax].signum())
    }
}

// Implement negation of an NdVec (i.e. negate each coordinate).
impl<D: DimFor<N>, N: NdVecNum + Neg> Neg for NdVec<D, N>
where
    for<'a> &'a N: Neg<Output = N>,
{
    type Output = Self;

    #[inline]
    fn neg(self) -> Self {
        -&self
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum + Neg> Neg for &'a NdVec<D, N>
where
    &'a N: Neg<Output = N>,
{
    type Output = NdVec<D, N>;

    #[inline]
    fn neg(self) -> Self::Output {
        NdVec::from_fn(|ax| -&self[ax])
    }
}
