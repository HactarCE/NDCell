//! Operations on iterators over `NdVec`s.

use std::iter::{Product, Sum};

use super::*;

impl<D: DimFor<N>, N: NdVecNum, X> Product<X> for NdVec<D, N>
where
    NdVec<D, N>: MulAssign<X>,
{
    fn product<I: Iterator<Item = X>>(iter: I) -> Self {
        let mut ret = Self::zero();
        for x in iter {
            ret *= x;
        }
        ret
    }
}

impl<D: DimFor<N>, N: NdVecNum, X> Sum<X> for NdVec<D, N>
where
    NdVec<D, N>: AddAssign<X>,
{
    fn sum<I: Iterator<Item = X>>(iter: I) -> Self {
        let mut ret = Self::zero();
        for x in iter {
            ret += x;
        }
        ret
    }
}
