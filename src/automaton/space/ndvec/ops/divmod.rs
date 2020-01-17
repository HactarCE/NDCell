//! Division and modulo/remainder operations between an NdVec and a scalar.

use num::{Float, Integer};
use std::ops::*;

use super::*;

// Implement floored euclidean division between an NdVec and a scalar
// (i.e. divide each coordinate by the scalar). Note that this is not
// the same as Rust's normal integer division operator.
//
// These are the only operations not implemented as traits.
impl<D: DimFor<N>, N: NdVecNum + Integer> NdVec<D, N> {
    /// Floored integer division.
    pub fn div_floor(self, other: &N) -> Self {
        let mut ret = self;
        ret.map_fn(|_ax, ret| *ret = ret.div_floor(other));
        ret
    }
    /// Floored integer modulo.
    pub fn mod_floor(self, other: &N) -> Self {
        let mut ret = self;
        ret.map_fn(|_ax, ret| *ret = ret.mod_floor(other));
        ret
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
    fn div_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret /= other);
    }
}
impl<D: DimFor<N>, N: NdVecNum + Float, X> Rem<X> for NdVec<D, N>
where
    NdVec<D, N>: RemAssign<X>,
{
    type Output = Self;
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
    fn rem_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret %= other);
    }
}
