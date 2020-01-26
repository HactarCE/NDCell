//! Bitwise operations between an NdVec and a scalar.

use std::ops::*;

use super::*;

// Implement bitwise AND between an NdVec and a scalar (i.e. AND each
// coordinate with the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> BitAnd<X> for NdVec<D, N>
where
    N: BitAndAssign<X>,
{
    type Output = Self;
    fn bitand(self, other: X) -> Self {
        let mut ret = self;
        ret &= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> BitAnd<X> for &'a NdVec<D, N>
where
    N: BitAndAssign<X>,
{
    type Output = NdVec<D, N>;
    fn bitand(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret &= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> BitAndAssign<X> for NdVec<D, N>
where
    N: BitAndAssign<X>,
{
    fn bitand_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret &= other);
    }
}

// Implement bitwise OR between an NdVec and a scalar (i.e. OR each
// coordinate with the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> BitOr<X> for NdVec<D, N>
where
    N: BitOrAssign<X>,
{
    type Output = Self;
    fn bitor(self, other: X) -> Self {
        let mut ret = self;
        ret |= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> BitOr<X> for &'a NdVec<D, N>
where
    N: BitOrAssign<X>,
{
    type Output = NdVec<D, N>;
    fn bitor(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret |= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> BitOrAssign<X> for NdVec<D, N>
where
    N: BitOrAssign<X>,
{
    fn bitor_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret |= other);
    }
}

// Implement bitwise XOR between an NdVec and a scalar (i.e. XOR each
// coordinate with the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> BitXor<X> for NdVec<D, N>
where
    N: BitXorAssign<X>,
{
    type Output = Self;
    fn bitxor(self, other: X) -> Self {
        let mut ret = self;
        ret ^= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> BitXor<X> for &'a NdVec<D, N>
where
    N: BitXorAssign<X>,
{
    type Output = NdVec<D, N>;
    fn bitxor(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret ^= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> BitXorAssign<X> for NdVec<D, N>
where
    N: BitXorAssign<X>,
{
    fn bitxor_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret ^= other);
    }
}
// Implement bitwise left-shift between an NdVec and a scalar (i.e.
// left-shift each coordinate by the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> Shl<X> for NdVec<D, N>
where
    N: ShlAssign<X>,
{
    type Output = Self;
    fn shl(self, other: X) -> Self {
        let mut ret = self;
        ret <<= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> Shl<X> for &'a NdVec<D, N>
where
    N: ShlAssign<X>,
{
    type Output = NdVec<D, N>;
    fn shl(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret <<= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> ShlAssign<X> for NdVec<D, N>
where
    N: ShlAssign<X>,
{
    fn shl_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret <<= other);
    }
}

// Implement bitwise right-shift between an NdVec and a scalar (i.e.
// right-shift each coordinate by the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> Shr<X> for NdVec<D, N>
where
    N: ShrAssign<X>,
{
    type Output = Self;
    fn shr(self, other: X) -> Self {
        let mut ret = self;
        ret >>= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> Shr<X> for &'a NdVec<D, N>
where
    N: ShrAssign<X>,
{
    type Output = NdVec<D, N>;
    fn shr(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret >>= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> ShrAssign<X> for NdVec<D, N>
where
    N: ShrAssign<X>,
{
    fn shr_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret >>= other);
    }
}
