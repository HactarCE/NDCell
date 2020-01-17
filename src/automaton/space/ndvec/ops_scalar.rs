//! Operations between an NdVec and a scalar.

use num::{Float, Integer};
use std::ops::*;

use super::*;

// Implement addition between an NdVec and a scalar (i.e. add the scalar
// to each coordinate).
impl<D: DimFor<N>, N: NdVecNum, X> Add<X> for NdVec<D, N>
where
    NdVec<D, N>: AddAssign<X>,
{
    type Output = Self;
    fn add(self, other: X) -> Self {
        let mut ret = self;
        ret += other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X> Add<X> for &'a NdVec<D, N>
where
    NdVec<D, N>: AddAssign<X>,
{
    type Output = NdVec<D, N>;
    fn add(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret += other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> AddAssign<X> for NdVec<D, N>
where
    N: AddAssign<X>,
{
    fn add_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret += other);
    }
}

// Implement subtraction between an NdVec and a scalar (i.e. subtract
// the scalar from each coordinate).
impl<D: DimFor<N>, N: NdVecNum, X> Sub<X> for NdVec<D, N>
where
    NdVec<D, N>: SubAssign<X>,
{
    type Output = Self;
    fn sub(self, other: X) -> Self {
        let mut ret = self;
        ret -= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X> Sub<X> for &'a NdVec<D, N>
where
    NdVec<D, N>: SubAssign<X>,
{
    type Output = NdVec<D, N>;
    fn sub(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret -= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> SubAssign<X> for NdVec<D, N>
where
    N: SubAssign<X>,
{
    fn sub_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret -= other);
    }
}

// Implement multiplication between an NdVec and a scalar (i.e. multiply
// each coordinate by the scalar).
impl<D: DimFor<N>, N: NdVecNum, X> Mul<X> for NdVec<D, N>
where
    NdVec<D, N>: MulAssign<X>,
{
    type Output = Self;
    fn mul(self, other: X) -> Self {
        let mut ret = self;
        ret *= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X> Mul<X> for &'a NdVec<D, N>
where
    NdVec<D, N>: MulAssign<X>,
{
    type Output = NdVec<D, N>;
    fn mul(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret *= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> MulAssign<X> for NdVec<D, N>
where
    N: MulAssign<X>,
{
    fn mul_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret *= other);
    }
}

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
