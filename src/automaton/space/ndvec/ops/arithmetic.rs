//! Addition, subtraction, and multiplication between an NdVec with a scalar.

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
impl<D: DimFor<N>, N: NdVecNum> AddAssign<N> for NdVec<D, N>
where
    N: AddAssign<N> + Copy,
{
    fn add_assign(&mut self, other: N) {
        self.map_fn(|_ax, ret| *ret += other);
    }
}
impl<'a, D: Dim> AddAssign<&'a BigInt> for BigVec<D> {
    fn add_assign(&mut self, other: &BigInt) {
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
impl<D: DimFor<N>, N: NdVecNum> SubAssign<N> for NdVec<D, N>
where
    N: SubAssign<N> + Copy,
{
    fn sub_assign(&mut self, other: N) {
        self.map_fn(|_ax, ret| *ret -= other);
    }
}
impl<'a, D: Dim> SubAssign<&'a BigInt> for BigVec<D> {
    fn sub_assign(&mut self, other: &BigInt) {
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
impl<D: DimFor<N>, N: NdVecNum> MulAssign<N> for NdVec<D, N>
where
    N: MulAssign<N> + Copy,
{
    fn mul_assign(&mut self, other: N) {
        self.map_fn(|_ax, ret| *ret *= other);
    }
}
impl<'a, D: Dim> MulAssign<&'a BigInt> for BigVec<D> {
    fn mul_assign(&mut self, other: &BigInt) {
        self.map_fn(|_ax, ret| *ret *= other);
    }
}
