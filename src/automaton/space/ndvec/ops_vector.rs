//! Operations between two NdVecs (and unary negation operator).

use std::ops::*;

use super::*;

// Implement negation of an NdVec (i.e. negate each coordinate).
impl<D: DimFor<N>, N: NdVecNum + Neg> Neg for NdVec<D, N>
where
    N: Neg<Output = N>,
{
    type Output = Self;
    fn neg(self) -> Self {
        -&self
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum + Neg> Neg for &'a NdVec<D, N>
where
    N: Neg<Output = N>,
{
    type Output = NdVec<D, N>;
    fn neg(self) -> Self::Output {
        NdVec::from_fn(|ax| -self[ax].clone())
    }
}

// Implement elementwise addition between two NdVecs.
impl<D: DimFor<N1> + DimFor<N2>, N1: NdVecNum, N2: NdVecNum + Copy> AddAssign<NdVec<D, N2>>
    for NdVec<D, N1>
where
    N1: AddAssign<N2>,
{
    fn add_assign(&mut self, other: NdVec<D, N2>) {
        self.map_fn(|ax, ret| *ret += other[ax]);
    }
}

// Implement elementwise subtraction between two NdVecs.
impl<D: DimFor<N1> + DimFor<N2>, N1: NdVecNum, N2: NdVecNum + Copy> SubAssign<NdVec<D, N2>>
    for NdVec<D, N1>
where
    N1: SubAssign<N2>,
{
    fn sub_assign(&mut self, other: NdVec<D, N2>) {
        self.map_fn(|ax, ret| *ret -= other[ax]);
    }
}

// Implement elementwise multiplication between two NdVecs.
impl<D: DimFor<N1> + DimFor<N2>, N1: NdVecNum, N2: NdVecNum + Copy> MulAssign<NdVec<D, N2>>
    for NdVec<D, N1>
where
    N1: MulAssign<N2>,
{
    fn mul_assign(&mut self, other: NdVec<D, N2>) {
        self.map_fn(|ax, ret| *ret *= other[ax]);
    }
}
