//! Operations involving `NdRects`.

use std::ops::*;

use super::*;

// Implement addition and subtraction on anything that can be added/subtracted
// to/from an `NdVec`.
impl<D: DimFor<N>, N: NdVecNum, X> Add<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + Add<X, Output = NdVec<D, N>>,
{
    type Output = Self;

    #[inline]
    fn add(self, operand: X) -> Self {
        Self {
            start: self.start + operand,
            size: self.size,
        }
    }
}
impl<D: DimFor<N>, N: NdVecNum, X> AddAssign<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + AddAssign<X>,
{
    #[inline]
    fn add_assign(&mut self, operand: X) {
        self.start += operand
    }
}
impl<D: DimFor<N>, N: NdVecNum, X> Sub<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + Sub<X, Output = NdVec<D, N>>,
{
    type Output = Self;

    #[inline]
    fn sub(self, operand: X) -> Self {
        Self {
            start: self.start - operand,
            size: self.size,
        }
    }
}
impl<D: DimFor<N>, N: NdVecNum, X> SubAssign<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + SubAssign<X>,
{
    #[inline]
    fn sub_assign(&mut self, operand: X) {
        self.start -= operand
    }
}

impl<D: DimFor<N>, N: NdVecNum, X> Mul<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
    Self: MulAssign<X>,
{
    type Output = Self;

    #[inline]
    fn mul(self, operand: X) -> Self {
        let mut ret = self;
        ret *= operand;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> MulAssign<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + MulAssign<X>,
{
    /// Scales the rectangle by the given value along each axis.
    ///
    /// # Panics
    ///
    /// This operation panics if the right-hand side is zero or negative along
    /// any axis.
    #[inline]
    fn mul_assign(&mut self, operand: X) {
        self.start *= operand;
        self.size *= operand;
        assert!(
            self.size > NdVec::origin(),
            "NdRect must have positive volume"
        );
    }
}

// Implement integer division.
impl<D: DimFor<N>, N: NdVecNum + Integer> NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
{
    /// "Outward-rounded" integer division; returns the largest rectangle that is
    /// the given fraction of the size of the original.
    #[inline]
    pub fn div_outward(&self, other: &N) -> Self {
        Self::span(self.min().div_floor(other), self.max().div_floor(other))
    }
}

// Implement float division.
impl<D: DimFor<N>, N: NdVecNum + Float, X> Div<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec,
    Self: DivAssign<X>,
{
    type Output = Self;

    #[inline]
    fn div(self, operand: X) -> Self {
        let mut ret = self;
        ret /= operand;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum + Float, X: Copy> DivAssign<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + DivAssign<X>,
{
    #[inline]
    fn div_assign(&mut self, operand: X) {
        self.start /= operand;
        self.size /= operand;
        assert!(
            self.size > NdVec::origin(),
            "NdRect must have positive volume"
        );
    }
}

// Implement left shift and right shift on anything that an NdVec can be
// left/right-shifted by.
impl<D: DimFor<N>, N: NdVecNum, X: Copy> Shl<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + Shl<X, Output = NdVec<D, N>>,
{
    type Output = Self;

    #[inline]
    fn shl(self, operand: X) -> Self {
        Self {
            start: self.start << operand,
            size: self.size << operand,
        }
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> ShlAssign<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + ShlAssign<X>,
{
    #[inline]
    fn shl_assign(&mut self, operand: X) {
        self.start <<= operand;
        self.size <<= operand;
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> Shr<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + Shr<X, Output = NdVec<D, N>>,
{
    type Output = Self;

    #[inline]
    fn shr(self, operand: X) -> Self {
        let max = self.max() >> operand;
        let min = self.start >> operand;
        Self::span(min, max)
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> ShrAssign<X> for NdRect<D, N>
where
    NdVec<D, N>: NdRectVec + Shr<X, Output = NdVec<D, N>>,
{
    #[inline]
    fn shr_assign(&mut self, operand: X) {
        let max = self.max() >> operand;
        let min = self.min() >> operand;
        *self = Self::span(min, max)
    }
}
