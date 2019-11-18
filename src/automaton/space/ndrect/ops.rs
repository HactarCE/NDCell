//! Operations on NdRects that are equivalent to applying the operation to the
//! minimum and maximum corners of the hyperrectangle.

use std::ops::*;

use super::*;

// Define addition, subtraction, multiplication, and division on NdRects for
// anything that can be added/subtracted to/from an NdVec.
impl<D: Dim, X> Add<X> for NdRect<D>
where
    NdVec<D>: Add<X, Output = NdVec<D>>,
{
    type Output = Self;
    fn add(self, operand: X) -> Self {
        Self {
            min: self.min + operand,
            max: self.max + operand,
        }
    }
}
impl<D: Dim, X> Sub<X> for NdRect<D>
where
    NdVec<D>: Sub<X, Output = NdVec<D>>,
{
    type Output = Self;
    fn sub(self, operand: X) -> Self {
        Self {
            min: self.min - operand,
            max: self.max - operand,
        }
    }
}
impl<D: Dim, X> Mul<X> for NdRect<D>
where
    NdVec<D>: Mul<X, Output = NdVec<D>>,
{
    type Output = Self;
    fn mul(self, operand: X) -> Self {
        // Call span() rather than constructing directly because if we multiply
        // by a negative number then the min and max might swap, so we need to
        // double-check the bounds.
        Self::span(self.min * operand, self.max * operand)
    }
}
impl<D: Dim, X> Div<X> for NdRect<D>
where
    NdVec<D>: Div<X, Output = NdVec<D>>,
{
    type Output = Self;
    fn div(self, operand: X) -> Self {
        Self::span(self.min / operand, self.max / operand)
    }
}
