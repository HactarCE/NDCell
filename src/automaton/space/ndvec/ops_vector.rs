//! Operations between two NdVecs (and unary negation operator).

use std::ops::*;

use super::*;

// Implement negation of an NdVec (i.e. negate each coordinate).
impl<D: Dim> Neg for NdVec<D> {
    type Output = Self;
    fn neg(mut self) -> Self {
        for &ax in D::axes() {
            self[ax] *= -1
        }
        self
    }
}

// Implement elementwise addition between two NdVecs.
impl<D: Dim> Add<Self> for NdVec<D> {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut ret = self;
        ret += other;
        ret
    }
}
impl<D: Dim> AddAssign<Self> for NdVec<D> {
    fn add_assign(&mut self, other: Self) {
        for &ax in D::axes() {
            self[ax] += other[ax];
        }
    }
}

// Implement elementwise subtraction between two NdVecs.
impl<D: Dim> Sub<Self> for NdVec<D> {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let mut ret = self;
        ret -= other;
        ret
    }
}
impl<D: Dim> SubAssign<Self> for NdVec<D> {
    fn sub_assign(&mut self, other: Self) {
        for &ax in D::axes() {
            self[ax] -= other[ax];
        }
    }
}

// Implement elementwise multiplication between two NdVecs.
impl<D: Dim> Mul<Self> for NdVec<D> {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        let mut ret = self;
        ret *= other;
        ret
    }
}
impl<D: Dim> MulAssign<Self> for NdVec<D> {
    fn mul_assign(&mut self, other: Self) {
        for &ax in D::axes() {
            self[ax] *= other[ax];
        }
    }
}
