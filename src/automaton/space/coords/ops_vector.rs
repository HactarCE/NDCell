//! Operations between two sets of coordinates (and unary negation operator).

use std::ops::*;

use super::*;

// Implement negation of a set of coordinates (i.e. negate each coordinate).
impl<D: Dim> Neg for Coords<D> {
    type Output = Self;
    fn neg(mut self) -> Self {
        for ax in D::axes() {
            self.0.set(ax, -self.0.get(ax));
        }
        self
    }
}

// Implement elementwise addition between two sets of coordinates.
impl<D: Dim> Add<Self> for Coords<D> {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut ret = self;
        ret += other;
        ret
    }
}
impl<D: Dim> AddAssign<Self> for Coords<D> {
    fn add_assign(&mut self, other: Self) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) + other.0.get(ax));
        }
    }
}

// Implement elementwise subtraction between two sets of coordinates.
impl<D: Dim> Sub<Self> for Coords<D> {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let mut ret = self;
        ret -= other;
        ret
    }
}
impl<D: Dim> SubAssign<Self> for Coords<D> {
    fn sub_assign(&mut self, other: Self) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) - other.0.get(ax));
        }
    }
}
