//! Floaty operations for floating-point `NdVecs`.

use super::*;
use crate::num::{r64, Float, R64};

impl<D: Dim> FixedVec<D> {
    /// Returns the integer part of each component of the vector.
    #[inline]
    pub fn trunc(&self) -> BigVec<D> {
        NdVec::from_fn(|ax| self[ax].trunc())
    }

    /// Returns the fractional part of each component of the vector.
    #[inline]
    pub fn fract(&self) -> FVec<D> {
        NdVec::from_fn(|ax| r64(self[ax].fract()))
    }

    /// Rounds each component of this vector to the nearest integer.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn round(&self) -> BigVec<D> {
        BigVec::from_fn(|ax| self[ax].round())
    }

    /// Returns the largest integer less than or equal to each component of the
    /// vector.
    #[inline]
    pub fn floor(&self) -> BigVec<D> {
        NdVec::from_fn(|ax| self[ax].floor())
    }

    /// Returns the smallest integer greater than or equal to each component of
    /// the vector.
    #[inline]
    pub fn ceil(&self) -> BigVec<D> {
        NdVec::from_fn(|ax| self[ax].ceil())
    }

    /// Returns the magnitude of the vector.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn mag(&self) -> FixedPoint {
        (self * self).sum().sqrt()
    }
}

impl<D: Dim> FVec<D> {
    /// Rounds each component of this vector to the nearest integer.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn round(&self) -> Self {
        Self::from_fn(|ax| self[ax].round())
    }

    /// Rounds each component of this vector down.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn floor(&self) -> Self {
        Self::from_fn(|ax| self[ax].floor())
    }

    /// Rounds each component of this vector up.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn ceil(&self) -> Self {
        Self::from_fn(|ax| self[ax].ceil())
    }

    /// Raises each component of this vector to the given power.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn powf(&self, power: R64) -> Self {
        Self::from_fn(|ax| self[ax].powf(power))
    }

    /// Returns the magnitude of the vector.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn mag(&self) -> R64 {
        self.powf(r64(2.0)).sum().powf(r64(0.5))
    }

    /// Returns the normalized vector.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn normalized(&self) -> Self {
        self / self.mag()
    }
}

impl<D: DimFor<R64>> Add<R64> for FVec<D> {
    type Output = Self;

    #[inline]
    fn add(self, other: R64) -> Self {
        NdVec::from_fn(|ax| self[ax] + other)
    }
}
impl<'a, D: DimFor<R64>> Add<&'a R64> for FVec<D> {
    type Output = Self;

    #[inline]
    fn add(self, other: &'a R64) -> Self {
        NdVec::from_fn(|ax| self[ax] + other)
    }
}
impl<D: DimFor<R64>> AddAssign<R64> for FVec<D> {
    #[inline]
    fn add_assign(&mut self, other: R64) {
        for &ax in D::Dim::axes() {
            self[ax] += other;
        }
    }
}
impl<'a, D: DimFor<R64>> AddAssign<&'a R64> for FVec<D> {
    #[inline]
    fn add_assign(&mut self, other: &'a R64) {
        for &ax in D::Dim::axes() {
            self[ax] += other;
        }
    }
}

impl<D: DimFor<R64>> Sub<R64> for FVec<D> {
    type Output = Self;

    #[inline]
    fn sub(self, other: R64) -> Self {
        NdVec::from_fn(|ax| self[ax] - other)
    }
}
impl<'a, D: DimFor<R64>> Sub<&'a R64> for FVec<D> {
    type Output = Self;

    #[inline]
    fn sub(self, other: &'a R64) -> Self {
        NdVec::from_fn(|ax| self[ax] - other)
    }
}
impl<D: DimFor<R64>> SubAssign<R64> for FVec<D> {
    #[inline]
    fn sub_assign(&mut self, other: R64) {
        for &ax in D::Dim::axes() {
            self[ax] -= other;
        }
    }
}
impl<'a, D: DimFor<R64>> SubAssign<&'a R64> for FVec<D> {
    #[inline]
    fn sub_assign(&mut self, other: &'a R64) {
        for &ax in D::Dim::axes() {
            self[ax] -= other;
        }
    }
}

impl<D: DimFor<R64>> Mul<R64> for FVec<D> {
    type Output = Self;

    #[inline]
    fn mul(self, other: R64) -> Self {
        NdVec::from_fn(|ax| self[ax] * other)
    }
}
impl<'a, D: DimFor<R64>> Mul<&'a R64> for FVec<D> {
    type Output = Self;

    #[inline]
    fn mul(self, other: &'a R64) -> Self {
        NdVec::from_fn(|ax| self[ax] * other)
    }
}
impl<D: DimFor<R64>> MulAssign<R64> for FVec<D> {
    #[inline]
    fn mul_assign(&mut self, other: R64) {
        for &ax in D::Dim::axes() {
            self[ax] *= other;
        }
    }
}
impl<'a, D: DimFor<R64>> MulAssign<&'a R64> for FVec<D> {
    #[inline]
    fn mul_assign(&mut self, other: &'a R64) {
        for &ax in D::Dim::axes() {
            self[ax] *= other;
        }
    }
}

impl<D: DimFor<R64>> Add<FVec<D>> for FVec<D> {
    type Output = Self;

    #[inline]
    fn add(self, other: FVec<D>) -> Self {
        NdVec::from_fn(|ax| self[ax] + other[ax])
    }
}
impl<'a, D: DimFor<R64>> Add<&'a FVec<D>> for FVec<D> {
    type Output = Self;

    #[inline]
    fn add(self, other: &'a FVec<D>) -> Self {
        NdVec::from_fn(|ax| self[ax] + other[ax])
    }
}
impl<D: DimFor<R64>> AddAssign<FVec<D>> for FVec<D> {
    #[inline]
    fn add_assign(&mut self, other: FVec<D>) {
        for &ax in D::Dim::axes() {
            self[ax] += other[ax];
        }
    }
}
impl<'a, D: DimFor<R64>> AddAssign<&'a FVec<D>> for FVec<D> {
    #[inline]
    fn add_assign(&mut self, other: &'a FVec<D>) {
        for &ax in D::Dim::axes() {
            self[ax] += other[ax];
        }
    }
}

impl<D: DimFor<R64>> Sub<FVec<D>> for FVec<D> {
    type Output = Self;

    #[inline]
    fn sub(self, other: FVec<D>) -> Self {
        NdVec::from_fn(|ax| self[ax] - other[ax])
    }
}
impl<'a, D: DimFor<R64>> Sub<&'a FVec<D>> for FVec<D> {
    type Output = Self;

    #[inline]
    fn sub(self, other: &'a FVec<D>) -> Self {
        NdVec::from_fn(|ax| self[ax] - other[ax])
    }
}
impl<D: DimFor<R64>> SubAssign<FVec<D>> for FVec<D> {
    #[inline]
    fn sub_assign(&mut self, other: FVec<D>) {
        for &ax in D::Dim::axes() {
            self[ax] -= other[ax];
        }
    }
}
impl<'a, D: DimFor<R64>> SubAssign<&'a FVec<D>> for FVec<D> {
    #[inline]
    fn sub_assign(&mut self, other: &'a FVec<D>) {
        for &ax in D::Dim::axes() {
            self[ax] -= other[ax];
        }
    }
}

impl<D: DimFor<R64>> Mul<FVec<D>> for FVec<D> {
    type Output = Self;

    #[inline]
    fn mul(self, other: FVec<D>) -> Self {
        NdVec::from_fn(|ax| self[ax] * other[ax])
    }
}
impl<'a, D: DimFor<R64>> Mul<&'a FVec<D>> for FVec<D> {
    type Output = Self;

    #[inline]
    fn mul(self, other: &'a FVec<D>) -> Self {
        NdVec::from_fn(|ax| self[ax] * other[ax])
    }
}
impl<D: DimFor<R64>> MulAssign<FVec<D>> for FVec<D> {
    #[inline]
    fn mul_assign(&mut self, other: FVec<D>) {
        for &ax in D::Dim::axes() {
            self[ax] *= other[ax];
        }
    }
}
impl<'a, D: DimFor<R64>> MulAssign<&'a FVec<D>> for FVec<D> {
    #[inline]
    fn mul_assign(&mut self, other: &'a FVec<D>) {
        for &ax in D::Dim::axes() {
            self[ax] *= other[ax];
        }
    }
}
