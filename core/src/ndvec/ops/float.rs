//! Floaty operations for floating-point `NdVecs`.

use super::*;
use crate::num::{r64, Float, R64};

impl<D: Dim> FVec<D> {
    /// Round each component of this vector to the nearest integer.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn round(&self) -> Self {
        Self::from_fn(|ax| self[ax].round())
    }

    /// Round each component of this vector down.
    #[inline]
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn floor(&self) -> Self {
        Self::from_fn(|ax| self[ax].floor())
    }

    /// Round each component of this vector up.
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
}
