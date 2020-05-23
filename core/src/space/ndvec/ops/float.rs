//! Floaty operations for floating-point NdVecs.

use noisy_float::prelude::{r64, R64};
use num::Float;

use super::*;

impl<D: Dim> FVec<D> {
    /// Round each component of this vector to the nearest integer.
    #[must_use]
    pub fn round(&self) -> Self {
        Self::from_fn(|ax| self[ax].round())
    }
    /// Round each component of this vector down.
    #[must_use]
    pub fn floor(&self) -> Self {
        Self::from_fn(|ax| self[ax].floor())
    }
    /// Round each component of this vector up.
    #[must_use]
    pub fn ceil(&self) -> Self {
        Self::from_fn(|ax| self[ax].ceil())
    }
    /// Raises each component of this vector to the given power.
    #[must_use]
    pub fn powf(&self, power: R64) -> Self {
        Self::from_fn(|ax| self[ax].powf(power))
    }
    /// Returns the magnitude of the vector.
    #[must_use]
    pub fn mag(&self) -> R64 {
        self.powf(r64(2.0)).sum().powf(r64(0.5))
    }
}
