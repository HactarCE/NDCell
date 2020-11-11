//! Conversions between different types of `NdRect`s.

use super::{BigRect, FixedRect, IRect, URect};
use crate::dim::Dim;

impl<D: Dim> BigRect<D> {
    /// Converts this `BigRect` to a `FixedVec`.
    #[inline]
    pub fn to_fixedrect(&self) -> FixedRect<D> {
        FixedRect::with_size(self.start.to_fixedvec(), self.size.to_fixedvec())
    }

    /// Converts this `BigRect` to an `IRect`.
    ///
    /// # Panics
    ///
    /// This method panics if any component of the minimum/maximum/size does not
    /// fit in `isize`.
    #[inline]
    pub fn to_irect(&self) -> IRect<D> {
        // Panic if the maximum doesn't fit inside `isize`.
        self.max().to_ivec();
        IRect::with_size(self.start.to_ivec(), self.size.to_ivec())
    }

    /// Converts this `BigRect` to a `URect`.
    ///
    /// # Panics
    ///
    /// This method panics if any component of the minimum/maximum/size does not
    /// fit in `usize`.
    #[inline]
    pub fn to_urect(&self) -> URect<D> {
        // Panic if the maximum doesn't fit inside `usize`.
        self.max().to_uvec();
        URect::with_size(self.start.to_uvec(), self.size.to_uvec())
    }
}

impl<D: Dim> IRect<D> {
    /// Converts this `IRect` to a `BigRect`.
    #[inline]
    pub fn to_bigrect(&self) -> BigRect<D> {
        BigRect::with_size(self.start.to_bigvec(), self.size.to_bigvec())
    }
}

impl<D: Dim> URect<D> {
    /// Converts this `URect` to a `BigRect`.
    #[inline]
    pub fn to_bigrect(&self) -> BigRect<D> {
        BigRect::with_size(self.start.to_bigvec(), self.size.to_bigvec())
    }
}
