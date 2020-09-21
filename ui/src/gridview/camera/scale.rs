use std::fmt;
use std::ops::*;

use ndcell_core::num::{r64, FixedPoint, Float, R64};

/// The scale factor.
///
/// In 2D, this is the width or height of a cell in pixels. In 3D, this is the
/// number of spatial "units" per cell, where the camera is always a fixed
/// number of "units" away from the pivot.
///
/// When zoomed in close, the scale factor is large, and the base-2 logarithm of
/// the scale factor is positive. When zoomed out far away, the scale factor is
/// small (between 0 and 1) and the base-2 logarithm of the scale factor is
/// negative.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Scale {
    log2_factor: R64,
}
impl Default for Scale {
    fn default() -> Self {
        // 32 pixels per cell
        Self::from_factor(r64(32.0))
    }
}
impl fmt::Display for Scale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.log2_factor > 0.0 {
            // Zoomed in
            write!(f, "{}:1", self.log2_factor.exp2().round())
        } else if self.log2_factor >= -8.0 {
            // Zoomed out a bit
            write!(f, "1:{}", (-self.log2_factor).exp2().round())
        } else {
            // Zoomed out a lot
            write!(f, "1:2^{}", -self.log2_factor.round())
        }
    }
}

impl Scale {
    /// The lower scale limit; i.e. the furthest the user can zoom out.
    ///
    /// `f64` has an 11-bit exponent and so cannott represent values above
    /// around 2^1022. For this reason we don't allow zooming out past around
    /// 2^1000:1. This restriction could probably be relaxed if anyone is
    /// actually trying to view patterns as large as 2^1000 cells across.
    const LOWER_LIMIT: f64 = -1000.0;
    /// The upper scale limit; i.e. the furthest the user can zoom in.
    ///
    /// For zooming in, we do not allow zoom scales larger than 1:256, just
    /// because we have to set a limit somewhere.
    const UPPER_LIMIT: f64 = 8.0;

    /// Creates a `Scale` from a scale factor's base-2 logarithm (e.g. `-3.0` = 1:8 scale).
    pub fn from_log2_factor(log2_factor: R64) -> Self {
        Self { log2_factor }
    }
    /// Creates a `Scale` from a scale factor (e.g. `0.25` = 1:4 scale).
    ///
    /// # Panics
    ///
    /// This function panics if `factor` is not greater than zero.
    pub fn from_factor(factor: R64) -> Self {
        Self {
            log2_factor: factor.log2(),
        }
    }

    /// Clamps the scale to the lower and upper limits. This is not
    /// automatically enforced by `Scale`; it must be called manually.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn clamp(self) -> Self {
        if self.log2_factor < Self::LOWER_LIMIT {
            Self {
                log2_factor: r64(Self::LOWER_LIMIT),
            }
        } else if self.log2_factor > Self::UPPER_LIMIT {
            Self {
                log2_factor: r64(Self::UPPER_LIMIT),
            }
        } else {
            self
        }
    }

    /// Returns the base-2 logarithm of the scale factor (e.g. -2.0 = 4:1 scale).
    pub fn log2_factor(self) -> R64 {
        self.log2_factor
    }
    /// Returns the scale factor (e.g. 0.25 = 4:1 scale).
    pub fn factor(self) -> FixedPoint {
        FixedPoint::from(self.log2_factor()).exp2()
    }

    /// Returns the length of pixels per cell, which is equivalent to the scale
    /// factor.
    ///
    /// # Panics
    ///
    /// This method panics if the result does not fit in an `f64`.
    pub fn pixels_per_cell(self) -> R64 {
        self.log2_factor().exp2()
    }
    /// Converts a length of cells to a length of pixels.
    pub fn cells_to_pixels<X: Div<FixedPoint>>(self, cells: X) -> X::Output {
        cells / FixedPoint::from(-self.log2_factor()).exp2()
    }

    /// Returns the length of cells per pixel, which is equivalent to the
    /// reciprocal of the scale factor.
    ///
    /// This value always fits in an `f64`, however it may be infinitessimally
    /// close to zero (and therefore round to zero).
    pub fn cells_per_pixel(self) -> R64 {
        (-self.log2_factor()).exp2()
    }
    /// Converts a length of pixels to a length of cells.
    pub fn pixels_to_cells<X: Mul<FixedPoint>>(self, pixels: X) -> X::Output {
        pixels * FixedPoint::from(-self.log2_factor()).exp2()
    }
    /// Rounds the scale factor to the nearest power of 2.
    pub fn round(self) -> Self {
        Self {
            log2_factor: self.log2_factor.round(),
        }
    }
    /// Rounds the scale factor down (zooms out) to the nearest power of 2.
    pub fn floor(self) -> Self {
        Self {
            log2_factor: self.log2_factor.floor(),
        }
    }
    /// Rounds the scale factor up (zooms in) to the nearest power of 2.
    pub fn ceil(self) -> Self {
        Self {
            log2_factor: self.log2_factor.ceil(),
        }
    }
}

impl<X: Float> Mul<X> for Scale
where
    R64: Add<X, Output = R64>,
{
    type Output = Self;

    /// Scales up / zooms in by a factor.
    fn mul(self, factor: X) -> Self {
        Self::from_log2_factor(self.log2_factor + factor.log2())
    }
}
impl<X: Float> MulAssign<X> for Scale
where
    R64: AddAssign<X>,
{
    /// Scales up / zooms in by a factor.
    fn mul_assign(&mut self, factor: X) {
        self.log2_factor += factor.log2();
    }
}

impl<X: Float> Div<X> for Scale
where
    R64: Sub<X, Output = R64>,
{
    type Output = Self;

    /// Scales down / zooms out by a factor.
    fn div(self, factor: X) -> Self {
        Self::from_log2_factor(self.log2_factor - factor.log2())
    }
}
impl<X: Float> DivAssign<X> for Scale
where
    R64: SubAssign<X>,
{
    /// Scales down / zooms out by a factor.
    fn div_assign(&mut self, factor: X) {
        self.log2_factor -= factor.log2();
    }
}

impl Div<Scale> for Scale {
    type Output = R64;

    /// Computes the ratio between two scales.
    ///
    /// # Panics
    ///
    /// This operation panics if the result does not fit in an `f64`.
    fn div(self, other: Self) -> R64 {
        (self.log2_factor - other.log2_factor).exp2()
    }
}
