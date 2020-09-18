use std::fmt;
use std::ops::*;

use ndcell_core::num::{r64, FixedPoint};

/// The zoom level of the 2D viewport, represented as a base-2 logarithm of the
/// width of each individual cell in pixels.
///
/// TODO: consider using R64 here instead of f64.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Zoom2D(f64);

impl Default for Zoom2D {
    fn default() -> Self {
        Self::from_factor(32.0)
    }
}

impl fmt::Display for Zoom2D {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0 > 0.0 {
            // Zoomed in
            write!(f, "1:{}", 2.0_f64.powf(self.0).round())
        } else if self.0 >= -8.0 {
            // Zoomed out a bit
            write!(f, "{}:1", 2.0_f64.powf(-self.0).round())
        } else {
            // Zoomed out a lot
            write!(f, "2^{}:1", -self.0.round())
        }
    }
}

// Zoom in by a given factor.
impl Mul<f64> for Zoom2D {
    type Output = Self;
    fn mul(self, factor: f64) -> Self {
        Self::from_power(self.0 + factor.log2())
    }
}
impl MulAssign<f64> for Zoom2D {
    fn mul_assign(&mut self, factor: f64) {
        self.0 += factor.log2();
    }
}

// Zoom out by a given factor.
impl Div<f64> for Zoom2D {
    type Output = Self;
    fn div(self, factor: f64) -> Self {
        Self::from_power(self.0 - factor.log2())
    }
}
impl DivAssign<f64> for Zoom2D {
    fn div_assign(&mut self, factor: f64) {
        self.0 -= factor.log2();
    }
}

// Compute the ratio between two zoom levels.
impl Div<Zoom2D> for Zoom2D {
    type Output = f64;
    fn div(self, other: Self) -> f64 {
        2.0_f64.powf(self.0 - other.0)
    }
}

impl Zoom2D {
    /// The lower limit on zoom level; i.e. the furthest the user can zoom out.
    ///
    /// In render.rs, the number of cells that fit on the screen is converted to
    /// an f64 at one point, which have an 11-bit exponent and so cannot
    /// represent values above around 2^1022. For this reason we don't allow
    /// zooming out past around 2^1000:1.
    const LOWER_LIMIT: f64 = -1000.0;
    /// The upper limit on zoom level; i.e. the furthest the user can zoom in.
    ///
    /// For zooming in, we do not allow zoom levels higher than 1:256, just
    /// because we have to set a limit somewhere.
    const UPPER_LIMIT: f64 = 8.0;

    /// Construct a Zoom2D with the given zoom power (e.g. -2.0 = 4:1 zoom).
    pub fn from_power(power: f64) -> Self {
        assert!(power.is_finite());
        Self(power)
    }
    /// Construct a Zoom2D with the given zoom factor (e.g. 0.25 = 4:1 zoom).
    /// The zoom factor must be greater than 0.
    pub fn from_factor(factor: f64) -> Self {
        assert!(factor.is_finite());
        Self(factor.log2())
    }

    /// Clamps the zoom level to the lower and upper limits. This is not
    /// automatically enforced by Zoom2D; it must be called manually.
    pub fn clamp(self) -> Self {
        if self.0 < Self::LOWER_LIMIT {
            Self(Self::LOWER_LIMIT)
        } else if self.0 > Self::UPPER_LIMIT {
            Self(Self::UPPER_LIMIT)
        } else {
            self
        }
    }
    /// Returns the zoom power (e.g. -2.0 = 4:1 zoom).
    pub fn power(self) -> f64 {
        self.0
    }
    /// Returns the zoom factor (e.g. 0.25 = 4:1 zoom).
    pub fn factor(self) -> f64 {
        2.0_f64.powf(self.power())
    }
    /// Returns the width of pixels per cell, which is equivalent to the zoom
    /// factor.
    pub fn pixels_per_cell(self) -> f64 {
        2.0_f64.powf(self.power())
    }
    /// Converts a number of cells to a number of pixels.
    pub fn cells_to_pixels<X: Div<FixedPoint>>(self, cells: X) -> X::Output {
        cells / FixedPoint::from(r64(-self.power())).exp_base2()
    }
    /// Returns the width of cells per pixel, which is equivalent to the
    /// reciprocal of the zoom factor.
    pub fn cells_per_pixel(self) -> f64 {
        2.0_f64.powf(-self.power())
    }
    /// Converts a number of pixels to a numer of cells.
    pub fn pixels_to_cells<X: Mul<FixedPoint>>(self, pixels: X) -> X::Output {
        pixels * FixedPoint::from(r64(-self.power())).exp_base2()
    }
    /// Returns the Zoom2D for the nearest power of 2.
    pub fn round(self) -> Self {
        Self(self.0.round())
    }
    /// Returns the Zoom2D for the nearest zoomed-out power of 2.
    pub fn floor(self) -> Self {
        Self(self.0.floor())
    }
    /// Returns the Zoom2D for the nearest zoomed-in power of 2.
    pub fn ceil(self) -> Self {
        Self(self.0.ceil())
    }
}
