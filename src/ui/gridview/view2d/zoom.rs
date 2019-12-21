use std::fmt;
use std::ops::*;

/// The zoom level of the 2D viewport, represented as a base-2 logarithm of the
/// width of each individual cell in pixels.
#[derive(Debug, Copy, Clone)]
pub struct Zoom2D(f32);

impl Default for Zoom2D {
    fn default() -> Self {
        Self::from_factor(32.0)
    }
}

impl fmt::Display for Zoom2D {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0 > 0.0 {
            // Zoomed in
            write!(f, "1:{}", 2.0f32.powf(self.0).round())
        } else if self.0 >= -8.0 {
            // Zoomed out a bit
            write!(f, "{}:1", 2.0f32.powf(-self.0).round())
        } else {
            // Zoomed out a lot
            write!(f, "2^{}:1", -self.0.round())
        }
    }
}

// Zoom in by a given factor.
impl Mul<f32> for Zoom2D {
    type Output = Self;
    fn mul(self, factor: f32) -> Self {
        Self(self.0 + factor.log2())
    }
}
impl MulAssign<f32> for Zoom2D {
    fn mul_assign(&mut self, factor: f32) {
        self.0 += factor.log2();
    }
}

// Zoom out by a given factor.
impl Div<f32> for Zoom2D {
    type Output = Self;
    fn div(self, factor: f32) -> Self {
        Self(self.0 - factor.log2())
    }
}
impl DivAssign<f32> for Zoom2D {
    fn div_assign(&mut self, factor: f32) {
        self.0 -= factor.log2();
    }
}

// Compute the ratio between two zoom levels.
impl Div<Zoom2D> for Zoom2D {
    type Output = f32;
    fn div(self, other: Self) -> f32 {
        2.0f32.powf(self.0 - other.0)
    }
}

impl Zoom2D {
    /// The lower limit on zoom level; i.e. the furthest the user can zoom out.
    ///
    /// The mantissa of a 32-bit float is 24 bits, so we don't allow zooming
    /// out past 2^(2^20):1. This leaves ~4 fractional bits to represent zoom
    /// levels that are not perfect powers of 2.
    const LOWER_LIMIT: f32 = -(1 << 20) as f32;
    /// The upper limit on zoom level; i.e. the furthest the user can zoom in.
    ///
    /// For zooming in, we do not allow zoom levels higher than 1:256, just
    /// because we have to set a limit somewhere.
    const UPPER_LIMIT: f32 = 8.0;

    /// Construct a Zoom2D with the given zoom power (e.g. -2.0 = 4:1 zoom).
    pub fn from_power(power: f32) -> Self {
        Self(power).clamp()
    }
    /// Construct a Zoom2D with the given zoom factor (e.g. 0.25 = 4:1 zoom).
    /// The zoom factor must be greater than 0.
    pub fn from_factor(factor: f32) -> Self {
        Self(factor.log2()).clamp()
    }

    /// Clamps the zoom level to the lower and upper limits. This is called
    /// whenever a Zoom2D is constructed.
    fn clamp(self) -> Self {
        if self.0 < Self::LOWER_LIMIT {
            Self(Self::LOWER_LIMIT)
        } else if self.0 > Self::UPPER_LIMIT {
            Self(Self::UPPER_LIMIT)
        } else {
            self
        }
    }
    /// Returns the zoom power (e.g. -2.0 = 4:1 zoom).
    pub fn power(self) -> f32 {
        self.0
    }
    /// Returns the zoom factor (e.g. 0.25 = 4:1 zoom).
    pub fn factor(self) -> f32 {
        2.0f32.powf(self.power())
    }
    /// Returns the width of pixels per cell, which is equivalent to the zoom
    /// factor.
    pub fn pixels_per_cell(self) -> f32 {
        2.0f32.powf(self.power())
    }
    /// Returns the width of cells per pixel, which is equivalent to the
    /// reciprocal of the zoom factor.
    pub fn cells_per_pixel(self) -> f32 {
        2.0f32.powf(-self.power())
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
