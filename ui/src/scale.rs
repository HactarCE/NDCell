use std::fmt;
use std::ops::*;

use ndcell_core::prelude::*;

/// Scale factor.
///
/// In 2D, this is the width or height of a cell in "scaled units," which are
/// pixels in 2D and abstract units in 3D, where the camera is always a fixed
/// number of "units" away from the viewpoint pivot.
///
/// When zoomed in close, the scale factor is large, and the base-2 logarithm of
/// the scale factor is positive. When zoomed out far away, the scale factor is
/// small (between 0 and 1) and the base-2 logarithm of the scale factor is
/// negative.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Scale {
    log2_factor: R64,
}
impl Default for Scale {
    fn default() -> Self {
        // 32 pixels per cell is a decent default
        Self::from_factor(r64(32.0))
    }
}
impl fmt::Display for Scale {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    /// `f64` has an 11-bit exponent and so cannot represent values above around
    /// 2^1022. For this reason we don't allow zooming out past around 2^1000:1.
    /// This restriction could probably be relaxed if anyone is actually trying
    /// to view patterns as large as 2^1000 cells across.
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
    /// Returns the largest `Scale` at which a rectangle of cells fits entirely
    /// on the target.
    pub fn from_fit<D: Dim>(cells_size: BigVec<D>, target_size: (u32, u32)) -> Self {
        let log2_cells_size =
            FVec::<D>::from_fn(|ax| r64(FixedPoint::from(cells_size[ax].clone()).log2()));

        let (log2_cells_w, log2_cells_h) = if D::NDIM == 2 {
            // Unpack vector into two values.
            (log2_cells_size[Axis::X], log2_cells_size[Axis::Y])
        } else {
            // Clone the same value.
            let max = *log2_cells_size.max_component();
            (max, max)
        };

        let log2_target_w: R64 = R64::from_u32(target_size.0).unwrap().log2();
        let log2_target_h: R64 = R64::from_u32(target_size.1).unwrap().log2();

        // Divide `cells_size` by `target_size` to get the number of cells per
        // pixel (i.e. scale factor).
        let log2_scale_factor =
            R64::min(log2_target_w - log2_cells_w, log2_target_h - log2_cells_h);
        Self::from_log2_factor(log2_scale_factor)
    }

    /// Clamps the scale to the lower and upper limits. This is not
    /// automatically enforced by `Scale`; it must be called manually.
    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn clamp(self) -> Self {
        if self.log2_factor < Self::LOWER_LIMIT {
            Self::from_log2_factor(r64(Self::LOWER_LIMIT))
        } else if self.log2_factor > Self::UPPER_LIMIT {
            Self::from_log2_factor(r64(Self::UPPER_LIMIT))
        } else {
            self
        }
    }

    /// Returns the base-2 logarithm of the scale factor (e.g. -2.0 = 4:1 scale).
    pub fn log2_factor(self) -> R64 {
        self.log2_factor
    }
    /// Returns the inverse scale factor (e.g. 4.0 = 4:1 scale).
    pub fn inv_factor(self) -> FixedPoint {
        FixedPoint::from(-self.log2_factor()).exp2()
    }

    /// Returns the length of scaled units per cell, which is equivalent to the
    /// scale factor.
    ///
    /// # Panics
    ///
    /// This method panics if the result does not fit in an `f64`.
    pub fn units_per_cell(self) -> R64 {
        self.log2_factor().exp2()
    }
    /// Converts a length of cells to a length of scaled units.
    pub fn cells_to_units<X: Div<FixedPoint>>(self, cells: X) -> X::Output {
        cells / self.inv_factor()
    }

    /// Returns the length of cells per scaled unit, which is equivalent to the
    /// reciprocal of the scale factor.
    ///
    /// This value always fits in an `f64`, however it may be infinitessimally
    /// close to zero (and therefore round to zero).
    pub fn cells_per_unit(self) -> R64 {
        (-self.log2_factor()).exp2()
    }
    /// Converts a length of scaled units to a length of cells.
    pub fn units_to_cells<X: Mul<FixedPoint>>(self, scaled_units: X) -> X::Output {
        scaled_units * FixedPoint::from(-self.log2_factor()).exp2()
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
