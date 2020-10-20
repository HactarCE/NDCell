use anyhow::Result;

use ndcell_core::prelude::*;

mod camera2d;
mod camera3d;
mod interpolator;
mod transform;

use crate::commands::ViewCommand;
use crate::config::Config;
use crate::gridview::DragHandler;
use crate::Scale;
pub use camera2d::Camera2D;
pub use camera3d::Camera3D;
pub use interpolator::{Interpolate, Interpolator};
pub use transform::{CellTransform, CellTransform2D, CellTransform3D, NdCellTransform};

/// Minimum target width & height, to avoid a divide-by-zero error.
const MIN_TARGET_SIZE: u32 = 10;

/// Number of pixels to pan that feels equivalent to scaling by a factor of 2.
///
/// Pixels are a very small unit compared to logarithmic scale factor, and
/// panning 400 pixels feels about equivalent to scaling by a factor of 2 to me.
///
/// Obviously this depends on DPI and/or window size, but deriving an absolute
/// formula for it is a nightmare of calculus. All that matters is it's vaguely
/// proportional to the size of the window, so at some point in the future this
/// could be changed to something like sqrt(h²+w²) / 5. Here's a Desmos link if
/// you're curious: https://www.desmos.com/calculator/1yxv7mglnj.
const PIXELS_PER_2X_SCALE: f64 = 400.0;

/// Number of degrees to rotate that feels equivalent to scaling by a factor of
/// 2.
///
/// This one is completely arbitrary.
const ROT_DEGREES_PER_2X_SCALE: f64 = PIXELS_PER_2X_SCALE;

/// Common functionality for 2D and 3D cameras.
pub trait Camera<D: Dim>: 'static + std::fmt::Debug + Default + Clone + PartialEq {
    /// Returns the width and height of the viewport.
    fn target_dimensions(&self) -> (u32, u32);
    /// Sets the width and height of the viewport.
    fn set_target_dimensions(&mut self, target_dimensions: (u32, u32));
    /// Returns the display scaling factor, which does not affect rendering of
    /// cells but may affect other UI elements.
    fn dpi(&self) -> f32;
    /// Sets the display scaling factor.
    fn set_dpi(&mut self, dpi: f32);

    /// Returns the position the camera is looking at/from.
    fn pos(&self) -> &FixedVec<D>;
    /// Sets the position the camera is looking at/from.
    fn set_pos(&mut self, pos: FixedVec<D>);
    /// Snap to the nearest integer cell position.
    fn snap_pos(&mut self, config: &Config);

    /// Returns the scale of the camera.
    fn scale(&self) -> Scale;
    /// Sets the scale of the camera.
    fn set_scale(&mut self, scale: Scale);

    /// Sets the scale of the camera, keeping one point at the same location in
    /// screen space.
    ///
    /// If `invariant_pos` is `None`, then the value returned by `pos()` is
    /// invariant.
    fn scale_to(&mut self, scale: Scale, invariant_pos: Option<FixedVec<D>>) {
        // Scale, keeping the center position invariant.
        let old_scale = self.scale();
        self.set_scale(scale);
        let new_scale = self.scale(); // `.clamp()`ed in `set_scale()`

        // Compute cell offset of `invariant_pos` from the camera position.
        let invariant_pos_offset = invariant_pos
            .map(|invar_pos| invar_pos - self.pos())
            .unwrap_or_default();

        // Compute the old scaled offset of `invariant_pos` from the camera
        // position.
        let invariant_pos_old_scaled_offset = old_scale.cells_to_units(&invariant_pos_offset);
        // Compute the new scaled offset of `invariant_pos` from the camera
        // position.
        let invariant_pos_new_scaled_offset = new_scale.cells_to_units(&invariant_pos_offset);

        // Compute the difference between those scaled offsets.
        let delta_pixel_offset = invariant_pos_new_scaled_offset - invariant_pos_old_scaled_offset;

        // Apply that offset so that the point goes back to the same scaled
        // location as before.
        self.set_pos(self.pos() + new_scale.units_to_cells(delta_pixel_offset));
    }
    /// Scales by 2^`log2_factor`, keeping one invariant point at the same
    /// location in screen space.
    ///
    /// If `invariant_pos` is `None`, then the value returned by `pos()` is
    /// invariant.
    fn scale_by_log2_factor(&mut self, log2_factor: R64, invariant_pos: Option<FixedVec<D>>) {
        self.scale_to(
            Scale::from_log2_factor(self.scale().log2_factor() + log2_factor),
            invariant_pos,
        );
    }
    /// Scales by the given factor, keeping one invariant point at the same
    /// location in screen space.
    ///
    /// If `invariant_pos` is `None`, then the value returned by `pos()` is
    /// invariant.
    fn scale_by_factor(&mut self, factor: R64, invariant_pos: Option<FixedVec<D>>) {
        assert!(
            factor > 0.0,
            "Scale factor must be a positive number, not {}",
            factor,
        );
        self.scale_by_log2_factor(factor.log2(), invariant_pos)
    }
    /// Snaps to the nearest power-of-2 scale factor, keeping one invariant
    /// point at the same location in screen space.
    ///
    /// If `invariant_pos` is `None`, then the value returned by `pos()` is
    /// invariant.
    fn snap_scale(&mut self, invariant_pos: Option<FixedVec<D>>) {
        self.scale_by_factor(self.scale().round() / self.scale(), invariant_pos);
        self.set_scale(self.scale().round()); // Fix any potential rounding error.
    }

    /// Returns the abstract "distance" between two cameras.
    fn distance(a: &Self, b: &Self) -> FixedPoint {
        let avg_scale = average_lerped_scale(a.scale(), b.scale());
        let total_cells_delta = (b.pos() - a.pos()).mag();
        let total_pixels_delta = total_cells_delta * avg_scale.factor();
        // Divide by a constant factor to bring translation and scale into the
        // same arbitrary units of optical flow.
        let panning_distance = total_pixels_delta / FixedPoint::from(r64(PIXELS_PER_2X_SCALE));
        let scale_distance = a.scale().log2_factor() - b.scale().log2_factor();
        let extra_distance = Self::_extra_distance_dimension(a, b);
        // Use euclidean distance.
        let squared_panning_distance = &panning_distance * &panning_distance;
        let squared_scale_distance: FixedPoint = (scale_distance * scale_distance).into();
        let squared_extra_distance = &extra_distance * &extra_distance;
        let squared_distance: FixedPoint =
            squared_panning_distance + squared_scale_distance + squared_extra_distance;
        squared_distance.sqrt()
    }
    fn _extra_distance_dimension(_a: &Self, _b: &Self) -> FixedPoint {
        FixedPoint::zero()
    }

    /// Returns a camera that is some fraction 0.0 <= t <= 1.0 of the distance
    /// between two cameras using linear interpolation that preserves the fixed
    /// point of the transformation from one camera to the other.
    ///
    /// This function attempts to interpolate linearly with respect to the
    /// apparent motion experienced by the user, so it linearly interpolates 2D
    /// panning speed in terms of on-screen pixels rather than cells, and
    /// interpolates scale factor logarithmically.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn lerp(a: &Self, b: &Self, t: R64) -> Self;

    /// Returns the layer and scale factor of render cells.
    fn render_cell_layer_and_scale(&self) -> (Layer, Scale) {
        let layer = Layer((-self.scale().round().log2_factor()).to_u32().unwrap_or(0));

        // Multiply the camera scale factor by the size of a render cell, except
        // it's addition because we're actually using logarithms.
        let scale = Scale::from_log2_factor(self.scale().log2_factor() + layer.to_u32() as f64);
        // The render cell scale factor should be greater than 1/2 (since 1/2 or
        // anything lower should be handled by having a higher render cell
        // layer). log₂(1/2) = -1.
        assert!(scale.log2_factor() > -1.0);

        (layer, scale)
    }
    /// Returns the cell transform for this camera with the given render target
    /// size and a base position near the camera center.
    fn cell_transform(&self) -> NdCellTransform<D> {
        self.cell_transform_with_base(self.pos().round())
            .expect("Camera::cell_transform() failed")
    }
    /// Returns the cell transform for this camera with the given render target
    /// size and base position.
    fn cell_transform_with_base(&self, base_cell_pos: BigVec<D>) -> Result<NdCellTransform<D>>;

    /// Executes a movement command.
    fn do_move_command(
        &mut self,
        command: ViewCommand,
        config: &Config,
    ) -> Result<Option<DragHandler<Self>>>;
}

/// Returns the "average" scale between the two cameras, averaging scale
/// factor linearly with respect to time during a linear interpolation,
/// where scale factor is interpolated logarithmically.
///
/// Read source comments to see how this is relevant.
fn average_lerped_scale(s1: Scale, s2: Scale) -> Scale {
    let s1 = s1.log2_factor();
    let s2 = s2.log2_factor();
    // Read the comments in the first half of `Camera2D::lerp()` before
    // proceeding.
    //
    // We want to find the total number of pixels to travel. The logarithm of
    // the scale factor is a linear function of time s(t) = s₁ + (s₂ - s₁) * t
    // for 0 <= t <= 1, where s₁ and s₂ are the logarithms of the inital and
    // final scales respectively. The number of pixels to travel is a constant
    // value for that range as well. The number of cells per pixel is
    // 1/(2^s(t)), so the total number of cells to travel is the integral of
    // pixels/(2^s(t)) dt from t=0 to t=1. That integral comes out to the
    // following:
    //
    //           pixels * ( 2^(-s₁) - 2^(-s₂) )
    // cells = - ------------------------------
    //                 ln(2) * (s₁ - s₂)
    //
    // (Note the negative sign in front.) We know how many cells to travel;
    // that's just b.pos - a.pos. We could solve the above equation for the
    // number of pixels, but instead let's solve it for the ratio of pixels to
    // cells:
    //
    // pixels     ln(2) * (s₁ - s₂)
    // ------ = - -----------------
    // cells      2^(-s₁) - 2^(-s₂)
    //
    // This gives us a sort of "average scale factor." But scale factors get
    // very small when zoomed out, so the reciprocal will be more precise:
    //
    //         1             2^(-s₁) - 2^(-s₂)
    // ----------------- = - -----------------
    // avg. scale factor     ln(2) * (s₁ - s₂)

    // Use fixed-point numbers for better precision.
    let s1_fixp = FixedPoint::from(s1);
    let s2_fixp = FixedPoint::from(s2);

    let numerator = (-&s1_fixp).exp2() - (-s2_fixp).exp2();
    let denominator = FixedPoint::from(r64(2.0).ln() * (s1 - s2));
    let recip_avg_scale_factor = if numerator.is_zero() || denominator.is_zero() {
        // The expression is undefined at s₁ = s₂, but the limit is 2^(-s).
        (-s1_fixp).exp2()
    } else {
        -numerator / denominator
    };

    // Take the base-2 logarithm and then negate to get the base-2 logarithm of
    // the scale factor.
    let avg_log2_scale_factor = r64(-recip_avg_scale_factor.log2());
    Scale::from_log2_factor(avg_log2_scale_factor)
}
