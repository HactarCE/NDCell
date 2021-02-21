use anyhow::Result;

use ndcell_core::prelude::*;

mod interpolator;
mod transform;
mod viewpoint2d;
mod viewpoint3d;

use crate::commands::{Move2D, Move3D};
use crate::gridview::drag::DragOutcome;
use crate::{Scale, CONFIG};
pub use interpolator::Interpolator;
pub use transform::{
    CellTransform, CellTransform2D, CellTransform3D, NdCellTransform, ProjectionType,
};
pub use viewpoint2d::Viewpoint2D;
pub use viewpoint3d::Viewpoint3D;

/// Drag update function that modifies a viewpoint or interpolator.
type DragUpdateViewFn<V> = Box<dyn FnMut(&mut V, FVec2D) -> Result<DragOutcome>>;

/// Minimum target width & height, to avoid divide-by-zero errors.
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
const ROT_DEGREES_PER_2X_SCALE: f64 = 100.0;

/// Radius of visible cells in 3D, measured in "scaled units". (See `Scale`
/// docs.)
const VIEW_RADIUS_3D: f32 = 5000.0;

/// Common functionality for 2D and 3D viewpoints.
pub trait Viewpoint<D: Dim>: 'static + std::fmt::Debug + Default + Clone + PartialEq {
    /// Returns the width and height of the render target.
    fn target_dimensions(&self) -> (u32, u32);
    /// Sets the width and height of the render target.
    fn set_target_dimensions(&mut self, target_dimensions: (u32, u32));
    /// Returns the display scaling factor, which does not affect rendering of
    /// cells but may affect other UI elements.
    fn dpi(&self) -> f32;
    /// Sets the display scaling factor.
    fn set_dpi(&mut self, dpi: f32);

    /// Returns the position the camera is looking at/from.
    fn center(&self) -> &FixedVec<D>;
    /// Sets the position the camera is looking at/from.
    fn set_center(&mut self, pos: FixedVec<D>);
    /// Snap to the nearest integer cell position.
    fn snap_center(&mut self);

    /// Returns the visual scale of cells.
    fn scale(&self) -> Scale;
    /// Sets the visual scale of cells.
    fn set_scale(&mut self, scale: Scale);

    /// Sets the visual scale of cells, keeping one point at the same location
    /// on the screen.
    ///
    /// If `invariant_pos` is `None`, then the value returned by `center()` is
    /// used instead.
    fn scale_to(&mut self, scale: Scale, invariant_pos: Option<FixedVec<D>>) {
        // Scale, keeping the center position invariant.
        let old_scale = self.scale();
        self.set_scale(scale);
        let new_scale = self.scale(); // `.clamp()`ed in `set_scale()`

        // Compute cell offset of `invariant_pos` from the center.
        let invariant_pos_offset = invariant_pos
            .map(|invar_pos| invar_pos - self.center())
            .unwrap_or_default();

        // Compute the old scaled offset of `invariant_pos` from the center.
        let invariant_pos_old_scaled_offset = old_scale.cells_to_units(&invariant_pos_offset);
        // Compute the new scaled offset of `invariant_pos` from the center.
        let invariant_pos_new_scaled_offset = new_scale.cells_to_units(&invariant_pos_offset);

        // Compute the difference between those scaled offsets.
        let delta_pixel_offset = invariant_pos_new_scaled_offset - invariant_pos_old_scaled_offset;

        // Apply that offset so that the point goes back to the same scaled
        // location as before.
        self.set_center(self.center() + new_scale.units_to_cells(delta_pixel_offset));
    }
    /// Scales by 2^`log2_factor`, keeping one invariant point at the same
    /// location on the screen.
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
    /// location on the screen.
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
    /// point at the same location on the screen.
    ///
    /// If `invariant_pos` is `None`, then the value returned by `pos()` is
    /// invariant.
    fn snap_scale(&mut self, invariant_pos: Option<FixedVec<D>>) {
        self.scale_by_factor(self.scale().round() / self.scale(), invariant_pos);
        self.set_scale(self.scale().round()); // Fix any potential rounding error.
    }

    /// Returns the abstract "distance" between two viewpoints.
    fn distance(a: &Self, b: &Self) -> FixedPoint {
        let avg_scale = average_lerped_scale(a.scale(), b.scale());
        let total_cells_delta = (b.center() - a.center()).mag();
        let total_pixels_delta = total_cells_delta / avg_scale.inv_factor();
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

    /// Returns a viewpoint that is some fraction 0.0 <= t <= 1.0 of the
    /// distance between two viewpoints using linear interpolation that
    /// preserves the fixed point of the transformation from one viewpoint to
    /// the other.
    ///
    /// This function attempts to interpolate linearly with respect to the
    /// apparent motion experienced by the user, so it linearly interpolates 2D
    /// panning speed in terms of on-screen pixels rather than cells, and
    /// interpolates scale factor logarithmically.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn lerp(a: &Self, b: &Self, t: R64) -> Self;

    /// Returns the layer of render cells.
    fn render_cell_layer(&self) -> Layer {
        Layer((-self.scale().round().log2_factor()).to_u32().unwrap_or(0))
    }
    /// Returns the layer and scale factor of render cells.
    fn render_cell_layer_and_scale(&self) -> (Layer, Scale) {
        let layer = self.render_cell_layer();

        // Multiply the cell scale factor by the size of a render cell, except
        // it's addition because we're actually using logarithms.
        let scale = Scale::from_log2_factor(self.scale().log2_factor() + layer.to_u32() as f64);
        // The render cell scale factor should be greater than 1/2 (since 1/2 or
        // anything lower should be handled by having a higher render cell
        // layer). log₂(1/2) = -1.
        assert!(scale.log2_factor() > -1.0);

        (layer, scale)
    }
    /// Returns the cell transform for this viewpoint.
    fn cell_transform(&self) -> NdCellTransform<D>;

    /// Returns a rectangle of cells that are at least partially visible,
    /// rounded outward to the nearest render cell.
    fn global_visible_rect(&self) -> BigRect<D>;

    /// Returns a drag update function for `DragViewCmd::Orbit3D`.
    fn drag_orbit_3d(&self, cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>>;
    /// Returns a drag update function for `DragViewCmd::Pan`.
    fn drag_pan(&self, cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>>;
    /// Returns a drag update function for `DragViewCmd::PanHorizontal3D`.
    fn drag_pan_horizontal_3d(&self, cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>>;
    /// Returns a drag update function for `DragViewCmd::Scale`.
    fn drag_scale(&self, cursor_start: FVec2D) -> Option<DragUpdateViewFn<Self>> {
        let initial_scale = self.scale();
        Some(Box::new(move |this, cursor_end| {
            let delta =
                (cursor_end - cursor_start)[Axis::Y] / -CONFIG.lock().ctrl.pixels_per_2x_scale_2d;
            this.set_scale(Scale::from_log2_factor(initial_scale.log2_factor() + delta));
            Ok(DragOutcome::Continue)
        }))
    }

    /// Moves the viewpoint in 2D.
    fn apply_move_2d(&mut self, movement: Move2D);
    /// Moves the viewpoint in 3D.
    fn apply_move_3d(&mut self, movement: Move3D);
}

/// Returns the "average" scale between the two viewpoints, averaging scale
/// factor linearly with respect to time during a linear interpolation, where
/// scale factor is interpolated logarithmically.
///
/// Read source comments to see how this is relevant.
fn average_lerped_scale(s1: Scale, s2: Scale) -> Scale {
    let s1 = s1.log2_factor();
    let s2 = s2.log2_factor();
    // Read the comments in the first half of `Viewpoint2D::lerp()` before
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
