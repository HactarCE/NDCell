use super::*;

/// Gridline rendering parameters.
///
/// Fields are arranged to eliminate padding.
#[derive(Debug, Default, Copy, Clone)]
pub(super) struct GridlineParams {
    /// Color of gridlines.
    grid_color: [f32; 4],

    /// Local position of the visible gridline with the largest exponent.
    grid_origin: [f32; 3],

    /// Pixel width of gridlines.
    grid_width: f32,

    /// Gridline exponent along each axis at `origin`.
    grid_max_exponents: [i32; 3],
    /// Render cell coefficient for the smallest visible gridlines.
    grid_coefficient: f32,
    /// Exponential base for gridlines.
    grid_base: i32,

    /// High end of the gridline opacity gradient; pixel spacing for gridlines
    /// with maximum opacity.
    grid_max_spacing: f32,
    /// Low end of the gridline opacity gradient; pixel spacing for gridlines
    /// with zero opacity.
    grid_min_spacing: f32,
}

implement_uniform_block!(
    GridlineParams,
    grid_color,
    grid_origin,
    grid_width,
    grid_max_exponents,
    grid_coefficient,
    grid_base,
    grid_max_spacing,
    grid_min_spacing,
);

impl From<&GridViewRender3D<'_>> for GridlineParams {
    fn from(gvr3d: &GridViewRender3D<'_>) -> Self {
        // Compute the coefficient for the smallest visible gridlines.
        let log2_global_min_spacing = (GRIDLINE_SPACING_BASE as f32).log2()
            * (gvr3d.gridline_cell_spacing_exponent(1.0) as f32)
            + (GRIDLINE_SPACING_COEFF as f32).log2();
        let global_min_spacing = FixedPoint::from_f32(log2_global_min_spacing)
            .unwrap()
            .exp2()
            .round();
        let log2_local_min_spacing =
            log2_global_min_spacing - (gvr3d.xform.render_cell_layer.to_u32() as f32);
        let local_coefficient = log2_local_min_spacing.exp2();

        // Find the position of a gridline in or near the visible area with the
        // largest exponent.
        let global_gridline_origin: BigVec3D;
        {
            // Compute the largest gridline spacing that fits within the visible
            // area.
            let max_visible_exponent =
                gvr3d.gridline_cell_spacing_exponent(Viewpoint3D::VIEW_RADIUS as f64 * 2.0);
            let max_visible_spacing = BigInt::from(GRIDLINE_SPACING_BASE)
                .pow(max_visible_exponent + 1)
                * GRIDLINE_SPACING_COEFF;
            // Round to nearest multiple of that spacing.
            global_gridline_origin =
                gvr3d.xform.origin.div_floor(&max_visible_spacing) * &max_visible_spacing;
        }

        // Compute the maximum exponent that will be visible for each axis.
        // There is a similar loop in the 3D gridlines fragment shader.
        let mut max_exponents = IVec3D::repeat(0);
        for &ax in Dim3D::axes() {
            const LARGE_EXPONENT: isize = 16;
            let spacing_base: BigInt = GRIDLINE_SPACING_BASE.into();

            let mut tmp: BigInt = global_gridline_origin[ax].div_floor(&global_min_spacing);
            if tmp.is_zero() {
                max_exponents[ax] = LARGE_EXPONENT;
            } else {
                while tmp.mod_floor(&spacing_base).is_zero() && max_exponents[ax] < LARGE_EXPONENT {
                    tmp /= GRIDLINE_SPACING_BASE;
                    max_exponents[ax] += 1;
                }
            }
        }

        let local_gridline_origin = gvr3d
            .xform
            .global_to_local_float(&global_gridline_origin.to_fixedvec())
            .unwrap();

        let line_width = if gvr3d.xform.render_cell_layer == Layer(0) {
            GRIDLINE_WIDTH as f32
        } else {
            0.0 // minimum width of one pixel
        };

        Self {
            grid_color: crate::colors::GRIDLINES.into_raw(),
            grid_width: line_width,

            grid_origin: local_gridline_origin.to_f32_array(),
            grid_max_exponents: max_exponents.to_i32_array(),
            grid_coefficient: local_coefficient,
            grid_base: GRIDLINE_SPACING_BASE as i32,

            grid_min_spacing: GRIDLINE_ALPHA_GRADIENT_LOW_PIXEL_SPACING as f32,
            grid_max_spacing: GRIDLINE_ALPHA_GRADIENT_HIGH_PIXEL_SPACING as f32,
        }
    }
}
