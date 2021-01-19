use super::*;

/// Fog rendering parameters.
///
/// Fields are arranged to eliminate padding.
#[derive(Debug, Copy, Clone)]
pub(super) struct FogParams {
    fog_color: [f32; 3],
    fog_start: f32,
    fog_center: [f32; 3],
    fog_end: f32,
}

implement_uniform_block!(FogParams, fog_color, fog_center, fog_start, fog_end);

impl From<&GridViewRender3D<'_>> for FogParams {
    fn from(gvr3d: &GridViewRender3D<'_>) -> Self {
        let fog_color = crate::colors::BACKGROUND_3D.into_raw();

        let fog_center = gvr3d
            .xform
            .global_to_local_float(gvr3d.viewpoint.center())
            .unwrap()
            .to_f32_array();

        let inv_scale_factor = gvr3d.xform.render_cell_scale.inv_factor().to_f32().unwrap();
        let fog_end = Viewpoint3D::VIEW_RADIUS * inv_scale_factor;

        let fog_start = FOG_START_FACTOR * fog_end;

        Self {
            fog_color,
            fog_center,
            fog_start,
            fog_end,
        }
    }
}
