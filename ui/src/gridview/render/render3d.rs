//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use anyhow::{Context, Result};
use glium::index::PrimitiveType;
use glium::uniforms::UniformBuffer;
use glium::Surface;
use sloth::Lazy;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::Vertex3D;
use super::CellDrawParams;
use crate::ext::*;
use crate::gridview::*;
use crate::{CONFIG, DISPLAY};

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

type QuadVerts = [Vertex3D; 4];
type CuboidVerts = [Option<QuadVerts>; 6];

type LazyUbo<T> = Lazy<UniformBuffer<T>, Box<dyn FnOnce() -> UniformBuffer<T>>>;

const DEPTH_TEST: glium::Depth = glium::Depth {
    test: glium::DepthTest::IfLessOrEqual,
    write: true,
    range: (0.0, 1.0),                                  // default
    clamp: glium::draw_parameters::DepthClamp::NoClamp, // default
};

pub(in crate::gridview) struct RenderDim3D {
    fog_uniform: LazyUbo<FogParams>,
    light_uniform: LazyUbo<LightParams>,
    gridline_uniform: LazyUbo<GridlineParams>,
}
impl<'a> GridViewRenderDimension<'a> for RenderDim3D {
    type D = Dim3D;
    type Viewpoint = Viewpoint3D;
    type OverlayQuad = ();

    const DEFAULT_COLOR: [f32; 4] = crate::colors::BACKGROUND_3D;
    const DEFAULT_DEPTH: f32 = f32::INFINITY;

    fn init(gvr3d: &GridViewRender3D<'a>) -> Self {
        let fog_params = FogParams::from(gvr3d);
        let light_params = LightParams::default();
        let gridline_params = GridlineParams::from(gvr3d);

        fn lazy_ubo<U: 'static + Copy>(u: U) -> LazyUbo<U> {
            Lazy::new(Box::new(move || {
                UniformBuffer::new(&**DISPLAY, u).expect("Failed to create uniform buffer")
            }))
        }

        Self {
            fog_uniform: lazy_ubo(fog_params),
            light_uniform: lazy_ubo(light_params),
            gridline_uniform: lazy_ubo(gridline_params),
        }
    }

    fn draw_overlay_quads(this: &mut GridViewRender3D<'a>) -> Result<()> {
        // todo!()
        Ok(())
    }
}

impl GridViewRender3D<'_> {
    /// Draw an ND-tree to scale on the target.
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim3D>) -> Result<()> {
        let visible_octree = match self.clip_ndtree_to_visible(&params) {
            Some(x) => x,
            None => return Ok(()), // There is nothing to draw.
        };

        let octree_offset = self
            .xform
            .global_to_local_int(&visible_octree.base_pos)
            .unwrap();

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        let gl_octree = cache.gl_octrees.gl_ndtree_from_node(
            (&visible_octree.root).into(),
            self.xform.render_cell_layer,
            Self::ndtree_node_color,
        )?;

        self.params
            .target
            .draw(
                &*vbos.ndtree_quad(),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::OCTREE.load(),
                &uniform! {
                    matrix: self.xform.gl_matrix(),

                    octree_texture: &gl_octree.texture,
                    layer_count: gl_octree.layers,
                    root_idx: gl_octree.root_idx,

                    octree_offset: octree_offset.to_i32_array(),

                    perf_view: CONFIG.lock().gfx.octree_perf_view,

                    FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                    LightParams: &**self.dim.as_ref().unwrap().light_uniform,
                },
                &glium::DrawParameters {
                    depth: DEPTH_TEST,
                    blend: glium::Blend::alpha_blending(),
                    ..Default::default()
                },
            )
            .expect("Drawing cells");

        Ok(())
    }

    pub fn draw_gridlines(
        &mut self,
        perpendicular_axis: Axis,
        perpendicular_coordinate: BigInt,
    ) -> Result<()> {
        let (ax1, ax2) = match perpendicular_axis {
            X => (Y, Z),
            Y => (Z, X),
            Z => (X, Y),
            _ => panic!("Invalid axis"),
        };

        // Convert `perpendicular_coordinate` to local space (still
        // `FixedPoint`, because this number may be very large).
        let mut tmp_global_pos = self.xform.origin.clone();
        tmp_global_pos[perpendicular_axis] = perpendicular_coordinate;
        let local_perpendicular_coordinate: f32;
        if let Some(tmp_local_pos) = self.xform.global_to_local_int(&tmp_global_pos) {
            if self.local_visible_rect.contains(&tmp_local_pos) {
                local_perpendicular_coordinate = tmp_local_pos[perpendicular_axis] as f32;
            } else {
                // The gridline plane isn't even visible.
                return Ok(());
            }
        } else {
            // The gridline plane isn't even visible.
            return Ok(());
        };

        // Compute quad.
        let mut min = self.local_visible_rect.min().to_fvec();
        let mut max = self.local_visible_rect.max().to_fvec();
        min[perpendicular_axis] = r64(0.0);
        max[perpendicular_axis] = r64(0.0);
        let quad = FRect2D::span(NdVec([min[ax1], min[ax2]]), NdVec([max[ax1], max[ax2]]));

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        self.params
            .target
            .draw(
                &*vbos.gridlines_quad(quad),
                &ibos.quad_indices(1),
                &shaders::GRIDLINES_3D.load(),
                &uniform! {
                    matrix: self.xform.gl_matrix(),

                    grid_axes: [ax1 as i32, ax2 as i32],
                    other_coordinate: local_perpendicular_coordinate,

                    GridlineParams: &**self.dim.as_ref().unwrap().gridline_uniform,
                    FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                },
                &glium::DrawParameters {
                    depth: DEPTH_TEST,
                    blend: glium::Blend::alpha_blending(),
                    backface_culling: glium::BackfaceCullingMode::CullingDisabled,
                    ..Default::default()
                },
            )
            .context("Drawing gridlines")?;

        Ok(())
    }

    fn draw_quads(&mut self, quad_verts: &[Vertex3D]) -> Result<()> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        for chunk in quad_verts.chunks(4 * QUAD_BATCH_SIZE) {
            let count = chunk.len() / 4;

            // Copy that into a VBO.
            let vbo_slice = vbos.quad_verts_3d(count);
            vbo_slice.write(&chunk);

            self.params
                .target
                .draw(
                    vbo_slice,
                    &ibos.quad_indices(count),
                    &shaders::GRIDLINES_3D.load(),
                    &uniform! {
                        matrix: self.xform.gl_matrix(),

                        LightParams: &**self.dim.as_ref().unwrap().light_uniform,
                        FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                    },
                    &glium::DrawParameters {
                        depth: DEPTH_TEST,
                        blend: glium::Blend::alpha_blending(),
                        smooth: Some(glium::Smooth::Nicest),
                        ..Default::default()
                    },
                )
                .context("Drawing faces to target")?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
struct FogParams {
    fog_color: [f32; 4],
    fog_center: [f32; 3],
    fog_start: f32,
    fog_end: f32,
}
type FogUbo = UniformBuffer<FogParams>;
implement_uniform_block!(FogParams, fog_color, fog_center, fog_start, fog_end);
impl From<&GridViewRender3D<'_>> for FogParams {
    fn from(gvr3d: &GridViewRender3D<'_>) -> Self {
        let fog_color = crate::colors::BACKGROUND_3D;

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

#[derive(Debug, Copy, Clone)]
struct LightParams {
    light_direction: [f32; 3],
    light_ambientness: f32,
    light_multiplier: f32,
}
type LightUbo = UniformBuffer<LightParams>;
implement_uniform_block!(
    LightParams,
    light_direction,
    light_ambientness,
    light_multiplier,
);
impl Default for LightParams {
    fn default() -> Self {
        Self {
            light_direction: LIGHT_DIRECTION,
            light_ambientness: LIGHT_AMBIENTNESS,
            light_multiplier: LIGHT_MULTIPLIER,
        }
    }
}

/// Gridline rendering parameters.
///
/// Fields are arranged to eliminate padding.
#[derive(Debug, Default, Copy, Clone)]
struct GridlineParams {
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
type GridlineUbo = UniformBuffer<GridlineParams>;
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
            grid_color: crate::colors::GRIDLINES,
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

/*
fn cuboid_verts(real_camera_pos: FVec3D, cuboid: FRect3D, color: [u8; 3]) -> CuboidVerts {
    let make_face_verts = |axis, sign| face_verts(real_camera_pos, cuboid, (axis, sign), color);
    [
        make_face_verts(X, Sign::Minus),
        make_face_verts(X, Sign::Plus),
        make_face_verts(Y, Sign::Minus),
        make_face_verts(Y, Sign::Plus),
        make_face_verts(Z, Sign::Minus),
        make_face_verts(Z, Sign::Plus),
    ]
}
fn face_verts(
    real_camera_pos: FVec3D,
    cuboid: FRect3D,
    face: (Axis, Sign),
    color: [u8; 3],
) -> Option<QuadVerts> {
    let (face_axis, face_sign) = face;

    let normal = match face {
        (X, Sign::Minus) => [i8::MIN, 0, 0],
        (X, Sign::Plus) => [i8::MAX, 0, 0],
        (Y, Sign::Minus) => [0, i8::MIN, 0],
        (Y, Sign::Plus) => [0, i8::MAX, 0],
        (Z, Sign::Minus) => [0, 0, i8::MIN],
        (Z, Sign::Plus) => [0, 0, i8::MAX],
        _ => return None,
    };

    let (mut ax1, mut ax2) = match face_axis {
        X => (Y, Z),
        Y => (Z, X),
        Z => (X, Y),
        _ => return None,
    };
    if face_sign == Sign::Plus {
        std::mem::swap(&mut ax1, &mut ax2);
    }

    let mut pos0 = cuboid.min();
    let mut pos3 = cuboid.max();

    // Backface culling
    if real_camera_pos[face_axis] < pos3[face_axis] && face_sign == Sign::Plus {
        // The camera is on the negative side, but this is the positive face.
        return None;
    }
    if real_camera_pos[face_axis] > pos0[face_axis] && face_sign == Sign::Minus {
        // The camera is on the positive side, but this is the negative face.
        return None;
    }

    match face_sign {
        Sign::Minus => pos3[face_axis] = pos0[face_axis],
        Sign::Plus => pos0[face_axis] = pos3[face_axis],
        _ => return None,
    }

    let mut pos1 = pos0;
    pos1[ax1] = pos3[ax1];

    let mut pos2 = pos0;
    pos2[ax2] = pos3[ax2];

    let [r, g, b] = color;
    let color = [r, g, b, u8::MAX];

    let pos_to_vertex = |NdVec([x, y, z]): FVec3D| Vertex3D {
        pos: [x.raw() as f32, y.raw() as f32, z.raw() as f32],
        normal,
        color,
    };
    Some([
        pos_to_vertex(pos0),
        pos_to_vertex(pos1),
        pos_to_vertex(pos2),
        pos_to_vertex(pos3),
    ])
}
*/
