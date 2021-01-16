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

mod fog;
mod gridlines;
mod lighting;

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::Vertex3D;
use super::CellDrawParams;
use crate::ext::*;
use crate::gridview::*;
use crate::{CONFIG, DISPLAY};
use fog::FogParams;
use gridlines::GridlineParams;
use lighting::LightingParams;

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
    gridline_uniform: LazyUbo<GridlineParams>,
    lighting_uniform: LazyUbo<LightingParams>,
}
impl<'a> GridViewRenderDimension<'a> for RenderDim3D {
    type D = Dim3D;
    type Viewpoint = Viewpoint3D;
    type OverlayQuad = ();

    const DEFAULT_COLOR: [f32; 4] = crate::colors::BACKGROUND_3D;
    const DEFAULT_DEPTH: f32 = f32::INFINITY;

    fn init(gvr3d: &GridViewRender3D<'a>) -> Self {
        let fog_params = FogParams::from(gvr3d);
        let gridline_params = GridlineParams::from(gvr3d);
        let lighting_params = LightingParams::default();

        fn lazy_ubo<U: 'static + Copy>(u: U) -> LazyUbo<U> {
            Lazy::new(Box::new(move || {
                UniformBuffer::new(&**DISPLAY, u).expect("Failed to create uniform buffer")
            }))
        }

        Self {
            fog_uniform: lazy_ubo(fog_params),
            gridline_uniform: lazy_ubo(gridline_params),
            lighting_uniform: lazy_ubo(lighting_params),
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
                    LightingParams: &**self.dim.as_ref().unwrap().lighting_uniform,
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

                    FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                    GridlineParams: &**self.dim.as_ref().unwrap().gridline_uniform,
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

                        FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                        LightingParams: &**self.dim.as_ref().unwrap().lighting_uniform,
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
