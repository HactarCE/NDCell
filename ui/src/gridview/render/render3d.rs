//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use anyhow::{Context, Result};
use glium::index::PrimitiveType;
use glium::Surface;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::Vertex3D;
use super::CellDrawParams;
use crate::gridview::*;

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

type QuadVerts = [Vertex3D; 4];
type CuboidVerts = [Option<QuadVerts>; 6];

#[derive(Default)]
pub(in crate::gridview) struct RenderDim3D;
impl GridViewRenderDimension<'_> for RenderDim3D {
    type D = Dim3D;
    type Camera = Camera3D;

    const DEFAULT_COLOR: (f32, f32, f32, f32) = crate::colors::BACKGROUND_3D;
    const DEFAULT_DEPTH: f32 = f32::INFINITY;
}

impl GridViewRender3D<'_> {
    /// Draw an ND-tree to scale on the target.
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim3D>) -> Result<()> {
        let (visible_octree, _visible_cuboid) =
            match self.clip_ndtree_to_visible_render_cells(&params) {
                Some(x) => x,
                None => return Ok(()), // There is nothing to draw.
            };

        let fog_center = ((self.camera.pos() - self.origin.to_fixedvec())
            / FixedPoint::from(self.render_cell_layer.big_len()))
        .to_fvec();

        let octree_offset = (visible_octree.offset - &self.origin)
            .div_floor(&self.render_cell_layer.big_len())
            .to_ivec();

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        let gl_octree = cache.gl_octrees.gl_ndtree_from_node(
            (&visible_octree.root).into(),
            self.render_cell_layer,
            Self::ndtree_node_color,
        )?;

        self.params
            .target
            .draw(
                &*vbos.ndtree_quad(),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::OCTREE.load(),
                &uniform! {
                    matrix: self.transform.gl_matrix(),

                    octree_texture: &gl_octree.texture,
                    layer_count: gl_octree.layers,
                    root_idx: gl_octree.root_idx,
                    // empty_node_idxs: &gl_octree.empty_node_idxs,

                    offset_into_octree: [
                        octree_offset[X] as i32,
                        octree_offset[Y] as i32,
                        octree_offset[Z] as i32,
                    ],

                    perf_view: false,

                    light_direction: LIGHT_DIRECTION,
                    light_ambientness: LIGHT_AMBIENTNESS,
                    max_light: MAX_LIGHT,

                    fog_color: crate::colors::BACKGROUND_3D,
                    fog_center: [
                        fog_center[X].raw() as f32,
                        fog_center[Y].raw() as f32,
                        fog_center[Z].raw() as f32,
                    ],
                    fog_start: FOG_START_FACTOR * 5000.0 * self.render_cell_scale.inv_factor().to_f32().unwrap(),
                    fog_end: 5000.0 * self.render_cell_scale.inv_factor().to_f32().unwrap(),
                },
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLessOrEqual,
                        write: true,
                        ..glium::Depth::default()
                    },
                    blend: glium::Blend::alpha_blending(),
                    smooth: Some(glium::Smooth::Nicest),
                    ..Default::default()
                },
            )
            .context("Drawing cells")?;

        Ok(())
    }

    fn draw_quads(&mut self, quad_verts: &[Vertex3D]) -> Result<()> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        let camera_pos = ((self.camera.pos() - self.origin.to_fixedvec())
            / &FixedPoint::from(self.render_cell_layer.big_len()))
            .to_fvec();

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
                        matrix: self.transform.gl_matrix(),

                        light_direction: LIGHT_DIRECTION,
                        light_ambientness: LIGHT_AMBIENTNESS,
                        max_light: MAX_LIGHT,

                        fog_color: crate::colors::BACKGROUND_3D,
                        fog_center: [
                            camera_pos[X].raw() as f32,
                            camera_pos[Y].raw() as f32,
                            camera_pos[Z].raw() as f32,
                        ],
                        fog_start: FOG_START_FACTOR * 5000.0 * self.render_cell_scale.inv_factor().to_f32().unwrap(),
                        fog_end: 5000.0 * self.render_cell_scale.inv_factor().to_f32().unwrap(),
                    },
                    &glium::DrawParameters {
                        depth: glium::Depth {
                            test: glium::DepthTest::IfLessOrEqual,
                            write: true,
                            ..glium::Depth::default()
                        },
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
