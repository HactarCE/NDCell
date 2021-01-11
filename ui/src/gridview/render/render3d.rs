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
use crate::CONFIG;

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

type QuadVerts = [Vertex3D; 4];
type CuboidVerts = [Option<QuadVerts>; 6];

#[derive(Default)]
pub(in crate::gridview) struct RenderDim3D {
    fog_center: [f32; 3],
    fog_start: f32,
    fog_end: f32,
}
impl<'a> GridViewRenderDimension<'a> for RenderDim3D {
    type D = Dim3D;
    type Viewpoint = Viewpoint3D;

    const DEFAULT_COLOR: (f32, f32, f32, f32) = crate::colors::BACKGROUND_3D;
    const DEFAULT_DEPTH: f32 = f32::INFINITY;

    fn init(mut this: GridViewRender3D<'a>) -> GridViewRender3D<'a> {
        let NdVec([x, y, z]) = this
            .xform
            .global_to_local_float(this.viewpoint.center())
            .unwrap();
        this.dim.fog_center = [x.raw() as f32, y.raw() as f32, z.raw() as f32];

        let inv_scale_factor = this.xform.render_cell_scale.inv_factor().to_f32().unwrap();
        this.dim.fog_end = Viewpoint3D::VIEW_RADIUS * inv_scale_factor;

        this.dim.fog_start = FOG_START_FACTOR * this.dim.fog_end;

        this
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
            .global_to_local_int(&visible_octree.offset)
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

                    offset_into_octree: [
                        octree_offset[X] as i32,
                        octree_offset[Y] as i32,
                        octree_offset[Z] as i32,
                    ],

                    perf_view: CONFIG.lock().gfx.octree_perf_view,

                    light_direction: LIGHT_DIRECTION,
                    light_ambientness: LIGHT_AMBIENTNESS,
                    max_light: MAX_LIGHT,

                    fog_color: crate::colors::BACKGROUND_3D,
                    fog_center: self.dim.fog_center,
                    fog_start: self.dim.fog_start,
                    fog_end: self.dim.fog_end,
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

    pub fn draw_gridlines(&mut self) -> Result<()> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        let mut min = self.local_visible_rect.min().to_fvec();
        let mut max = self.local_visible_rect.max().to_fvec();
        min[Z] = r64(0.0);
        max[Z] = r64(0.0);
        let rect = FRect2D::span(NdVec([min[X], min[Y]]), NdVec([max[X], max[Y]]));

        // TODO: Proper local grid origin
        let grid_origin = self
            .xform
            .global_to_local_float(&FixedVec::origin())
            .expect("Gridline origin is not implemented correctly");

        self.params
            .target
            .draw(
                &*vbos.gridlines_quad(rect),
                &ibos.quad_indices(1),
                &shaders::GRIDLINES_3D.load(),
                &uniform! {
                    matrix: self.xform.gl_matrix(),

                    grid_axes: [0_i32, 1],
                    grid_color: crate::colors::GRIDLINES,
                    grid_origin: [
                        grid_origin[X].raw() as f32,
                        grid_origin[Y].raw() as f32,
                        grid_origin[Z].raw() as f32,
                    ],
                    grid_coefficient: GRIDLINE_SPACING_COEFF as f32,
                    grid_base: GRIDLINE_SPACING_BASE as i32,
                    min_line_spacing: GRIDLINE_ALPHA_GRADIENT_LOW_PIXEL_SPACING as f32,
                    max_line_spacing: GRIDLINE_ALPHA_GRADIENT_HIGH_PIXEL_SPACING as f32,
                    line_width: if self.xform.render_cell_layer == Layer(0) {
                        GRIDLINE_WIDTH as f32
                    } else {
                        0.0 // minimum width of one pixel
                    },

                    fog_color: crate::colors::BACKGROUND_3D,
                    fog_center: self.dim.fog_center,
                    fog_start: self.dim.fog_start,
                    fog_end: self.dim.fog_end,
                },
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLessOrEqual,
                        write: true,
                        ..glium::Depth::default()
                    },
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

                        light_direction: LIGHT_DIRECTION,
                        light_ambientness: LIGHT_AMBIENTNESS,
                        max_light: MAX_LIGHT,

                        fog_color: crate::colors::BACKGROUND_3D,
                        fog_center: self.dim.fog_center,
                        fog_start: self.dim.fog_start,
                        fog_end: self.dim.fog_end,
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
