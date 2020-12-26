//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use anyhow::{Context, Result};
use glium::Surface;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::IntVertex3D;
use super::CellDrawParams;
use crate::gridview::*;

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

type QuadVerts = [IntVertex3D; 4];
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
    #[optick_attr::profile]
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim3D>) -> Result<()> {
        let (visible_octree, visible_rect) = match self.clip_ndtree_to_visible_render_cells(&params)
        {
            Some(x) => x,
            None => return Ok(()), // There is nothing to draw.
        };

        // Position of the actual camera, not the camera pivot.
        let real_camera_pos = ((self.camera.camera_pos() - self.origin.to_fixedvec())
            / FixedPoint::from(self.render_cell_layer.big_len()))
        .to_fvec();

        let mut quad_verts: Vec<IntVertex3D> = vec![];
        let octree_offset = (visible_octree.offset - &self.origin)
            .div_floor(&self.render_cell_layer.big_len())
            .to_ivec();
        {
            optick::event!("build node quads");
            self.build_node_quads(
                &mut quad_verts,
                visible_octree.root.as_ref(),
                octree_offset,
                Some(visible_rect + octree_offset),
                real_camera_pos,
            );
        }
        self.draw_quads(&quad_verts)?;

        Ok(())
    }

    fn build_node_quads(
        &self,
        buffer: &mut Vec<IntVertex3D>,
        octree_node: NodeRef<'_, Dim3D>,
        node_offset: IVec3D,
        mut visible_rect: Option<IRect3D>,
        real_camera_pos: FVec3D,
    ) {
        // Don't draw empty nodes.
        if octree_node.is_empty() {
            return;
        }

        let side_len = (octree_node.layer() - self.render_cell_layer)
            .len()
            .unwrap() as isize;

        if let Some(vis_rect) = visible_rect {
            let node_rect = IRect::with_size(node_offset, IVec::repeat(side_len));
            // Don't draw nodes outside the visible cuboid.
            if !vis_rect.intersects(&node_rect) {
                return;
            }
            // Don't bother keeping track of the visible cuboid if this node is
            // completely inside it.
            if vis_rect.contains(&node_rect) {
                visible_rect = None;
            }
        }

        if octree_node.layer() > self.render_cell_layer {
            // The node is too big; subdivide and recurse.
            for (i, child) in octree_node.subdivide().unwrap().into_iter().enumerate() {
                let child_offset = Layer(1).child_offset(i).unwrap().to_ivec() * (side_len / 2);
                self.build_node_quads(
                    buffer,
                    child,
                    node_offset + child_offset,
                    visible_rect,
                    real_camera_pos,
                )
            }
        } else {
            // The node is small enough; build the vertices.
            let [r, g, b, _a] = Self::ndtree_node_color(octree_node);
            {
                optick::event!("extending buffer");
                buffer.extend(
                    self.cuboid_verts(
                        real_camera_pos,
                        IRect3D::single_cell(node_offset),
                        [r, g, b],
                    )
                    .iter()
                    .flatten()
                    .flatten()
                    .copied(),
                );
            }
        }
    }

    #[optick_attr::profile]
    fn draw_quads(&mut self, quad_verts: &[IntVertex3D]) -> Result<()> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        let matrix: [[f32; 4]; 4] =
            (self.transform.projection_transform * self.transform.render_cell_transform).into();

        for chunk in quad_verts.chunks(4 * QUAD_BATCH_SIZE) {
            let count = chunk.len() / 4;

            // Copy that into a VBO.
            let vbo_slice = vbos.quad_int_verts_3d(count);
            {
                optick::event!("writing to VBO");
                vbo_slice.write(&chunk);
            }

            {
                optick::event!("draw call");
                self.params
                    .target
                    .draw(
                        vbo_slice,
                        &ibos.quad_indices(count),
                        &shaders::RGB3D,
                        &uniform! {
                            matrix: matrix,

                            light_direction: LIGHT_DIRECTION,
                            light_ambientness: LIGHT_AMBIENTNESS,
                            max_light: MAX_LIGHT,
                        },
                        &glium::DrawParameters {
                            depth: glium::Depth {
                                test: glium::DepthTest::IfLessOrEqual,
                                write: true,
                                ..glium::Depth::default()
                            },
                            blend: glium::Blend::alpha_blending(),
                            polygon_mode: glium::PolygonMode::Line,
                            smooth: Some(glium::Smooth::Nicest),
                            ..Default::default()
                        },
                    )
                    .context("Drawing faces to target")?;
            }
        }
        Ok(())
    }

    fn cuboid_verts(
        &self,
        real_camera_pos: FVec3D,
        cuboid: IRect3D,
        color: [u8; 3],
    ) -> CuboidVerts {
        let make_face_verts =
            |axis, sign| self.face_verts(real_camera_pos, cuboid, (axis, sign), color);
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
        &self,
        real_camera_pos: FVec3D,
        cuboid: IRect3D,
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
        let mut pos3 = cuboid.max() + 1;

        // Backface culling
        if real_camera_pos[face_axis] < r64(pos0[face_axis] as f64) && face_sign == Sign::Plus {
            // The camera is on the negative side, but this is the positive face.
            return None;
        }
        if real_camera_pos[face_axis] > r64(pos3[face_axis] as f64) && face_sign == Sign::Minus {
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

        let pos_to_vertex = |NdVec([x, y, z]): IVec3D| IntVertex3D {
            pos: [x as i16, y as i16, z as i16],
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
}
