//! OpenGL vertex buffer objects.

use glium::vertex::{VertexBuffer, VertexBufferSlice};

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::consts::{MOUSE_TARGET_BATCH_SIZE, QUAD_BATCH_SIZE};
use super::vertices::*;
use crate::ext::*;
use crate::DISPLAY;

fn empty_vbo<T: glium::Vertex>(size: usize) -> VertexBuffer<T> {
    VertexBuffer::empty_dynamic(&**DISPLAY, size).expect("Failed to create vertex buffer")
}

pub struct VboCache {
    ndtree_quad: VertexBuffer<PosVertex2D>,
    gridlines_quad: VertexBuffer<PosVertex2D>,

    blit_quad: VertexBuffer<TexturePosVertex>,
    quad_verts_2d: VertexBuffer<Vertex2D>,
    quad_verts_3d: VertexBuffer<Vertex3D>,
    mouse_target_verts: VertexBuffer<MouseTargetVertex>,
}
impl Default for VboCache {
    fn default() -> Self {
        Self {
            ndtree_quad: VertexBuffer::immutable(
                &**DISPLAY,
                &[
                    PosVertex2D { pos: [-1.0, -1.0] },
                    PosVertex2D { pos: [1.0, -1.0] },
                    PosVertex2D { pos: [-1.0, 1.0] },
                    PosVertex2D { pos: [1.0, 1.0] },
                ],
            )
            .expect("Failed to create vertex buffer"),
            gridlines_quad: empty_vbo(4),

            blit_quad: empty_vbo(4),
            quad_verts_2d: empty_vbo(4 * QUAD_BATCH_SIZE),
            quad_verts_3d: empty_vbo(4 * QUAD_BATCH_SIZE),
            mouse_target_verts: empty_vbo(3 * MOUSE_TARGET_BATCH_SIZE),
        }
    }
}
impl VboCache {
    pub fn ndtree_quad(&self) -> &VertexBuffer<PosVertex2D> {
        &self.ndtree_quad
    }

    pub fn gridlines_quad(&mut self, rect: FRect2D) -> &mut VertexBuffer<PosVertex2D> {
        let [x1, y1] = rect.min().to_f32_array();
        let [x2, y2] = rect.max().to_f32_array();
        self.gridlines_quad.write(&[
            PosVertex2D { pos: [x1, y1] },
            PosVertex2D { pos: [x2, y1] },
            PosVertex2D { pos: [x1, y2] },
            PosVertex2D { pos: [x2, y2] },
        ]);
        &mut self.gridlines_quad
    }

    pub fn blit_quad_with_src_coords(
        &mut self,
        rect: FRect2D,
    ) -> &mut VertexBuffer<TexturePosVertex> {
        let left = rect.min()[X].to_f32().unwrap();
        let right = rect.max()[X].to_f32().unwrap();
        let bottom = rect.min()[Y].to_f32().unwrap();
        let top = rect.max()[Y].to_f32().unwrap();
        self.blit_quad.write(&[
            TexturePosVertex {
                src_coords: [left, bottom],
                dest_coords: [-1.0, -1.0],
            },
            TexturePosVertex {
                src_coords: [right, bottom],
                dest_coords: [1.0, -1.0],
            },
            TexturePosVertex {
                src_coords: [left, top],
                dest_coords: [-1.0, 1.0],
            },
            TexturePosVertex {
                src_coords: [right, top],
                dest_coords: [1.0, 1.0],
            },
        ]);
        &mut self.blit_quad
    }

    pub fn quad_verts_2d(&mut self, quad_count: usize) -> VertexBufferSlice<'_, Vertex2D> {
        self.quad_verts_2d.slice(..(4 * quad_count)).unwrap()
    }

    pub fn quad_verts_3d(&mut self, quad_count: usize) -> VertexBufferSlice<'_, Vertex3D> {
        self.quad_verts_3d.slice(..(4 * quad_count)).unwrap()
    }

    pub fn mouse_target_verts(&mut self, count: usize) -> VertexBufferSlice<'_, MouseTargetVertex> {
        self.mouse_target_verts.slice(..(3 * count)).unwrap()
    }
}
