//! OpenGL vertex buffer objects.

use glium::VertexBuffer;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

use super::consts::{CELL_OVERLAY_BATCH_SIZE, MOUSE_TARGET_BATCH_SIZE};
use super::vertices::*;
use crate::DISPLAY;

fn empty_vbo<T: glium::Vertex>(size: usize) -> VertexBuffer<T> {
    VertexBuffer::empty_dynamic(&**DISPLAY, size).expect("Failed to create vertex buffer")
}

pub struct VboCache {
    quadtree_quad: VertexBuffer<QuadtreePosVertex>,
    blit_quad: VertexBuffer<TexturePosVertex>,
    rgba_verts: VertexBuffer<RgbaVertex>,
    mouse_target_verts: VertexBuffer<MouseTargetVertex>,
}
impl Default for VboCache {
    fn default() -> Self {
        Self {
            quadtree_quad: empty_vbo(4),
            blit_quad: empty_vbo(4),
            rgba_verts: empty_vbo(4 * CELL_OVERLAY_BATCH_SIZE),
            mouse_target_verts: empty_vbo(3 * MOUSE_TARGET_BATCH_SIZE),
        }
    }
}
impl VboCache {
    pub fn quadtree_quad_with_quadtree_coords(
        &mut self,
        rect: IRect2D,
    ) -> &mut VertexBuffer<QuadtreePosVertex> {
        let left = rect.min()[X] as i32;
        let right = rect.max()[X] as i32;
        let bottom = rect.min()[Y] as i32;
        let top = rect.max()[Y] as i32;
        self.quadtree_quad.write(&[
            QuadtreePosVertex {
                cell_coords: [left, bottom],
                dest_coords: [-1.0, -1.0],
            },
            QuadtreePosVertex {
                cell_coords: [right, bottom],
                dest_coords: [1.0, -1.0],
            },
            QuadtreePosVertex {
                cell_coords: [left, top],
                dest_coords: [-1.0, 1.0],
            },
            QuadtreePosVertex {
                cell_coords: [right, top],
                dest_coords: [1.0, 1.0],
            },
        ]);
        &mut self.quadtree_quad
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

    pub fn rgba_verts(&mut self) -> &mut VertexBuffer<RgbaVertex> {
        &mut self.rgba_verts
    }

    pub fn mouse_target_verts(&mut self) -> &mut VertexBuffer<MouseTargetVertex> {
        &mut self.mouse_target_verts
    }
}
