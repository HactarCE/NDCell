//! OpenGL vertex buffer objects.

use glium::VertexBuffer;
use send_wrapper::SendWrapper;
use std::cell::{RefCell, RefMut};

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

use super::consts::CELL_OVERLAY_BATCH_SIZE;
use super::vertices::*;
use crate::DISPLAY;

type StaticVbo<T> = SendWrapper<RefCell<VertexBuffer<T>>>;

fn empty_static_vbo<T: glium::Vertex>(size: usize) -> StaticVbo<T> {
    SendWrapper::new(RefCell::new(
        VertexBuffer::empty_dynamic(&**DISPLAY, size).expect("Failed to create vertex buffer"),
    ))
}

lazy_static! {
    static ref QUADTREE_QUAD: StaticVbo<QuadtreePosVertex> = empty_static_vbo(4);
    static ref BLIT_QUAD: StaticVbo<TexturePosVertex> = empty_static_vbo(4);
    static ref GRIDLINES: StaticVbo<RgbaVertex> = empty_static_vbo(4 * CELL_OVERLAY_BATCH_SIZE);
}

pub fn quadtree_quad<'a>() -> RefMut<'a, VertexBuffer<QuadtreePosVertex>> {
    QUADTREE_QUAD.borrow_mut()
}
pub fn quadtree_quad_with_quadtree_coords<'a>(
    rect: IRect2D,
    texture_fraction: FVec2D,
) -> RefMut<'a, VertexBuffer<QuadtreePosVertex>> {
    let ret = quadtree_quad();
    let left = rect.min()[X] as i32;
    let right = rect.max()[X] as i32;
    let bottom = rect.min()[Y] as i32;
    let top = rect.max()[Y] as i32;
    let dest_top_right = texture_fraction * r64(2.0) - r64(1.0);
    let dest_right = dest_top_right[X].to_f32().unwrap();
    let dest_top = dest_top_right[Y].to_f32().unwrap();
    ret.write(&[
        QuadtreePosVertex {
            cell_coords: [left, bottom],
            dest_coords: [-1.0, -1.0],
        },
        QuadtreePosVertex {
            cell_coords: [right, bottom],
            dest_coords: [dest_right, -1.0],
        },
        QuadtreePosVertex {
            cell_coords: [left, top],
            dest_coords: [-1.0, dest_top],
        },
        QuadtreePosVertex {
            cell_coords: [right, top],
            dest_coords: [dest_right, dest_top],
        },
    ]);
    ret
}

pub fn blit_quad<'a>() -> RefMut<'a, VertexBuffer<TexturePosVertex>> {
    BLIT_QUAD.borrow_mut()
}
pub fn blit_quad_with_src_coords<'a>(rect: FRect2D) -> RefMut<'a, VertexBuffer<TexturePosVertex>> {
    let ret = blit_quad();
    let left = rect.min()[X].to_f32().unwrap();
    let right = rect.max()[X].to_f32().unwrap();
    let bottom = rect.min()[Y].to_f32().unwrap();
    let top = rect.max()[Y].to_f32().unwrap();
    ret.write(&[
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
    ret
}

pub fn gridlines<'a>() -> RefMut<'a, VertexBuffer<RgbaVertex>> {
    GRIDLINES.borrow_mut()
}
