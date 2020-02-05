use crate::automaton::{FRect2D, IRect2D, X, Y};
use glium::VertexBuffer;
use ref_thread_local::{RefMut, RefThreadLocal};

use super::vertices::*;
use super::GRIDLINE_BATCH_SIZE;

fn empty_vbo<T: glium::Vertex>(size: usize) -> VertexBuffer<T> {
    VertexBuffer::empty_dynamic(&*crate::ui::get_display(), size)
        .expect("Failed to create vertex buffer")
}

ref_thread_local! {
    static managed QUADTREE_QUAD: VertexBuffer<QuadtreePosVertex> = empty_vbo(4);
    static managed BLIT_QUAD: VertexBuffer<TexturePosVertex> = empty_vbo(4);
    static managed GRIDLINES: VertexBuffer<RgbaVertex> = empty_vbo(GRIDLINE_BATCH_SIZE);
}

pub fn quadtree_quad<'a>() -> RefMut<'a, VertexBuffer<QuadtreePosVertex>> {
    QUADTREE_QUAD.borrow_mut()
}
pub fn quadtree_quad_with_quadtree_coords<'a>(
    rect: IRect2D,
) -> RefMut<'a, VertexBuffer<QuadtreePosVertex>> {
    let ret = quadtree_quad();
    let left = rect.min()[X] as i32;
    let right = rect.max()[X] as i32;
    let bottom = rect.min()[Y] as i32;
    let top = rect.max()[Y] as i32;
    ret.write(&[
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
    ret
}

pub fn blit_quad<'a>() -> RefMut<'a, VertexBuffer<TexturePosVertex>> {
    BLIT_QUAD.borrow_mut()
}
pub fn blit_quad_with_src_coords<'a>(rect: FRect2D) -> RefMut<'a, VertexBuffer<TexturePosVertex>> {
    let ret = blit_quad();
    let left = rect.min()[X].raw() as f32;
    let right = rect.max()[X].raw() as f32;
    let bottom = rect.min()[Y].raw() as f32;
    let top = rect.max()[Y].raw() as f32;
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
