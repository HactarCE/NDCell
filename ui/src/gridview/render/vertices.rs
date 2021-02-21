//! OpenGL vertex types.

use glium::implement_vertex;
use palette::{Pixel, Srgba};

/// Vertex containing only a 2D NDC position.
#[derive(Debug, Default, Copy, Clone)]
pub struct PosVertex2D {
    pub pos: [f32; 2],
}
implement_vertex!(PosVertex2D, pos);

/// Vertex containing a 2D floating-point position and a 2D texture position.
#[derive(Debug, Default, Copy, Clone)]
pub struct TexturePosVertex {
    pub src_coords: [f32; 2],
    pub dest_coords: [f32; 2],
}
implement_vertex!(TexturePosVertex, src_coords, dest_coords);

/// Vertex containing a 3D floating-point position and an sRGBA color.
#[derive(Debug, Default, Copy, Clone)]
pub struct Vertex2D {
    pos: [f32; 2],
    color: [u8; 4],
}
implement_vertex!(Vertex2D, pos normalize(false), color normalize(true));
impl Vertex2D {
    pub fn new(pos: [f32; 2], color: Srgba) -> Self {
        let color = color.into_format().into_raw();
        Self { pos, color }
    }
}

/// Vertex containing a 3D position, normal vector, and an sRGBA color.
#[derive(Debug, Default, Copy, Clone)]
pub struct Vertex3D {
    pos: [f32; 3],
    normal: [i8; 3],
    color: [u8; 4],
}
implement_vertex!(Vertex3D, pos normalize(false), normal normalize(false), color normalize(true));
impl Vertex3D {
    pub fn new(pos: [f32; 3], normal: [i8; 3], color: Srgba) -> Self {
        let color = color.into_format().into_raw();
        Self { pos, normal, color }
    }
}

/// Vertex containing a 3D floating-point position and an ID, for use with the
/// pixel buffer object to map out regions that the mouse cursor can interact
/// with.
#[derive(Debug, Default, Copy, Clone)]
pub struct MouseTargetVertex {
    pub pos: [f32; 3],
    pub target_id: u32,
}
implement_vertex!(MouseTargetVertex, pos, target_id);
