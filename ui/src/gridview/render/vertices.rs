//! OpenGL vertex types.

use glium::implement_vertex;

/// Vertex containing only a 2D screen space position.
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

/// Vertex containing a 3D floating-point position and an RGBA color.
#[derive(Debug, Default, Copy, Clone)]
pub struct Vertex2D {
    pub pos: [f32; 3],
    pub color: [f32; 4],
}
implement_vertex!(Vertex2D, pos, color);
impl From<([isize; 2], f32, [f32; 4])> for Vertex2D {
    fn from(pos_and_color: ([isize; 2], f32, [f32; 4])) -> Self {
        let ([x, y], z, color) = pos_and_color;
        Self::from(([x as f32, y as f32, z], color))
    }
}
impl From<([f32; 3], [f32; 4])> for Vertex2D {
    fn from(pos_and_color: ([f32; 3], [f32; 4])) -> Self {
        let (pos, color) = pos_and_color;
        Self { pos, color }
    }
}

/// Vertex containing a 3D position, normal vector, and an RGBA color.
#[derive(Debug, Default, Copy, Clone)]
pub struct Vertex3D {
    pub pos: [f32; 3],
    pub normal: [i8; 3],
    pub color: [u8; 4],
}
implement_vertex!(
    Vertex3D,
    pos normalize(false),
    normal normalize(true),
    color normalize(true)
);

/// Vertex containing a 3D floating-point position and an ID, for use with the
/// pixel buffer object to map out regions that the mouse cursor can interact
/// with.
#[derive(Debug, Default, Copy, Clone)]
pub struct MouseTargetVertex {
    pub pos: [f32; 3],
    pub target_id: u32,
}
implement_vertex!(MouseTargetVertex, pos, target_id);
