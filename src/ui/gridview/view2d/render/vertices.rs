use glium::implement_vertex;

/// A vertex containing a 2D floating-point position and a 2D texture position.
#[derive(Debug, Default, Copy, Clone)]
pub struct TexturePosVertex {
    pub src_coords: [f32; 2],
    pub dest_coords: [f32; 2],
}
implement_vertex!(TexturePosVertex, src_coords, dest_coords);

/// A vertex containing a 2D floating-point position and a 2D cell position.
#[derive(Debug, Default, Copy, Clone)]
pub struct QuadtreePosVertex {
    pub cell_coords: [i32; 2],
    pub dest_coords: [f32; 2],
}
implement_vertex!(QuadtreePosVertex, cell_coords, dest_coords);

/// A vertex containing a 2D floating-point position and an RGBA color.
#[derive(Debug, Default, Copy, Clone)]
pub struct RgbaVertex {
    pub pos: [f32; 2],
    pub color: [f32; 4],
}
implement_vertex!(RgbaVertex, pos, color);
impl From<([isize; 2], [f32; 4])> for RgbaVertex {
    fn from(pos_and_color: ([isize; 2], [f32; 4])) -> Self {
        let (pos, color) = pos_and_color;
        Self::from(([pos[0] as f32, pos[1] as f32], color))
    }
}
impl From<([f32; 2], [f32; 4])> for RgbaVertex {
    fn from(pos_and_color: ([f32; 2], [f32; 4])) -> Self {
        let (pos, color) = pos_and_color;
        Self { pos, color }
    }
}
