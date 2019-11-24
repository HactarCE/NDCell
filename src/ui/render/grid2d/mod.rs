use glium::*;

mod controller;

use super::shaders;
use super::AutomatonSlice2D;
use crate::automaton::*;

#[derive(Copy, Clone)]
pub struct Vertex {
    position: [f32; 2],
    color: [f32; 4],
}
implement_vertex!(Vertex, position, color);

pub struct Grid2D {
    pub slice: Box<dyn AutomatonSlice2D>,
    pub x: f32,
    pub y: f32,
    pub scale: f32,
}
impl Grid2D {
    pub fn new(slice: Box<dyn AutomatonSlice2D>) -> Self {
        Self {
            slice,
            x: 0.0f32,
            y: 0.0f32,
            scale: 32.0f32,
        }
    }

    pub fn draw_editor(&mut self, display: &Display, target: &mut glium::Frame) {
        target.clear_color_srgb(0.2, 0.2, 0.2, 1.0);

        let (frame_w, frame_h) = target.get_dimensions();
        let (w, h) = (frame_w as f32 / self.scale, frame_h as f32 / self.scale);

        let cell_w = frame_w as f32 / 2.0 / self.scale;
        let cell_h = frame_h as f32 / 2.0 / self.scale;

        let left = (self.x - cell_w).floor() as isize;
        let right = (self.x + cell_w).ceil() as isize;
        let bottom = (self.y - cell_h).floor() as isize;
        let top = (self.y + cell_h).ceil() as isize;

        let mut vertices = vec![];
        for y in bottom..top {
            for x in left..right {
                vertices.push(Vertex {
                    position: [x as f32, y as f32],
                    color: if self.slice.get_cell_state([x, y].into()) {
                        [0.5, 0.5, 0.5, 1.0]
                    } else {
                        [0.005, 0.005, 0.005, 1.0]
                    },
                });
            }
        }

        let vertex_buffer =
            glium::VertexBuffer::new(display, &vertices).expect("Failed to create vertex buffer");
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        let x_scale = self.scale * 2.0 / frame_w as f32;
        let y_scale = self.scale * 2.0 / frame_h as f32;
        let x_offset = -self.x * x_scale;
        let y_offset = -self.y * y_scale;
        let border_size = if self.scale > 2.0 {
            0.5 / self.scale
        } else {
            0.0
        };

        target
            .draw(
                &vertex_buffer,
                &indices,
                &shaders::cell2d::compile(display),
                &uniform! {
                    matrix: [
                        [x_scale, 0.0, 0.0, 0.0],
                        [0.0, y_scale, 0.0, 0.0],
                        [0.0, 0.0, 1.0, 0.0],
                        [x_offset, y_offset, 0.0, 1.0],
                    ],
                    low_offset: border_size,
                    high_offset: 1.0 - border_size,
                },
                &Default::default(),
            )
            .expect("Failed to draw grid");
    }
}
