use glium::*;

mod controller;
mod zoom;

use super::shaders;
use crate::automaton::space::{Axis, CanContain, Rect2D, Vec2D};
use crate::ui::gridview::*;
use zoom::Zoom2D;

#[derive(Copy, Clone)]
pub struct Vertex {
    position: [f32; 2],
    color: [f32; 4],
}
implement_vertex!(Vertex, position, color);

const GRID_COLOR: [f32; 4] = [0.2, 0.2, 0.2, 1.0];
const MIN_COLOR: [f32; 4] = [0.005, 0.005, 0.005, 1.0];
const MAX_COLOR: [f32; 4] = [0.5, 0.5, 0.5, 1.0];

pub struct AutomatonView2D {
    pub automaton: QuadTreeAutomaton<bool>,
    pub x: f32,
    pub y: f32,
    pub zoom: Zoom2D,
}
impl AutomatonView2D {
    pub fn new(automaton: QuadTreeAutomaton<bool>) -> Self {
        Self {
            automaton,
            x: 0.0f32,
            y: 0.0f32,
            zoom: Zoom2D::Close(32),
        }
    }

    fn make_vertex(&self, color_proportion: f32, pos: Vec2D) -> Vertex {
        let mut color = MIN_COLOR;
        for i in 0..4 {
            color[i] += (MAX_COLOR[i] - MIN_COLOR[i]) * color_proportion;
        }
        Vertex {
            position: [pos[Axis::X] as f32, pos[Axis::Y] as f32],
            color,
        }
    }

    fn make_vertices(
        &self,
        vertices: &mut Vec<Vertex>,
        slice_branch: QuadTreeSliceBranch<bool>,
        rect: Rect2D,
    ) {
        match slice_branch {
            QuadTreeSliceBranch::Leaf(cell_state, pos) => {
                if rect.contains(pos) {
                    vertices.push(self.make_vertex(if cell_state { 1.0 } else { 0.0 }, pos));
                }
            }
            QuadTreeSliceBranch::Node(slice) => {
                if !rect.intersects(slice.get_rect()) {
                    return;
                }
                if slice.get_root().get_layer() <= self.zoom.node_layer() {
                    let live_cells = slice.get_root().get_population() as f32;
                    let total_cells = 2.0f32.powf(slice.get_root().get_layer() as f32).powf(2.0);
                    let color_proportion = live_cells / total_cells;
                    vertices.push(self.make_vertex(color_proportion, slice.get_rect().min()));
                } else {
                    for branch in slice.get_branches().into_iter() {
                        self.make_vertices(vertices, branch.clone(), rect);
                    }
                }
            }
        }
    }

    pub fn draw_editor(&mut self, display: &Display, target: &mut glium::Frame) {
        target.clear_color_srgb(GRID_COLOR[0], GRID_COLOR[1], GRID_COLOR[2], GRID_COLOR[3]);

        let (frame_w, frame_h) = target.get_dimensions();
        let frame_w = frame_w / 4;
        let frame_h = frame_h / 4;

        let visible_rect: Rect2D;
        let mut vertices: Vec<Vertex>;
        match self.zoom {
            Zoom2D::Close(pixels_per_cell) => {
                let cell_w = frame_w as f32 / pixels_per_cell as f32;
                let cell_h = frame_h as f32 / pixels_per_cell as f32;
                let left = (self.x - cell_w / 2.0).floor() as isize;
                let right = (self.x + cell_w / 2.0).ceil() as isize;
                let bottom = (self.y - cell_h / 2.0).floor() as isize;
                let top = (self.y + cell_h / 2.0).ceil() as isize;
                visible_rect = Rect2D::span([left, bottom].into(), [right, top].into());
                vertices = Vec::with_capacity(visible_rect.count());
            }
            Zoom2D::Far(_) => {
                let cells_per_pixel = self.zoom.cells_per_pixel() as isize;
                let cell_w = cells_per_pixel * frame_w as isize;
                let cell_h = cells_per_pixel * frame_h as isize;
                let left = self.x as isize - cell_w;
                let right = self.x as isize + cell_w;
                let bottom = self.y as isize - cell_h;
                let top = self.y as isize + cell_h;
                visible_rect = Rect2D::span([left, bottom].into(), [right, top].into());
                vertices = Vec::with_capacity((frame_w * frame_h) as usize);
            }
        };
        self.automaton.expand_to(visible_rect.min());
        self.automaton.expand_to(visible_rect.max());
        for branch in self.automaton.slice().get_branches().into_iter() {
            self.make_vertices(&mut vertices, branch.clone(), visible_rect);
        }
        self.automaton.shrink();

        let vertex_buffer =
            glium::VertexBuffer::new(display, &vertices).expect("Failed to create vertex buffer");
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        let x_scale = self.zoom.pixels_per_cell() as f32 * 2.0 / frame_w as f32;
        let y_scale = self.zoom.pixels_per_cell() as f32 * 2.0 / frame_h as f32;
        let x_offset = -self.x * x_scale;
        let y_offset = -self.y * y_scale;
        let cell_density = match self.zoom {
            Zoom2D::Close(_) => 1.0,
            Zoom2D::Far(n) => 2.0f32.powf(n as f32),
        };
        let border_size = if self.zoom.pixels_per_cell() > 2.0 {
            0.5 / self.zoom.pixels_per_cell()
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
                    cell_density: cell_density,
                },
                &Default::default(),
            )
            .expect("Failed to draw grid");
    }
}
