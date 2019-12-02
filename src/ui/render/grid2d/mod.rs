use glium::*;

mod controller;
mod viewport;
mod zoom;

use super::shaders;
use crate::automaton::space::{Axis, Dim2D, NdTreeNode, Rect2D, Vec2D};
use crate::ui::gridview::*;
pub use viewport::Viewport2D;
use zoom::Zoom2D;

#[derive(Copy, Clone)]
pub struct Vertex {
    position: [f32; 2],
    proportion_live: f32,
}
implement_vertex!(Vertex, position, proportion_live);

const GRID_COLOR: [f32; 4] = [0.2, 0.2, 0.2, 1.0];
const DEAD_COLOR: [f32; 4] = [0.005, 0.005, 0.005, 1.0];
const LIVE_COLOR: [f32; 4] = [0.5, 0.5, 0.5, 1.0];

pub struct AutomatonView2D {
    pub automaton: QuadTreeAutomaton<bool>,
    pub viewport: Viewport2D,
}
impl AutomatonView2D {
    pub fn new(automaton: QuadTreeAutomaton<bool>) -> Self {
        Self {
            automaton,
            viewport: Viewport2D::default(),
        }
    }

    pub fn draw(&mut self, display: &Display, target: &mut glium::Frame) {
        target.clear_color_srgb(GRID_COLOR[0], GRID_COLOR[1], GRID_COLOR[2], GRID_COLOR[3]);
        self.draw_cells(display, target);
    }

    /// Draw the cells that appear in the viewport.
    ///
    /// This algorithm is based loosely on the one used in Golly.
    fn draw_cells(&mut self, display: &Display, target: &mut glium::Frame) {
        // Find the number of cells that fit horizontally (cells_h) and
        // vertically (cells_v) across the screen.
        let pixels_per_cell = self.viewport.zoom.pixels_per_cell();
        let (pixels_h, pixels_v) = target.get_dimensions();
        let cells_h = pixels_h as f32 / pixels_per_cell;
        let cells_v = pixels_v as f32 / pixels_per_cell;

        // Find the lower and upper bounds on the cell positions that need to be
        // displayed.
        let half_visible_diagonal = Vec2D::from([
            (cells_h.ceil() / 2.0) as isize,
            (cells_v.ceil() / 2.0) as isize,
        ]);
        let lower_bound = self.viewport.pos - half_visible_diagonal;
        let upper_bound = self.viewport.pos + half_visible_diagonal;
        let visible_rect = Rect2D::span(lower_bound, upper_bound);

        // Find the smallest quadtree node that will cover the screen, creating
        // a new node by combining four others if need be.
        let slice = self.automaton.get_slice_containing(visible_rect);

        // Make a Vertex object for each cell in the slice.
        let mut vertices = Vec::with_capacity(
            1 << ((slice.get_root().get_layer() - self.viewport.zoom.node_layer()) * 2),
        );
        self.make_node_vertices(
            &mut vertices,
            &slice.get_root(),
            [0, 0],
            self.viewport.zoom.node_layer(),
        );

        let vertex_buffer =
            glium::VertexBuffer::new(display, &vertices).expect("Failed to create vertex buffer");
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        // Cell-space offset
        let screen_space_lower_bound = slice.get_rect().min() - self.viewport.pos;
        let mut x_offset = screen_space_lower_bound[Axis::X] as f32;
        let mut y_offset = screen_space_lower_bound[Axis::Y] as f32;
        // Offset within a cell (round to nearest pixel).
        x_offset -= self.viewport.x_offset;
        y_offset -= self.viewport.y_offset;
        let pix_per_render_cell = self.viewport.zoom.pixels_per_render_cell() as f32 * 2.0;
        x_offset = (x_offset * pix_per_render_cell).round() / pix_per_render_cell;
        y_offset = (y_offset * pix_per_render_cell).round() / pix_per_render_cell;

        // Multiply by 2 because the OpenGL screen space is 2x2.
        let x_scale = pixels_per_cell as f32 / pixels_h as f32 * 2.0;
        let y_scale = pixels_per_cell as f32 / pixels_v as f32 * 2.0;
        let border_size = if pixels_per_cell > 2.0 {
            0.5 / pixels_per_cell
        } else {
            0.0
        };

        // Convert offset from cell-space to screen-space.
        x_offset *= x_scale;
        y_offset *= y_scale;

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
                    color_dead: DEAD_COLOR,
                    color_live: LIVE_COLOR,
                },
                &Default::default(),
            )
            .expect("Failed to draw grid");
    }

    /// Make a Vertex object for each cell (or pixel-level sub-node) in the
    /// given node.
    fn make_node_vertices(
        &self,
        vertices: &mut Vec<Vertex>,
        node: &QuadTreeNode<bool>,
        min_pos: [isize; 2],
        lowest_layer: usize,
    ) {
        for branch_idx in 0..4 {
            let branch_offset = NdTreeNode::<bool, Dim2D>::branch_offset_at_layer(
                node.get_layer() - lowest_layer,
                branch_idx,
            );
            let branch_min_pos = [
                min_pos[0] + branch_offset[Axis::X],
                min_pos[1] + branch_offset[Axis::Y],
            ];
            match node.get_branch(branch_idx) {
                QuadTreeBranch::Leaf(cell_state) => vertices.push(self.make_vertex(
                    if cell_state { 1.0 } else { 0.0 },
                    [branch_min_pos[0] as f32, branch_min_pos[1] as f32],
                )),
                QuadTreeBranch::Node(child_node) => {
                    if child_node.get_layer() <= lowest_layer {
                        vertices.push(self.make_node_vertex(
                            &child_node,
                            [branch_min_pos[0] as f32, branch_min_pos[1] as f32],
                        ));
                    } else {
                        self.make_node_vertices(
                            vertices,
                            &child_node,
                            branch_min_pos,
                            lowest_layer,
                        );
                    }
                }
            }
        }
    }

    fn make_node_vertex(&self, node: &QuadTreeNode<bool>, position: [f32; 2]) -> Vertex {
        let live_cells = node.get_population() as f32;
        let total_cells = 2.0f32.powf(node.get_layer() as f32).powf(2.0);
        let proportion_live = live_cells / total_cells;
        self.make_vertex(proportion_live, position)
    }

    fn make_vertex(&self, proportion_live: f32, position: [f32; 2]) -> Vertex {
        Vertex {
            position,
            proportion_live,
        }
    }
}
