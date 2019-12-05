use glium::*;

mod controller;
mod viewport;
mod zoom;

use super::shaders;
use crate::automaton::space::{Axis, CanContain, Dim2D, NdTreeNode, Rect2D, Vec2D, VecXY};
use crate::ui::gridview::*;
pub use viewport::Viewport2D;
use zoom::Zoom2D;

#[derive(Debug, Copy, Clone)]
struct CellVertex {
    position: [f32; 2],
    proportion_live: f32,
}
implement_vertex!(CellVertex, position, proportion_live);

#[derive(Debug, Copy, Clone)]
struct GridlineVertex {
    position: [f32; 2],
}
implement_vertex!(GridlineVertex, position);

const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
const DEAD_COLOR: [f32; 4] = [0.1, 0.1, 0.1, 1.0];
const LIVE_COLOR: [f32; 4] = [1.0, 1.0, 1.0, 1.0];

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
        target.clear_color_srgb(DEAD_COLOR[0], DEAD_COLOR[1], DEAD_COLOR[2], DEAD_COLOR[3]);
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

        // Compute the rectangle of the slice that is visible.
        let screen_space_visible_rect = visible_rect - slice.get_rect().min();
        // (Include some wiggle room on the edges.)
        let screen_space_visible_rect = Rect2D::span(
            screen_space_visible_rect.min() - 1,
            screen_space_visible_rect.max() + 1,
        );

        // Cell-space offset
        let screen_space_center = slice.get_rect().min() - self.viewport.pos;
        let mut x_offset = *screen_space_center.x() as f32;
        let mut y_offset = *screen_space_center.y() as f32;
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

        let view_matrix = [
            [x_scale, 0.0, 0.0, 0.0],
            [0.0, y_scale, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [x_offset, y_offset, 0.0, 1.0],
        ];

        // Make the VBO and indices for the cells.
        let mut cell_vertices = Vec::with_capacity(screen_space_visible_rect.count());
        self.make_node_vertices(
            &mut cell_vertices,
            &slice.get_root(),
            Vec2D::origin(),
            Some(screen_space_visible_rect),
            slice.get_root().get_layer() - self.viewport.zoom.node_layer(),
        );
        let cells_vertex_buffer = glium::VertexBuffer::new(display, &cell_vertices)
            .expect("Failed to create vertex buffer");
        let cells_indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        // Draw cells.
        target
            .draw(
                &cells_vertex_buffer,
                &cells_indices,
                &shaders::cell2d::compile(display),
                &uniform! {
                    matrix: view_matrix,
                    low_offset: border_size,
                    high_offset: 1.0 - border_size,
                    color_dead: DEAD_COLOR,
                    color_live: LIVE_COLOR,
                },
                &Default::default(),
            )
            .expect("Failed to draw cells");

        // Make the VBO and indices for the gridlines.
        let mut gridline_vertices = Vec::with_capacity(
            (screen_space_visible_rect.len(Axis::X) + screen_space_visible_rect.len(Axis::Y)) * 2,
        );
        for x in screen_space_visible_rect.axis_range(Axis::X) {
            gridline_vertices.push(GridlineVertex {
                position: [x as f32, *screen_space_visible_rect.min().y() as f32],
            });
            gridline_vertices.push(GridlineVertex {
                position: [x as f32, *screen_space_visible_rect.max().y() as f32],
            });
        }
        for y in screen_space_visible_rect.axis_range(Axis::Y) {
            gridline_vertices.push(GridlineVertex {
                position: [*screen_space_visible_rect.min().x() as f32, y as f32],
            });
            gridline_vertices.push(GridlineVertex {
                position: [*screen_space_visible_rect.max().x() as f32, y as f32],
            });
        }
        let gridlines_vertex_buffer = glium::VertexBuffer::new(display, &gridline_vertices)
            .expect("Failed to create vertex buffer");
        let gridlines_indices = glium::index::NoIndices(glium::index::PrimitiveType::LinesList);

        // Draw gridlines.
        target
            .draw(
                &gridlines_vertex_buffer,
                &gridlines_indices,
                &shaders::lines2d::compile(display),
                &uniform! {
                    matrix: view_matrix,
                    grid_color: GRID_COLOR,
                },
                &glium::DrawParameters {
                    line_width: Some(1.0),
                    ..Default::default()
                },
            )
            .expect("Failed to draw gridlines");
    }

    /// Make a CellVertex object for each cell (or pixel-level sub-node) in the
    /// given node.
    fn make_node_vertices(
        &self,
        vertices: &mut Vec<CellVertex>,
        node: &QuadTreeNode<bool>,
        min_pos: Vec2D,
        visible_rect: Option<Rect2D>,
        layers_remaining: usize,
    ) {
        let layer = node.get_layer();
        for branch_idx in 0..4 {
            let branch_offset =
                NdTreeNode::<bool, Dim2D>::branch_offset_at_layer(layers_remaining, branch_idx);
            let branch_min_pos = min_pos + branch_offset;
            let mut next_visible_rect = None;
            if let Some(r) = visible_rect {
                let branch_max_pos = branch_min_pos + (1 << (layer - 1)) - 1;
                let branch_rect = Rect2D::span(branch_min_pos, branch_max_pos);
                if !r.intersects(branch_rect) {
                    continue;
                }
                if !r.contains(branch_rect) {
                    next_visible_rect = visible_rect;
                }
            }
            match node.get_branch(branch_idx) {
                QuadTreeBranch::Leaf(cell_state) => vertices.push(self.make_vertex(
                    if cell_state { 1.0 } else { 0.0 },
                    [*branch_min_pos.x() as f32, *branch_min_pos.y() as f32],
                )),
                QuadTreeBranch::Node(child_node) => {
                    // if child_node.get_population() == 0 {
                    //     continue;
                    // }
                    if layers_remaining == 0 {
                        vertices.push(self.make_node_vertex(
                            &child_node,
                            [*branch_min_pos.x() as f32, *branch_min_pos.y() as f32],
                        ));
                    } else {
                        self.make_node_vertices(
                            vertices,
                            &child_node,
                            branch_min_pos,
                            next_visible_rect,
                            layers_remaining - 1,
                        );
                    }
                }
            }
        }
    }

    fn make_node_vertex(&self, node: &QuadTreeNode<bool>, position: [f32; 2]) -> CellVertex {
        let live_cells = node.get_population() as f32;
        let total_cells = 2.0f32.powf(node.get_layer() as f32).powf(2.0);
        let proportion_live = live_cells / total_cells;
        self.make_vertex(proportion_live, position)
    }

    fn make_vertex(&self, proportion_live: f32, position: [f32; 2]) -> CellVertex {
        CellVertex {
            position,
            proportion_live,
        }
    }
}
