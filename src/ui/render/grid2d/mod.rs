use glium::index::PrimitiveType;
use glium::{uniform, Surface as _};
use std::collections::HashMap;
use std::rc::Rc;

mod controller;
mod shaders;
mod viewport;
mod zoom;

use crate::automaton::space::*;
use crate::ui::gridview::*;
pub use viewport::Viewport2D;
use zoom::Zoom2D;

/// A vertex containing just a cell state.
#[derive(Debug, Default, Copy, Clone)]
struct CellVertex {
    state: u32,
}
glium::implement_vertex!(CellVertex, state);

/// A vertex containing just a floating-point position.
#[derive(Debug, Default, Copy, Clone)]
struct PointVertex {
    pos: [f32; 2],
}
glium::implement_vertex!(PointVertex, pos);

/// The color of the grid. TODO: make this configurable
const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
/// The color for dead cells. TODO: make this configurable
const DEAD_COLOR: [f32; 4] = [0.1, 0.1, 0.1, 1.0];
/// The color for live cells. TODO: make this configurable
const LIVE_COLOR: [f32; 4] = [1.0, 1.0, 1.0, 1.0];

const CHUNK_POW: usize = 6;
/// The number of render cells per chunk.
///
/// See Zoom2D::cells_per_render_cell() for an explanation of render cells.
const CHUNK_SIZE: usize = 1 << CHUNK_POW; // 2^8 = 256

/// The vertex array for a single point, centered on a cell.
const CELL_POINT_VERTS: [PointVertex; 1] = [PointVertex { pos: [0.5, 0.5] }];
/// The vertex array for a rectangle covering a cell.
const CELL_SQUARE_VERTS: [PointVertex; 4] = [
    PointVertex { pos: [0.0, 0.0] },
    PointVertex { pos: [0.0, 1.0] },
    PointVertex { pos: [1.0, 0.0] },
    PointVertex { pos: [1.0, 1.0] },
];

type CellVertexChunk = [CellVertex; CHUNK_SIZE * CHUNK_SIZE];
type CellVertexChunk2D = [[CellVertex; CHUNK_SIZE]; CHUNK_SIZE];
type ChunkCache<C> = HashMap<QuadTreeNode<C>, (CellVertexChunk, bool), NodeHasher>;

pub struct AutomatonView2D {
    pub automaton: QuadTreeAutomaton<bool>,
    pub viewport: Viewport2D,

    cell_chunk_glsl_program: glium::Program,
    gridline_glsl_program: glium::Program,
    cell_chunk_vb: glium::VertexBuffer<CellVertex>,
    cell_point_vb: glium::VertexBuffer<PointVertex>,
    cell_square_vb: glium::VertexBuffer<PointVertex>,

    pub display: Rc<glium::Display>,

    cached_chunks: ChunkCache<bool>,
}
impl AutomatonView2D {
    pub fn new(display: Rc<glium::Display>, automaton: QuadTreeAutomaton<bool>) -> Self {
        Self {
            automaton,
            viewport: Viewport2D::default(),

            cell_chunk_glsl_program: shaders::compile_cell_program(&*display),
            gridline_glsl_program: shaders::compile_lines_program(&*display),
            cell_chunk_vb: glium::VertexBuffer::empty_dynamic(&*display, CHUNK_SIZE * CHUNK_SIZE)
                .expect("Failed to create vertex buffer"),
            cell_point_vb: glium::VertexBuffer::new(&*display, &CELL_POINT_VERTS)
                .expect("Failed to create vertex buffer"),
            cell_square_vb: glium::VertexBuffer::new(&*display, &CELL_SQUARE_VERTS)
                .expect("Failed to create vertex buffer"),

            display,

            cached_chunks: HashMap::default(),
        }
    }

    /// Draw the grid of cells that appear in the viewport.
    ///
    /// This algorithm is based loosely on the one used in Golly.
    pub fn draw(&mut self, target: &mut glium::Frame) {
        // // Clear the screen. (This should not be necessary.)
        // target.clear_color_srgb(1.0, 0.0, 1.0, 1.0);

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
        let global_lower_bound = self.viewport.pos - half_visible_diagonal;
        let global_upper_bound = self.viewport.pos + half_visible_diagonal;
        let cells_per_chunk =
            CHUNK_SIZE as isize * self.viewport.zoom.cells_per_render_cell() as isize;
        // Round bounds to nearest chunk boundary.
        let global_chunk_lower_bound = global_lower_bound.div_euclid(cells_per_chunk);
        let global_chunk_upper_bound = global_upper_bound.div_euclid(cells_per_chunk);
        let global_chunk_visible_rect =
            Rect2D::span(global_chunk_lower_bound, global_chunk_upper_bound);
        let global_visible_rect =
            (global_chunk_visible_rect * cells_per_chunk).offset_min_max(0, cells_per_chunk - 1);

        // Find the smallest quadtree node that will cover the screen, creating
        // a new node by combining four others if need be. (This is the strategy
        // that Golly uses when rendering.)
        let slice = self.automaton.get_slice_containing(global_visible_rect);

        // Offset global_visible_rect and global_chunk_visible_rect to be relative to the
        // slice.
        let chunk_visible_rect =
            global_chunk_visible_rect - slice.get_rect().min().div_euclid(cells_per_chunk);
        let visible_rect = global_visible_rect - slice.get_rect().min();

        // Offset with respect to render cells
        let screen_space_center = slice.get_rect().min() - self.viewport.pos;
        let mut x_offset =
            *screen_space_center.x() as f32 / self.viewport.zoom.cells_per_render_cell();
        let mut y_offset =
            *screen_space_center.y() as f32 / self.viewport.zoom.cells_per_render_cell();
        let pixels_per_render_cell = self.viewport.zoom.pixels_per_render_cell() as f32;
        if let Zoom2D::Close(_) = self.viewport.zoom {
            // Offset within a cell (round to nearest pixel).
            x_offset -= self.viewport.x_offset;
            y_offset -= self.viewport.y_offset;
            x_offset = (x_offset * pixels_per_render_cell).round() / pixels_per_render_cell;
            y_offset = (y_offset * pixels_per_render_cell).round() / pixels_per_render_cell;
        }
        // Multiply by 2 because the OpenGL screen space ranged from -1 to +1.
        let x_scale = pixels_per_render_cell as f32 / pixels_h as f32 * 2.0;
        let y_scale = pixels_per_render_cell as f32 / pixels_v as f32 * 2.0;

        // Convert offset from render-cell-space to screen-space.
        x_offset *= x_scale;
        y_offset *= y_scale;

        let view_matrix = [
            [x_scale, 0.0, 0.0, 0.0],
            [0.0, y_scale, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [x_offset, y_offset, 0.0, 1.0],
        ];

        // Determine how many layers down we need to go for each chunk.
        let slice_layer = slice.get_root().get_layer();
        let layer_at_chunk = self.viewport.zoom.node_layer() + CHUNK_POW;

        self.draw_cell_chunks(
            target,
            view_matrix,
            slice.get_root(),
            Vec2D::origin(),
            slice_layer - layer_at_chunk,
            chunk_visible_rect,
        );
        // Remove elements from the cache that we did not use this frame, and
        // mark the rest as being unused (to prepare for the next frame).
        self.cached_chunks.retain(|_, (_, ref mut keep)| {
            let ret = *keep;
            *keep = false;
            ret
        });

        if self.viewport.zoom.pixels_per_cell() >= 4.0 {
            self.draw_gridlines(&*self.display, target, view_matrix, visible_rect);
        }
    }

    fn draw_cell_chunks(
        &mut self,
        target: &mut glium::Frame,
        view_matrix: [[f32; 4]; 4],
        root_node: QuadTreeNode<bool>,
        root_node_offset: Vec2D,
        layers_remaining: usize,
        chunk_visible_rect: Rect2D,
    ) {
        if layers_remaining == 0 {
            // Populate the VBO for this chunk.
            self.cell_chunk_vb.write(Self::get_chunk_vertices(
                &mut self.cached_chunks,
                &root_node,
            ));

            // Decide whether to draw points or squares.
            let model_vb;
            let model_indices;
            let draw_parameters;
            if self.viewport.zoom.pixels_per_render_cell() > 4 {
                // Render rectangles
                model_vb = &self.cell_square_vb;
                model_indices = glium::index::NoIndices(PrimitiveType::TriangleStrip);
                draw_parameters = Default::default();
            } else {
                // Render points.
                model_vb = &self.cell_point_vb;
                model_indices = glium::index::NoIndices(PrimitiveType::Points);
                draw_parameters = glium::DrawParameters {
                    point_size: Some(self.viewport.zoom.pixels_per_render_cell() as f32),
                    ..Default::default()
                };
            }

            // Convert the chunk position to f32.
            let chunk_x = *root_node_offset.x() as f32;
            let chunk_y = *root_node_offset.y() as f32;

            target
                .draw(
                    (model_vb, self.cell_chunk_vb.per_instance().unwrap()),
                    &model_indices,
                    &self.cell_chunk_glsl_program,
                    &uniform! {
                        matrix: view_matrix,
                        chunk_pos: [chunk_x, chunk_y],
                        chunk_size: CHUNK_SIZE as f32,
                        color1: DEAD_COLOR,
                        color2: LIVE_COLOR,
                        state1: 0u32,
                        state2: 1u32,
                        default_color: DEAD_COLOR,
                    },
                    &draw_parameters,
                )
                .expect("Failed to draw cell chunk");
        } else {
            for branch_idx in 0..4 {
                if let QuadTreeBranch::Node(child_node) = root_node.get_branch(branch_idx) {
                    let branch_offset = ndtree_branch_offset(layers_remaining, branch_idx);
                    let child_node_offset = root_node_offset + branch_offset;
                    let child_node_rect = Rect2D::span(
                        child_node_offset,
                        child_node_offset + (1 << layers_remaining) - 1,
                    );
                    if !chunk_visible_rect.intersects(child_node_rect) {
                        continue;
                    }
                    self.draw_cell_chunks(
                        target,
                        view_matrix,
                        child_node,
                        child_node_offset,
                        layers_remaining - 1,
                        chunk_visible_rect,
                    );
                } else {
                    panic!();
                }
            }
        }
    }

    // Get a Vec of vertices for all the cells in the given chunk, making it if
    // necessary.
    fn get_chunk_vertices<'a>(
        cached_chunks: &'a mut ChunkCache<bool>,
        node: &QuadTreeNode<bool>,
    ) -> &'a CellVertexChunk {
        &cached_chunks
            .entry(node.clone())
            .and_modify(|(_, ref mut keep)| {
                *keep = true;
            })
            .or_insert_with(|| {
                let mut vertices = [[CellVertex::default(); CHUNK_SIZE]; CHUNK_SIZE];
                Self::add_node_vertices(&mut vertices, node, Vec2D::origin(), CHUNK_POW);
                // Adding cells is easier with a 2D array, but the price of
                // convenience if unsafety; we must use std::mem::transmute() to
                // flatten it before putting it in a vertex buffer.
                let flattened_vertices = unsafe { std::mem::transmute(vertices) };
                (flattened_vertices, true)
            })
            .0
    }

    /// Make a CellVertex object for each cell (or pixel-level sub-node) in the
    /// given node.
    fn add_node_vertices(
        vertices: &mut CellVertexChunk2D,
        node: &QuadTreeNode<bool>,
        pos_in_chunk: Vec2D,
        layers_remaining: usize,
    ) {
        // Skip this node if there are no cells in it.
        if node.get_population() == 0 {
            return;
        }
        for branch_idx in 0..4 {
            let branch_offset = ndtree_branch_offset(layers_remaining, branch_idx);
            let branch_pos = pos_in_chunk + branch_offset;
            let branch = node.get_branch(branch_idx);
            match &branch {
                QuadTreeBranch::Leaf(_) => {
                    Self::add_vertex(vertices, branch_pos, branch);
                }
                QuadTreeBranch::Node(child_node) => {
                    if child_node.get_population() == 0 {
                        continue;
                    }
                    if layers_remaining == 1 {
                        Self::add_vertex(vertices, branch_pos, branch);
                    } else {
                        Self::add_node_vertices(
                            vertices,
                            child_node,
                            branch_pos,
                            layers_remaining - 1,
                        );
                    }
                }
            }
        }
    }

    fn add_vertex(vertices: &mut CellVertexChunk2D, pos: Vec2D, branch: QuadTreeBranch<bool>) {
        let live;
        match branch {
            QuadTreeBranch::Leaf(cell_state) => live = cell_state,
            QuadTreeBranch::Node(node) => live = node.get_population() != 0,
        };
        let state = if live { 1 } else { 0 };
        let vertex = CellVertex { state };
        let cell_x = *pos.x() as usize;
        let cell_y = *pos.y() as usize;
        vertices[cell_y][cell_x] = vertex;
    }

    fn draw_gridlines(
        &self,
        display: &glium::Display,
        target: &mut glium::Frame,
        view_matrix: [[f32; 4]; 4],
        visible_rect: Rect2D,
    ) {
        // Make the VBO and indices for the gridlines.
        let gridline_vertices = self.make_gridline_vertices(visible_rect);
        let gridlines_vb = glium::VertexBuffer::new(display, &gridline_vertices)
            .expect("Failed to create vertex buffer");
        let gridlines_indices = glium::index::NoIndices(PrimitiveType::LinesList);

        // Draw gridlines.
        target
            .draw(
                &gridlines_vb,
                &gridlines_indices,
                &self.gridline_glsl_program,
                &uniform! {
                    matrix: view_matrix,
                    lines_color: GRID_COLOR,
                },
                &glium::DrawParameters {
                    line_width: Some(1.0),
                    ..Default::default()
                },
            )
            .expect("Failed to draw gridlines");
    }

    fn make_gridline_vertices(&self, visible_rect: Rect2D) -> Vec<PointVertex> {
        let mut ret =
            Vec::with_capacity((visible_rect.len(Axis::X) + visible_rect.len(Axis::Y)) * 2);
        let min = visible_rect.min();
        let max = visible_rect.max();
        for x in visible_rect.axis_range(Axis::X) {
            // if x.rem_euclid(CHUNK_SIZE as isize) != 0 {
            //     continue;
            // }
            ret.push(PointVertex {
                pos: [x as f32, *min.y() as f32],
            });
            ret.push(PointVertex {
                pos: [x as f32, *max.y() as f32],
            });
        }
        for y in visible_rect.axis_range(Axis::Y) {
            // if y.rem_euclid(CHUNK_SIZE as isize) != 0 {
            //     continue;
            // }
            ret.push(PointVertex {
                pos: [*min.x() as f32, y as f32],
            });
            ret.push(PointVertex {
                pos: [*max.x() as f32, y as f32],
            });
        }
        ret
    }
}
