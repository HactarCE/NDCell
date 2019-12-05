use glium::{uniform, Surface as _};
use std::collections::HashMap;

mod controller;
mod viewport;
mod zoom;

use super::shaders;
use crate::automaton::space::*;
use crate::ui::gridview::*;
pub use viewport::Viewport2D;
use zoom::Zoom2D;

#[derive(Debug, Default, Copy, Clone)]
struct CellVertex {
    proportion_live: f32,
}
glium::implement_vertex!(CellVertex, proportion_live);

#[derive(Debug, Default, Copy, Clone)]
struct GridlineVertex {
    position: [f32; 2],
}
glium::implement_vertex!(GridlineVertex, position);

const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
const DEAD_COLOR: [f32; 4] = [0.1, 0.1, 0.1, 1.0];
const LIVE_COLOR: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
const CHUNK_POW: usize = 6;
const CHUNK_SIZE: usize = 1 << CHUNK_POW; // 2^6 = 64

type CellVertexChunk = [CellVertex; CHUNK_SIZE * CHUNK_SIZE];
type ChunkCache<C> = HashMap<QuadTreeNode<C>, CellVertexChunk, NodeHasher>;

pub struct AutomatonView2D {
    pub automaton: QuadTreeAutomaton<bool>,
    pub viewport: Viewport2D,
    cached_chunks: ChunkCache<bool>,
}
impl AutomatonView2D {
    pub fn new(automaton: QuadTreeAutomaton<bool>) -> Self {
        Self {
            automaton,
            viewport: Viewport2D::default(),
            cached_chunks: HashMap::default(),
        }
    }

    pub fn draw(&mut self, display: &glium::Display, target: &mut glium::Frame) {
        target.clear_color_srgb(DEAD_COLOR[0], DEAD_COLOR[1], DEAD_COLOR[2], DEAD_COLOR[3]);
        self.draw_grid(display, target);
    }

    /// Draw the grid of cells that appear in the viewport.
    ///
    /// This algorithm is based loosely on the one used in Golly.
    fn draw_grid(&mut self, display: &glium::Display, target: &mut glium::Frame) {
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
        // Round bounds to nearest chunk boundary.
        let chunk_lower_bound = lower_bound / CHUNK_SIZE as isize;
        let chunk_upper_bound = upper_bound / CHUNK_SIZE as isize + 1;
        let mut chunk_visible_rect = Rect2D::span(chunk_lower_bound, chunk_upper_bound);
        let mut visible_rect =
            (chunk_visible_rect * CHUNK_SIZE as isize).offset_min_max(0, CHUNK_SIZE as isize - 1);

        // Find the smallest quadtree node that will cover the screen, creating
        // a new node by combining four others if need be. (This is the strategy
        // that Golly uses when rendering.)
        let slice = self.automaton.get_slice_containing(visible_rect);

        // Offset visible_rect and chunk_visible_rect to be relative to the
        // slice.
        chunk_visible_rect = chunk_visible_rect - slice.get_rect().min() / CHUNK_SIZE as isize;
        visible_rect = visible_rect - slice.get_rect().min();

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

        // Convert offset from cell-space to screen-space.
        x_offset *= x_scale;
        y_offset *= y_scale;

        let view_matrix = [
            [x_scale, 0.0, 0.0, 0.0],
            [0.0, y_scale, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [x_offset, y_offset, 0.0, 1.0],
        ];

        let mut new_cache = ChunkCache::default();
        let mut cells_vertex_buffer =
            glium::VertexBuffer::empty_dynamic(display, CHUNK_SIZE * CHUNK_SIZE)
                .expect("Failed to create vertex buffer");
        let cells_indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);
        self.draw_cell_chunks(
            &mut new_cache,
            target,
            &mut cells_vertex_buffer,
            &cells_indices,
            &shaders::cell_chunk_2d::compile(display),
            view_matrix,
            slice.get_root(),
            Vec2D::origin(),
            slice.get_root().get_layer() - self.viewport.zoom.node_layer() - CHUNK_POW,
            chunk_visible_rect,
        );
        self.cached_chunks = new_cache;

        // if self.viewport.zoom.pixels_per_cell() >= 4.0 {
        self.draw_gridlines(display, target, view_matrix, visible_rect);
        // }
    }

    fn draw_cell_chunks(
        &mut self,
        new_cache: &mut ChunkCache<bool>,
        target: &mut glium::Frame,
        vertex_buffer: &mut glium::VertexBuffer<CellVertex>,
        indices: &glium::index::NoIndices,
        shader: &glium::Program,
        view_matrix: [[f32; 4]; 4],
        root_node: QuadTreeNode<bool>,
        root_node_offset: Vec2D,
        layers_remaining: usize,
        chunk_visible_rect: Rect2D,
    ) {
        for branch_idx in 0..4 {
            if let QuadTreeBranch::Node(child_node) = root_node.get_branch(branch_idx) {
                if child_node.get_population() == 0 {
                    continue;
                }
                let child_node_offset =
                    root_node_offset + ndtree_branch_offset(layers_remaining, branch_idx);
                if !chunk_visible_rect.contains(child_node_offset) {
                    continue;
                }
                if layers_remaining == 1 {
                    // Make the VBO for this chunk.
                    let cell_vertices = self.get_chunk_vertices(new_cache, &child_node);
                    vertex_buffer.write(cell_vertices);
                    // Draw cells in the chunk.
                    let x = *child_node_offset.x() as f32;
                    let y = *child_node_offset.y() as f32;
                    target
                        .draw(
                            &*vertex_buffer,
                            &*indices,
                            &shader,
                            &uniform! {
                                matrix: view_matrix,
                                chunk_pos: [x, y],
                                chunk_size: CHUNK_SIZE as f32,
                                color_dead: DEAD_COLOR,
                                color_live: LIVE_COLOR,
                            },
                            &Default::default(),
                        )
                        .expect("Failed to draw cell chunk");
                } else {
                    self.draw_cell_chunks(
                        new_cache,
                        target,
                        vertex_buffer,
                        indices,
                        shader,
                        view_matrix,
                        child_node,
                        child_node_offset,
                        layers_remaining - 1,
                        chunk_visible_rect,
                    );
                }
            } else {
                panic!();
            }
        }
    }

    // Get a Vec of vertices for all the cells in the given chunk, making it if
    // necessary.
    fn get_chunk_vertices<'a>(
        &mut self,
        new_cache: &'a mut ChunkCache<bool>,
        node: &QuadTreeNode<bool>,
    ) -> &'a CellVertexChunk {
        if let Some((owned_node, vertices)) = self.cached_chunks.remove_entry(node) {
            new_cache.insert(owned_node, vertices);
        } else if !new_cache.contains_key(node) {
            let mut vertices = [CellVertex::default(); CHUNK_SIZE * CHUNK_SIZE];
            self.add_node_vertices(&mut vertices, node, Vec2D::origin(), CHUNK_POW);
            new_cache.insert(node.clone(), vertices);
        }
        &new_cache[node]
    }

    /// Make a CellVertex object for each cell (or pixel-level sub-node) in the
    /// given node.
    fn add_node_vertices(
        &self,
        vertices: &mut CellVertexChunk,
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
            let mut maybe_vertex = None;
            match node.get_branch(branch_idx) {
                QuadTreeBranch::Leaf(cell_state) => {
                    maybe_vertex = Some(self.make_vertex(if cell_state { 1.0 } else { 0.0 }));
                }
                QuadTreeBranch::Node(child_node) => {
                    if child_node.get_population() == 0 {
                        continue;
                    }
                    if layers_remaining == 0 {
                        maybe_vertex = Some(self.make_node_vertex(&child_node));
                    } else {
                        self.add_node_vertices(
                            vertices,
                            &child_node,
                            branch_pos,
                            layers_remaining - 1,
                        );
                    }
                }
            }
            if let Some(vertex) = maybe_vertex {
                let y = *branch_pos.y() as usize;
                let x = *branch_pos.x() as usize;
                vertices[y * CHUNK_SIZE + x] = vertex;
            }
        }
    }

    fn make_node_vertex(&self, node: &QuadTreeNode<bool>) -> CellVertex {
        let live_cells = node.get_population() as f32;
        let total_cells = 2.0f32.powf(node.get_layer() as f32).powf(2.0);
        let proportion_live = live_cells / total_cells;
        self.make_vertex(proportion_live)
    }

    fn make_vertex(&self, proportion_live: f32) -> CellVertex {
        CellVertex { proportion_live }
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
        let gridlines_vertex_buffer = glium::VertexBuffer::new(display, &gridline_vertices)
            .expect("Failed to create vertex buffer");
        let gridlines_indices = glium::index::NoIndices(glium::index::PrimitiveType::LinesList);

        // Draw gridlines.
        target
            .draw(
                &gridlines_vertex_buffer,
                &gridlines_indices,
                &shaders::gridlines_2d::compile(display),
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

    fn make_gridline_vertices(&self, visible_rect: Rect2D) -> Vec<GridlineVertex> {
        let mut ret =
            Vec::with_capacity((visible_rect.len(Axis::X) + visible_rect.len(Axis::Y)) * 2);
        let min = visible_rect.min();
        let max = visible_rect.max();
        for x in visible_rect.axis_range(Axis::X) {
            if x.rem_euclid(CHUNK_SIZE as isize) != 0 {
                continue;
            }
            ret.push(GridlineVertex {
                position: [x as f32, *min.y() as f32],
            });
            ret.push(GridlineVertex {
                position: [x as f32, *max.y() as f32],
            });
        }
        for y in visible_rect.axis_range(Axis::Y) {
            if y.rem_euclid(CHUNK_SIZE as isize) != 0 {
                continue;
            }
            ret.push(GridlineVertex {
                position: [*min.x() as f32, y as f32],
            });
            ret.push(GridlineVertex {
                position: [*max.x() as f32, y as f32],
            });
        }
        ret
    }
}
