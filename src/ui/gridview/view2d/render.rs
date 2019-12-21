use super::*;
use glium::index::PrimitiveType;
use glium::{uniform, Surface as _};
use std::collections::HashMap;

use crate::automaton::space::*;
pub use viewport::Viewport2D;

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
impl<T> From<T> for PointVertex
where
    Vec2D: From<T>,
{
    fn from(pos: T) -> Self {
        let vec = Vec2D::from(pos);
        Self {
            pos: [*vec.x() as f32, *vec.y() as f32],
        }
    }
}

/// The color of the grid. TODO: make this configurable
const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
/// The color for dead cells. TODO: make this configurable
const DEAD_COLOR: [f32; 4] = [0.1, 0.1, 0.1, 1.0];
/// The color for live cells. TODO: make this configurable
const LIVE_COLOR: [f32; 4] = [1.0, 1.0, 1.0, 1.0];

const CHUNK_POW: usize = 6;
/// The length of render cells per chunk; a render cell is a square of cells
/// that is rendered as one unit.
///
/// When zoomed in, each render cell is the same as a normal cell. When
/// zoomed out beyond 1:1, each render cell is a single pixel, which can
/// contain a large square of cells. For example, at zoom level 8:1, this
/// method returns `8`.
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

const GRIDLINE_BATCH_SIZE: usize = 256;

type CellVertexChunk = [CellVertex; CHUNK_SIZE * CHUNK_SIZE];
type CellVertexChunk2D = [[CellVertex; CHUNK_SIZE]; CHUNK_SIZE];
type ChunkCache<C> = HashMap<NdCachedNode<C, Dim2D>, (CellVertexChunk, bool), NodeHasher>;

#[derive(Default)]
pub(super) struct RenderCache {
    chunks: ChunkCache<u8>,
}

pub(super) struct Shaders {
    cell_chunk: glium::Program,
    gridlines: glium::Program,
}
impl Shaders {
    pub fn compile(display: &glium::Display) -> Self {
        Self {
            cell_chunk: shaders::compile_cell_program(display),
            gridlines: shaders::compile_lines_program(display),
        }
    }
}

pub(super) struct VBOs {
    cell_chunk: glium::VertexBuffer<CellVertex>,
    cell_point: glium::VertexBuffer<PointVertex>,
    cell_square: glium::VertexBuffer<PointVertex>,
    gridlines: glium::VertexBuffer<PointVertex>,
}
impl VBOs {
    pub fn new(display: &glium::Display) -> Self {
        Self {
            cell_chunk: glium::VertexBuffer::empty_dynamic(display, CHUNK_SIZE * CHUNK_SIZE)
                .expect("Failed to create vertex buffer"),
            cell_point: glium::VertexBuffer::new(display, &CELL_POINT_VERTS)
                .expect("Failed to create vertex buffer"),
            cell_square: glium::VertexBuffer::new(display, &CELL_SQUARE_VERTS)
                .expect("Failed to create vertex buffer"),
            gridlines: glium::VertexBuffer::empty_dynamic(display, GRIDLINE_BATCH_SIZE)
                .expect("Failed to create vertex buffer"),
        }
    }
}

#[derive(Default)]
pub(super) struct Textures {
    cells: Option<glium::framebuffer::RenderBuffer>,
}
impl Textures {
    pub fn new(_display: &glium::Display) -> Self {
        Self::default()
    }
}

pub fn draw(grid_view: &mut GridView2D, target: &mut glium::Frame) {
    let mut rip = RenderInProgress::new(grid_view, target);
    rip.draw_cells();
    rip.draw_gridlines();
}

pub(super) struct RenderInProgress<'a> {
    g: &'a mut GridView2D,
    target: &'a mut glium::Frame,
    cell_render_dimensions: (u32, u32),
    render_cell_layer: usize,
    render_cell_pixels: usize,
    view_matrix: [[f32; 4]; 4],
    slice: NdTreeSlice<u8, Dim2D>,
    visible_rect: Rect2D,
    chunk_visible_rect: Rect2D,
}
impl<'a> RenderInProgress<'a> {
    pub fn new(g: &'a mut GridView2D, target: &'a mut glium::Frame) -> Self {
        // Compute the dimensions of the renderbuffer on which the cells will be
        // drawn. At integer scale powers (i.e. scale factors that are a power
        // of 2) we render at the same resolution as the screen; at any other
        // scale, we render at a higher resolution and downscale the result.
        let effective_zoom = g.viewport.zoom.floor();
        let pixels_h: u32;
        let pixels_v: u32;
        {
            let (target_h, target_v) = target.get_dimensions();
            let mut scale_factor = effective_zoom / g.viewport.zoom;
            pixels_h = (target_h as f32 * scale_factor) as u32;
            pixels_v = (target_v as f32 * scale_factor) as u32;
        }

        // Compute the lowest layer that must be visited, which is the layer of
        // a "render cell," a node that is rendered as one unit.
        let render_cell_layer: usize = std::cmp::max(0, -effective_zoom.power() as isize) as usize;
        // Compute the width of cells represented by each "render cell."
        let render_cell_cells: usize = 1 << render_cell_layer;
        // Compute the width of pixels for each "render cell."
        let render_cell_pixels: usize = effective_zoom.pixels_per_cell().ceil() as usize;

        let cell_chunk_size: isize = CHUNK_SIZE as isize * render_cell_cells as isize;

        // Compute the rectangular area of the whole grid that is visible.
        let mut global_visible_rect: Rect2D;
        {
            // Find the number of cells that fit horizontally (cells_h) and
            // vertically (cells_v) across the screen.
            let pixels_per_cell = effective_zoom.pixels_per_cell();
            let cells_h = pixels_h as f32 / pixels_per_cell;
            let cells_v = pixels_v as f32 / pixels_per_cell;

            // Find the lower and upper bounds on the cell positions that need to be
            // displayed.
            let half_visible_diagonal = Vec2D::from([
                (cells_h / 2.0).ceil() as isize,
                (cells_v / 2.0).ceil() as isize,
            ]);
            let lower_bound = g.viewport.pos - half_visible_diagonal;
            let upper_bound = g.viewport.pos + half_visible_diagonal;
            global_visible_rect = Rect2D::span(lower_bound, upper_bound);
        }

        let global_chunk_visible_rect: Rect2D;
        {
            // Convert that rectangle into chunk coordinates, taking the smallest
            // rectangle of chunks that contains everything visible.
            let lower_chunk_bound = global_visible_rect.min().div_euclid(cell_chunk_size);
            let upper_chunk_bound = global_visible_rect.max().div_euclid(cell_chunk_size);
            global_chunk_visible_rect = Rect2D::span(lower_chunk_bound, upper_chunk_bound);

            // Expand the rectangle of visible cells to encompass all visible chunks.
            let lower_bound = lower_chunk_bound * cell_chunk_size;
            let upper_bound = upper_chunk_bound * cell_chunk_size + (cell_chunk_size - 1);
            global_visible_rect = Rect2D::span(lower_bound, upper_bound);
        }

        // Find the smallest quadtree node that will cover the screen, creating
        // a new node by combining four others if need be. (This is the strategy
        // that Golly uses when rendering.)
        let slice = g
            .automaton
            .get_projected_tree()
            .get_slice_containing(global_visible_rect);

        // Offset global_visible_rect and global_chunk_visible_rect to be relative to the
        // slice.
        let chunk_visible_rect =
            global_chunk_visible_rect - slice.rect().min().div_euclid(cell_chunk_size);
        let visible_rect = global_visible_rect - slice.rect().min();

        // Compute offset with respect to render cells.
        let screen_space_center = slice.rect().min() - g.viewport.pos;
        let mut x_offset: f32 = *screen_space_center.x() as f32 / render_cell_cells as f32;
        let mut y_offset: f32 = *screen_space_center.y() as f32 / render_cell_cells as f32;
        // Comupte offset within a cell (round to nearest pixel).
        x_offset -= g.viewport.x_offset;
        y_offset -= g.viewport.y_offset;
        x_offset = (x_offset * render_cell_pixels as f32).round() / render_cell_pixels as f32;
        y_offset = (y_offset * render_cell_pixels as f32).round() / render_cell_pixels as f32;
        // Multiply by 2 because the OpenGL screen space ranged from -1 to +1.
        let x_scale = render_cell_pixels as f32 / pixels_h as f32 * 2.0;
        let y_scale = render_cell_pixels as f32 / pixels_v as f32 * 2.0;

        // Convert offset from render-cell-space to screen-space.
        x_offset *= x_scale;
        y_offset *= y_scale;

        let view_matrix = [
            [x_scale, 0.0, 0.0, 0.0],
            [0.0, y_scale, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [x_offset, y_offset, 0.0, 1.0],
        ];

        Self {
            g,
            target,
            cell_render_dimensions: (pixels_h, pixels_v),
            render_cell_layer,
            render_cell_pixels,
            view_matrix,
            slice,
            visible_rect,
            chunk_visible_rect,
        }
    }

    /// Draw the cells that appear in the viewport.
    pub fn draw_cells(&mut self) {
        let (pixels_h, pixels_v) = self.cell_render_dimensions;
        let cells_texture = glium::framebuffer::RenderBuffer::new(
            &*self.g.display,
            glium::texture::UncompressedFloatFormat::U8U8U8,
            pixels_h,
            pixels_v,
        )
        .expect("Failed to create render buffer");
        let mut cells_fbo =
            glium::framebuffer::SimpleFrameBuffer::new(&*self.g.display, &cells_texture)
                .expect("Failed to create frame buffer");

        // Clear the screen. (TODO: This should not be necessary)
        self.target.clear_color_srgb(1.0, 0.0, 1.0, 1.0);

        // Determine how many layers down we need to go for each chunk.
        let slice_layer = self.slice.root.layer;
        let layer_at_chunk = self.render_cell_layer + CHUNK_POW;

        // Draw to the frame buffer.
        self.draw_cell_chunks(
            &mut cells_fbo,
            &self.slice.root.clone(),
            Vec2D::origin(),
            slice_layer - layer_at_chunk,
        );

        // Blit the frame buffer to the display.
        {
            let source_rect = glium::Rect {
                left: 0,
                bottom: 0,
                width: pixels_h,
                height: pixels_v,
            };
            let (target_h, target_v) = self.target.get_dimensions();
            let destination_rect = glium::BlitTarget {
                left: 0,
                bottom: 0,
                width: target_h as i32,
                height: target_v as i32,
            };
            self.target.blit_from_simple_framebuffer(
                &cells_fbo,
                &source_rect,
                &destination_rect,
                glium::uniforms::MagnifySamplerFilter::Nearest,
            );
        }

        // Remove elements from the cache that we did not use this frame, and
        // mark the rest as being unused (to prepare for the next frame).
        self.g.render_cache.chunks.retain(|_, (_, ref mut keep)| {
            let ret = *keep;
            *keep = false;
            ret
        });
    }
    fn draw_cell_chunks<T: glium::Surface>(
        &mut self,
        target: &mut T,
        root_node: &NdCachedNode<u8, Dim2D>,
        root_node_offset: Vec2D,
        layers_remaining: usize,
    ) {
        if layers_remaining == 0 {
            // Populate the VBO for this chunk.
            self.g.vbos.cell_chunk.write(Self::get_chunk_vertices(
                &mut self.g.render_cache,
                root_node,
            ));

            // Decide whether to draw points or squares.
            let model_indices;
            let model_vb;
            let draw_parameters;
            if self.render_cell_pixels > 4 {
                // Render rectangles
                model_indices = glium::index::NoIndices(PrimitiveType::TriangleStrip);
                model_vb = &self.g.vbos.cell_square;
                draw_parameters = Default::default();
            } else {
                // Render points.
                model_indices = glium::index::NoIndices(PrimitiveType::Points);
                model_vb = &self.g.vbos.cell_point;
                draw_parameters = glium::DrawParameters {
                    point_size: Some(self.render_cell_pixels as f32),
                    ..Default::default()
                };
            }

            // Convert the chunk position to f32.
            let chunk_x = *root_node_offset.x() as f32;
            let chunk_y = *root_node_offset.y() as f32;

            target
                .draw(
                    (model_vb, self.g.vbos.cell_chunk.per_instance().unwrap()),
                    &model_indices,
                    &self.g.shaders.cell_chunk,
                    &uniform! {
                        matrix: self.view_matrix,
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
                if let NdTreeBranch::Node(child_node) = &root_node.branches[branch_idx] {
                    let branch_offset = ndtree_branch_offset(layers_remaining, branch_idx);
                    let child_node_offset = root_node_offset + branch_offset;
                    let child_node_rect = Rect2D::span(
                        child_node_offset,
                        child_node_offset + (1 << layers_remaining) - 1,
                    );
                    self.draw_cell_chunks(
                        target,
                        &child_node,
                        child_node_offset,
                        layers_remaining - 1,
                    );
                } else {
                    panic!();
                }
            }
        }
    }
    // Get a Vec of vertices for all the cells in the given chunk, creating a
    // new one if necessary.
    fn get_chunk_vertices<'b>(
        render_cache: &'b mut RenderCache,
        node: &NdCachedNode<u8, Dim2D>,
    ) -> &'b CellVertexChunk {
        &render_cache
            .chunks
            .entry(node.clone())
            .and_modify(|(_, ref mut keep)| {
                *keep = true;
            })
            .or_insert_with(|| {
                let mut vertices: CellVertexChunk2D =
                    [[CellVertex::default(); CHUNK_SIZE]; CHUNK_SIZE];
                Self::add_node_vertices(&mut vertices, node, Vec2D::origin(), CHUNK_POW);
                // Adding cells is easier with a 2D array, but the price of
                // convenience if unsafety; we must use std::mem::transmute() to
                // flatten it before putting it in a vertex buffer.
                let flattened_vertices: CellVertexChunk = unsafe { std::mem::transmute(vertices) };
                (flattened_vertices, true)
            })
            .0
    }
    /// Make a CellVertex for each cell (or pixel-level sub-node) in the
    /// given node.
    fn add_node_vertices(
        vertices: &mut CellVertexChunk2D,
        node: &NdCachedNode<u8, Dim2D>,
        pos_in_chunk: Vec2D,
        layers_remaining: usize,
    ) {
        // Skip this node if there are no cells in it.
        if node.population == 0 {
            return;
        }
        for branch_idx in 0..4 {
            let branch_offset = ndtree_branch_offset(layers_remaining, branch_idx);
            let branch_pos = pos_in_chunk + branch_offset;
            let branch = &node.branches[branch_idx];
            match &branch {
                NdTreeBranch::Leaf(_) => {
                    Self::add_vertex(vertices, branch_pos, branch);
                }
                NdTreeBranch::Node(child_node) => {
                    if child_node.population == 0 {
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
    /// Make a CellVertex for the given pixel-level cell or sub-node.
    fn add_vertex(vertices: &mut CellVertexChunk2D, pos: Vec2D, branch: &NdTreeBranch<u8, Dim2D>) {
        let state;
        match branch {
            NdTreeBranch::Leaf(cell_state) => state = *cell_state,
            NdTreeBranch::Node(node) => state = if node.population == 0 { 0 } else { 1 },
        };
        let state = state as u32;
        let vertex = CellVertex { state };
        let cell_x = *pos.x() as usize;
        let cell_y = *pos.y() as usize;
        vertices[cell_y][cell_x] = vertex;
    }

    /// Draw the gridlines that appear in the viewport.
    pub fn draw_gridlines(&mut self) {
        // Don't bother drawing gridlines if we're zoomed out far enough.
        if self.g.viewport.zoom.pixels_per_cell() < 4.0 {
            return;
        }
        // Generate a pair of vertices for each gridline.
        let gridlines_indices = glium::index::NoIndices(PrimitiveType::LinesList);
        let mut gridline_vertices = Vec::with_capacity(
            (self.visible_rect.len(Axis::X) + self.visible_rect.len(Axis::Y)) * 2,
        );
        let min = self.visible_rect.min();
        let max = self.visible_rect.max();
        let min_x = *min.x();
        let min_y = *min.y();
        let max_x = *max.x();
        let max_y = *max.y();
        for x in self.visible_rect.axis_range(Axis::X) {
            gridline_vertices.push(PointVertex::from([x, min_y]));
            gridline_vertices.push(PointVertex::from([x, max_y]));
        }
        for y in self.visible_rect.axis_range(Axis::Y) {
            gridline_vertices.push(PointVertex::from([min_x, y]));
            gridline_vertices.push(PointVertex::from([max_x, y]));
        }

        // Draw the gridlines in batches, because the VBO might not be able to
        // hold all the points at once.
        for batch in gridline_vertices.chunks(GRIDLINE_BATCH_SIZE) {
            // Put the data in a slice of the VBO.
            let vbo_slice = self.g.vbos.gridlines.slice(0..batch.len()).unwrap();
            vbo_slice.write(batch);
            // Draw gridlines.
            self.target
                .draw(
                    vbo_slice,
                    &gridlines_indices,
                    &self.g.shaders.gridlines,
                    &uniform! {
                        matrix: self.view_matrix,
                        lines_color: GRID_COLOR,
                    },
                    &glium::DrawParameters {
                        line_width: Some(1.0),
                        ..Default::default()
                    },
                )
                .expect("Failed to draw gridlines");
        }
    }
}
