//! The data structures and routines related to rendering 2D square cells.
//!
//! Currently, only solid colors are supported, however I plan to add icons in
//! the future.
//!
//! Not including preliminary computations, there are four main stages to
//! rendering:
//!
//! 1. Read a chunk of cells from quadtree and produce a 2D array of colors,
//!    with one array element for each render cell. (A "render cell" is a node
//!    of the quadtree which is rendered as single unit; this is a single cell
//!    when zoomed in, but may be larger when zoomed out.)
//! 2. Copy that pixel data into the corresponding section of an OpenGL texture,
//!    where each pixel represents one cell.
//! 3. Resize that texture to the next-highest power-of-2 zoom factor using
//!    nearest-neighbor sampling. For example, if the zoom factor is 2.4 (i.e.
//!    each cell takes up a square 2.4 pixels wide), resize the texture by a
//!    factor of 4.
//! 4. Blit that texture onto the screen at the appropriate position using
//!    linear sampling, which accounts for the remaining scaling.
//!
//! We resize the texture twice to get the best of both nearest-neighbor scaling
//! and linear scaling: if only nearest-neighbor were used, cells at non-integer
//! zoom factors would look unbalanced; some cells would be larger and some
//! would be smaller, and many would be slightly rectangular instead of square.
//! This happens in LifeViewer. If only linear scaling were used, then cells
//! would appear blurry when zoomed in. (Anyone who has made pixel art is
//! probably familiar with this issue.) Performing nearest-neighbor scaling for
//! the potentially large integer factor and then linear scaling for the
//! relatively small remaining factor yields a much nicer result that
//! imperceptibly blends between different zoom levels.
//!
//! It's worth noting that translation is always rounded to the nearest pixel,
//! so at any integer zoom factor, cell boundaries are always between pixels.
//! This is a major factor in contributing to a crisp image, and ensures that
//! gridlines need no anti-aliasing (which is very good, since anti-aliasing
//! lines is not at all trivial in OpenGL!). At non-integer zoom factors,
//! however, this is not guaranteed.

use super::*;
use glium::index::PrimitiveType;
use glium::{uniform, Surface as _};
use std::borrow::Cow;
use std::collections::HashMap;

use crate::automaton::space::*;
use crate::math;
pub use viewport::Viewport2D;

/// A vertex containing just a color.
#[derive(Debug, Default, Copy, Clone)]
struct SimpleColorVertex {
    color: (u8, u8, u8),
}
glium::implement_vertex!(SimpleColorVertex, color);

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

/// The color of the grid. This will be configurable in the future.
const GRID_COLOR: [f32; 4] = [0.125, 0.125, 0.125, 1.0];
/// The color for dead cells. This will be configurable in the future.
const DEAD_COLOR: (u8, u8, u8) = (0, 0, 0);
/// The color for live cells. This will be configurable in the future.
const LIVE_COLOR: (u8, u8, u8) = (255, 255, 255);

const CHUNK_POW: usize = 6;
/// The length of render cells per chunk; a render cell is a square of cells
/// that is rendered as one unit.
///
/// When zoomed in, each render cell is the same as a normal cell. When zoomed
/// out beyond 1:1, each render cell is a single pixel, which can contain a
/// large square of cells. A chunk is a groups of render cells that is passed to
/// the graphics card at once.
const CHUNK_SIZE: usize = 1 << CHUNK_POW; // 2^6 = 64

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

#[derive(Clone)]
struct CellPixelChunk([[(u8, u8, u8); CHUNK_SIZE]; CHUNK_SIZE]);
impl Default for CellPixelChunk {
    fn default() -> Self {
        Self([[(0, 0, 0); CHUNK_SIZE]; CHUNK_SIZE])
    }
}
impl<'a> glium::texture::Texture2dDataSource<'a> for &'a CellPixelChunk {
    type Data = (u8, u8, u8);
    fn into_raw(self) -> glium::texture::RawImage2d<'a, (u8, u8, u8)> {
        let flattened_pixels = unsafe {
            std::mem::transmute::<
                &[[(u8, u8, u8); CHUNK_SIZE]; CHUNK_SIZE],
                &[(u8, u8, u8); CHUNK_SIZE * CHUNK_SIZE],
            >(&self.0)
        };
        glium::texture::RawImage2d {
            data: Cow::Borrowed(flattened_pixels),
            width: CHUNK_SIZE as u32,
            height: CHUNK_SIZE as u32,
            format: glium::texture::ClientFormat::U8U8U8,
        }
    }
}
impl CellPixelChunk {
    pub fn fill_with_quadtree_node<C: CellType, F>(
        &mut self,
        branch: &NdTreeBranch<C, Dim2D>,
        branch_to_pixel: &F,
    ) where
        F: Fn(&NdTreeBranch<C, Dim2D>) -> (u8, u8, u8),
    {
        self.fill_part_with_quadtree_node(branch, branch_to_pixel, CHUNK_POW, Vec2D::origin())
    }
    fn fill_part_with_quadtree_node<C: CellType, F>(
        &mut self,
        branch: &NdTreeBranch<C, Dim2D>,
        branch_to_pixel: &F,
        layers_remaining: usize,
        offset: Vec2D,
    ) where
        F: Fn(&NdTreeBranch<C, Dim2D>) -> (u8, u8, u8),
    {
        if layers_remaining == 0 {
            let x = *offset.x() as usize;
            let y = *offset.y() as usize;
            self.0[y][x] = branch_to_pixel(branch);
        } else if let NdTreeBranch::Node(node) = branch {
            for branch_idx in 0..4 {
                self.fill_part_with_quadtree_node(
                    &node.branches[branch_idx],
                    branch_to_pixel,
                    layers_remaining - 1,
                    offset + ndtree_branch_offset(layers_remaining, branch_idx),
                );
            }
        }
    }
}

#[derive(Default)]
pub(super) struct RenderCache {
    chunks: HashMap<NdCachedNode<u8, Dim2D>, (CellPixelChunk, bool)>,
}

pub(super) struct Shaders {
    cell_chunk: glium::Program,
    gridlines: glium::Program,
}
impl Shaders {
    pub fn compile(display: &Rc<glium::Display>) -> Self {
        let display = &**display;
        Self {
            cell_chunk: shaders::compile_cell_program(display),
            gridlines: shaders::compile_lines_program(display),
        }
    }
}

pub(super) struct VBOs {
    cell_chunk: glium::VertexBuffer<SimpleColorVertex>,
    cell_point: glium::VertexBuffer<PointVertex>,
    cell_square: glium::VertexBuffer<PointVertex>,
    gridlines: glium::VertexBuffer<PointVertex>,
}
impl VBOs {
    pub fn new(display: &Rc<glium::Display>) -> Self {
        let display = &**display;
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

struct CachedRenderBuffer<F: glium::backend::Facade> {
    facade: Rc<F>,
    format: glium::texture::UncompressedFloatFormat,
    buffer: Option<glium::framebuffer::RenderBuffer>,
}
impl<F: glium::backend::Facade> CachedRenderBuffer<F> {
    pub fn new(facade: Rc<F>, format: glium::texture::UncompressedFloatFormat) -> Self {
        Self {
            facade,
            format,
            buffer: None,
        }
    }
    pub fn current_size(&self) -> Option<(u32, u32)> {
        self.buffer.as_ref().map(|buffer| buffer.get_dimensions())
    }
    pub fn unwrap(&self) -> &glium::framebuffer::RenderBuffer {
        if let Some(buffer) = &self.buffer {
            buffer
        } else {
            panic!("Called .unwrap() on an empty CachedRenderBuffer")
        }
    }
    pub fn at_size(&mut self, width: u32, height: u32) -> &glium::framebuffer::RenderBuffer {
        if let Some((width, height)) = self.current_size() {
            self.unwrap()
        } else {
            let ret =
                glium::framebuffer::RenderBuffer::new(&*self.facade, self.format, width, height)
                    .expect("Failed to create render buffer");
            self.buffer = Some(ret);
            self.unwrap()
        }
    }
}

pub(super) struct Textures {
    cell_pixels: CachedRenderBuffer<glium::Display>,
    scaled_cells: CachedRenderBuffer<glium::Display>,
}
impl Textures {
    pub fn new(display: &Rc<glium::Display>) -> Self {
        Self {
            cell_pixels: CachedRenderBuffer::new(
                display.clone(),
                glium::texture::UncompressedFloatFormat::U8U8U8,
            ),
            scaled_cells: CachedRenderBuffer::new(
                display.clone(),
                glium::texture::UncompressedFloatFormat::U8U8U8,
            ),
        }
    }
}

pub fn draw(grid_view: &mut GridView2D, target: &mut glium::Frame) {
    #[cfg(debug_assertions)]
    {
        // In debug mode, clear the screen with magenta to highlight any gaps
        // that are not updated.
        target.clear_color_srgb(1.0, 0.0, 1.0, 1.0);
    }
    let mut rip = RenderInProgress::new(grid_view, target);
    rip.draw_cells();
    // rip.draw_gridlines();
}

struct RenderInProgress<'a> {
    g: &'a mut GridView2D,
    target: &'a mut glium::Frame,
    /// The node layer of a render cell.
    render_cell_layer: usize,
    /// The width in pixels of a render cell.
    render_cell_pixels: f32,
    /// A slice of the quadtree that encompasses all visible cells.
    quadtree_slice: NdTreeSlice<u8, Dim2D>,
    /// The render cell position within quadtree_slice that is centered on the screen.
    pos: (f32, f32),
    /// A rectangle of render cells within quadtree_slice that is visible.
    visible_rect: Rect2D,
    /// A rectangle of chunk coordinates (i.e. cell coordinates divided by the
    /// number of cells in a chunk) for the chunks that are visible.
    chunk_visible_rect: Rect2D,
    // /// The view matrix converting from quadtree_slice space (1 unit = 1 render
    // /// cell; (0, 0) = bottom left) to screen space ((-1, -1) = bottom left; (1,
    // /// 1) = top right).
    // view_matrix: [[f32; 4]; 4],
}
impl<'a> RenderInProgress<'a> {
    /// Performs preliminary computations and returns a RenderInProgress.
    pub fn new(g: &'a mut GridView2D, target: &'a mut glium::Frame) -> Self {
        let (target_w, target_h) = target.get_dimensions();
        let mut zoom = g.viewport.zoom;

        // Compute the width of pixels for each individual cell.
        let cell_pixels: f32 = zoom.pixels_per_cell();

        // Compute the lowest layer that must be visited, which is the layer of
        // a "render cell," a node that is rendered as one unit (one pixel in
        // step #1).
        let render_cell_layer: usize = std::cmp::max(0, -zoom.ceil().power() as isize) as usize;
        // Compute the width of cells represented by each render cell.
        let render_cell_len: usize = 1 << render_cell_layer;
        // The render cell layer is also the power of two we should subtract from
        // the zoom level.
        zoom = Zoom2D::from_power(zoom.power() + render_cell_layer as f32);
        // Now the zoom power should be greater than -1.0 (since -1.0 or
        // anything lower should be handled by having a higher render cell
        // layer).
        assert!(zoom.power() > -1.0);

        // Compute the width of pixels for each render cell.
        let render_cell_pixels: f32 = zoom.pixels_per_cell();

        // Compute the width of cells for each chunk. (Note that CHUNK_SIZE
        // is the number of render cells per chunk, not individual cells.)
        let chunk_cell_len: isize = (CHUNK_SIZE * (1 << render_cell_layer)) as isize;

        // Get an NdCachedNode that covers the entire visible area, a rectangle
        // of visible chunks relative to that node (using cell coordinates
        // divided by CHUNK_SIZE), a rectangle of visible cells relative to that
        // node, and a floating-point position relative to that node that will
        // be in the center of the screen.
        let quadtree_slice: NdTreeSlice<u8, Dim2D>;
        let visible_rect: Rect2D;
        let chunk_visible_rect: Rect2D;
        let pos_x: f32;
        let pos_y: f32;
        {
            // Before computing the rectangles relative to the slice, get the
            // rectangles in global space.
            let global_visible_rect: Rect2D;
            let global_chunk_visible_rect: Rect2D;
            {
                // Compute half the width and height of individual cells that
                // fit on the screen. (This ends up being more useful than the
                // full number.)
                let half_cells_w = target_w as f32 / cell_pixels / 2.0;
                let half_cells_h = target_h as f32 / cell_pixels / 2.0;
                let half_diag =
                    Vec2D::from([half_cells_w.ceil() as isize, half_cells_h.ceil() as isize]);

                let lower_bound = g.viewport.pos - half_diag;
                let upper_bound = g.viewport.pos + half_diag;
                global_visible_rect = Rect2D::span(lower_bound, upper_bound);

                // Compute the chunk coordinates for the lower and upper bounds
                // using simple floor division.
                global_chunk_visible_rect = global_visible_rect / chunk_cell_len;
            }

            // Now fetch the NdTreeSlice containing all of the visible chunks.
            quadtree_slice = g.automaton.get_projected_tree().get_slice_containing(
                // Convert chunk coordinates into normal cell coordinates.
                global_chunk_visible_rect * chunk_cell_len,
            );
            // That slice should line up along chunk boundaries.
            assert_eq!(Vec2D::origin(), quadtree_slice.min() % chunk_cell_len);
            assert_eq!(Vec2D::origin(), (quadtree_slice.max() + 1) % chunk_cell_len);

            // Subtract the slice offset from global_visible_rect and
            // global_chunk_visible_rect so that they are relative to the slice.
            // TODO: This is where you should convert from BigInts to isize, at
            // least for chunk_visible_rect.
            visible_rect = global_visible_rect - quadtree_slice.offset;
            chunk_visible_rect = global_chunk_visible_rect - quadtree_slice.offset / chunk_cell_len;

            // Subtract the slice offset from the viewport position and divide
            // by the size of a render cell.
            // let integer_pos = g.viewport.pos - slice.offset;
            let integer_pos = (g.viewport.pos - quadtree_slice.offset) / render_cell_len as isize;
            let integer_pos_x = *integer_pos.x() as f32;
            let integer_pos_y = *integer_pos.y() as f32;
            // viewport.x_offset and .y_offset are sub-cell offsets, so they
            // only matter when zoomed in more than 1:1.
            if g.viewport.zoom.pixels_per_cell() > 1.0 {
                pos_x = integer_pos_x + g.viewport.x_offset;
                pos_y = integer_pos_y + g.viewport.y_offset;
            } else {
                pos_x = integer_pos_x;
                pos_y = integer_pos_y;
            }
        }

        Self {
            g,
            target,
            render_cell_layer,
            render_cell_pixels,
            // view_matrix,
            quadtree_slice,
            pos: (pos_x, pos_y),
            visible_rect,
            chunk_visible_rect,
        }
    }

    /// Draw the cells that appear in the viewport.
    pub fn draw_cells(&mut self) {
        // Steps #1 and #2: draw at 1 pixel per render cell.
        let render_cells_rect = self.chunk_visible_rect * CHUNK_SIZE as isize;
        let unscaled_cells_w = render_cells_rect.len(Axis::X) as u32;
        let unscaled_cells_h = render_cells_rect.len(Axis::Y) as u32;
        // let unscaled_cells_texture = self
        //     .g
        //     .textures
        //     .cell_pixels
        //     .at_size(unscaled_cells_w, unscaled_cells_h);
        let unscaled_cells_texture = &glium::texture::srgb_texture2d::SrgbTexture2d::empty(
            &*self.g.display,
            unscaled_cells_w,
            unscaled_cells_h,
        )
        .expect("Failed to create texture");
        let mut unscaled_cells_fbo =
            glium::framebuffer::SimpleFrameBuffer::new(&*self.g.display, unscaled_cells_texture)
                .expect("Failed to create frame buffer");
        self.draw_cell_chunks(
            unscaled_cells_texture,
            &self.quadtree_slice.root.clone(),
            Vec2D::origin(),
            self.quadtree_slice.root.layer - self.render_cell_layer - CHUNK_POW,
        );

        // Step #3: resize that texture by an integer factor, only including the
        // cells that are visible.
        let render_cell_visible_rect = self.visible_rect / (1 << self.render_cell_layer)
            - (self.chunk_visible_rect * CHUNK_SIZE as isize).min();
        let integer_scale_factor = self.render_cell_pixels.ceil() as u32;
        let visible_cells_w = render_cell_visible_rect.len(Axis::X) as u32;
        let visible_cells_h = render_cell_visible_rect.len(Axis::Y) as u32;
        let scaled_cells_w = visible_cells_w * integer_scale_factor;
        let scaled_cells_h = visible_cells_h * integer_scale_factor;
        // let scaled_cells_texture = self
        //     .g
        //     .textures
        //     .scaled_cells
        //     .at_size(scaled_cells_w, scaled_cells_h);
        let scaled_cells_texture = &glium::framebuffer::RenderBuffer::new(
            &*self.g.display,
            glium::texture::UncompressedFloatFormat::U8U8U8,
            scaled_cells_w,
            scaled_cells_h,
        )
        .expect("Failed to create render buffer");

        let scaled_cells_fbo =
            glium::framebuffer::SimpleFrameBuffer::new(&*self.g.display, scaled_cells_texture)
                .expect("Failed to create frame buffer");
        let source_rect = glium::Rect {
            left: *render_cell_visible_rect.min().x() as u32,
            bottom: *render_cell_visible_rect.min().y() as u32,
            width: render_cell_visible_rect.len(Axis::X) as u32,
            height: render_cell_visible_rect.len(Axis::Y) as u32,
        };
        scaled_cells_fbo.blit_from_simple_framebuffer(
            &unscaled_cells_fbo,
            &source_rect,
            &entire_blit_target(&scaled_cells_fbo),
            glium::uniforms::MagnifySamplerFilter::Nearest,
        );

        // Step #4: blit that onto the screen.
        let (target_w, target_h) = self.target.get_dimensions();
        let render_cells_w = (target_w as f32 / self.render_cell_pixels) as u32;
        let render_cells_h = (target_h as f32 / self.render_cell_pixels) as u32;
        let render_cells_x = self.pos.0
            - *(self.visible_rect / (1 << self.render_cell_layer) as isize)
                .min()
                .x() as f32
            - render_cells_w as f32 / 2.0;
        let render_cells_y = self.pos.1
            - *(self.visible_rect / (1 << self.render_cell_layer) as isize)
                .min()
                .y() as f32
            - render_cells_h as f32 / 2.0;
        let source_rect = glium::Rect {
            left: (render_cells_x * integer_scale_factor as f32).round() as u32,
            bottom: (render_cells_y * integer_scale_factor as f32).round() as u32,
            width: render_cells_w * integer_scale_factor,
            height: render_cells_h * integer_scale_factor,
        };
        self.target.blit_from_simple_framebuffer(
            &scaled_cells_fbo,
            &source_rect,
            &entire_blit_target(self.target),
            glium::uniforms::MagnifySamplerFilter::Linear,
        );

        // // minimap in corner
        // self.target.blit_from_simple_framebuffer(
        //     &scaled_cells_fbo,
        //     &entire_rect(&scaled_cells_fbo),
        //     &entire_blit_target(&scaled_cells_fbo),
        //     glium::uniforms::MagnifySamplerFilter::Linear,
        // );

        // Remove elements from the cache that we did not use this frame, and
        // mark the rest as being unused (to prepare for the next frame).
        self.g.render_cache.chunks.retain(|_, (_, ref mut keep)| {
            let ret = *keep;
            *keep = false;
            ret
        });
    }
    fn draw_cell_chunks(
        &mut self,
        target: &glium::texture::srgb_texture2d::SrgbTexture2d,
        root_node: &NdCachedNode<u8, Dim2D>,
        root_node_offset: Vec2D,
        layers_remaining: usize,
    ) {
        let lower_chunk_bound = root_node_offset;
        let upper_chunk_bound = root_node_offset + (1 << layers_remaining) - 1;
        let root_node_chunk_rect = Rect2D::span(lower_chunk_bound, upper_chunk_bound);
        // If nothing in this node is visible, just skip it entirely.
        if !self.chunk_visible_rect.intersects(root_node_chunk_rect) {
            return;
        }
        if layers_remaining == 0 {
            // Get the pixel data.
            let pixels = self.get_color_array(root_node);

            // Write those pixels into the cells texture.
            let offset_in_texture =
                (root_node_offset - self.chunk_visible_rect.min()) * CHUNK_SIZE as isize;
            target.write(
                glium::Rect {
                    left: *offset_in_texture.x() as u32,
                    bottom: *offset_in_texture.y() as u32,
                    width: CHUNK_SIZE as u32,
                    height: CHUNK_SIZE as u32,
                },
                &pixels,
            );
        } else {
            // Recurse into each child node.
            for branch_idx in 0..4 {
                if let NdTreeBranch::Node(child_node) = &root_node.branches[branch_idx] {
                    let branch_offset = ndtree_branch_offset(layers_remaining, branch_idx);
                    let child_node_offset = root_node_offset + branch_offset;
                    self.draw_cell_chunks(
                        target,
                        &child_node,
                        child_node_offset,
                        layers_remaining - 1,
                    );
                } else {
                    unreachable!();
                }
            }
        }
    }
    // Get a Vec of vertices for all the cells in the given chunk, creating a
    // new one if necessary.
    fn get_color_array(&mut self, node: &NdCachedNode<u8, Dim2D>) -> CellPixelChunk {
        self.g
            .render_cache
            .chunks
            .entry(node.clone())
            .and_modify(|(_, ref mut keep)| {
                *keep = true;
            })
            .or_insert_with(|| {
                let mut vertices = CellPixelChunk::default();
                vertices.fill_with_quadtree_node(
                    &NdTreeBranch::Node(node.clone()),
                    &|branch: &NdTreeBranch<u8, Dim2D>| {
                        let live = match branch {
                            NdTreeBranch::Leaf(cell_state) => *cell_state != 0,
                            NdTreeBranch::Node(node) => node.population != 0,
                        };
                        if live {
                            LIVE_COLOR
                        } else {
                            DEAD_COLOR
                        }
                    },
                );
                (vertices, true)
            })
            .0
            .clone()
    }
    /// Draw the gridlines that appear in the viewport.
    pub fn draw_gridlines(&mut self) {
        // Don't bother drawing gridlines if we're zoomed out far enough.
        if self.g.viewport.zoom.pixels_per_cell() < 8.0 {
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

        let (target_w, target_h) = self.target.get_dimensions();
        // Multiply by 2 because the OpenGL screen space ranged from -1 to +1.
        let x_scale = self.g.viewport.zoom.pixels_per_cell() / target_w as f32 * 2.0;
        let y_scale = self.g.viewport.zoom.pixels_per_cell() / target_h as f32 * 2.0;
        let (x_render_cell_pos, y_render_cell_pos) = self.pos;
        let mut x_offset = x_render_cell_pos * (1 << self.render_cell_layer) as f32;
        let mut y_offset = y_render_cell_pos * (1 << self.render_cell_layer) as f32;
        // Convert offset from render-cell-space to screen-space.
        x_offset *= x_scale;
        y_offset *= y_scale;
        // Compute the view matrix.
        let view_matrix = [
            [x_scale, 0.0, 0.0, 0.0],
            [0.0, y_scale, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [-x_offset, -y_offset, 0.0, 1.0],
        ];

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
    }
}

fn entire_rect<S: glium::Surface>(surface: &S) -> glium::Rect {
    let (width, height) = surface.get_dimensions();
    glium::Rect {
        left: 0,
        bottom: 0,
        width,
        height,
    }
}

fn entire_blit_target<S: glium::Surface>(surface: &S) -> glium::BlitTarget {
    let (width, height) = surface.get_dimensions();
    glium::BlitTarget {
        left: 0,
        bottom: 0,
        width: width as i32,
        height: height as i32,
    }
}
