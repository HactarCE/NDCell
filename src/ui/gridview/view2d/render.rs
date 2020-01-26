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

use glium::index::PrimitiveType;
use glium::{uniform, Surface as _};
use noisy_float::prelude::r64;
use num::{BigInt, ToPrimitive, Zero};
use std::borrow::Cow;
use std::cell::RefMut;
use std::collections::HashMap;

use super::*;
use crate::automaton::space::*;

/// A vertex containing a 2D floating-point position and a 2D texture coordinate.
#[derive(Debug, Default, Copy, Clone)]
struct TexturePosVertex {
    pos: [f32; 2],
    tex_coords: [f32; 2],
}
glium::implement_vertex!(TexturePosVertex, pos, tex_coords);

/// A vertex containing just a 2D floating-point position.
#[derive(Debug, Default, Copy, Clone)]
struct PointVertex {
    pos: [f32; 2],
}
glium::implement_vertex!(PointVertex, pos);
impl From<[isize; 2]> for PointVertex {
    fn from(pos: [isize; 2]) -> Self {
        Self {
            pos: [pos[0] as f32, pos[1] as f32],
        }
    }
}

/// The color of the grid. This will be configurable in the future.
const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
/// The color for dead cells. This will be configurable in the future.
const DEAD_COLOR: (u8, u8, u8) = (0, 0, 0);
/// The color for live cells. This will be configurable in the future.
const LIVE_COLOR: (u8, u8, u8) = (255, 255, 255);

const CHUNK_POW: usize = 8;
/// The length of render cells per chunk; a render cell is a square of cells
/// that is rendered as one unit.
///
/// When zoomed in, each render cell is the same as a normal cell. When zoomed
/// out beyond 1:1, each render cell is a single pixel, which can contain a
/// large square of cells. A chunk is a groups of render cells that is passed to
/// the graphics card at once.
const CHUNK_SIZE: usize = 1 << CHUNK_POW; // 2^8 = 256

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
        offset: UVec2D,
    ) where
        F: Fn(&NdTreeBranch<C, Dim2D>) -> (u8, u8, u8),
    {
        if layers_remaining == 0 {
            let x = offset[X];
            let y = offset[Y];
            self.0[y][x] = branch_to_pixel(branch);
        } else {
            for (sub_branch_idx, sub_branch) in branch.node().unwrap().branch_iter() {
                let sub_branch_offset: UVec2D = sub_branch_idx.branch_offset(layers_remaining);
                self.fill_part_with_quadtree_node(
                    &sub_branch,
                    branch_to_pixel,
                    layers_remaining - 1,
                    offset + sub_branch_offset,
                );
            }
        }
    }
}

struct Shaders {
    cells: glium::Program,
    gridlines: glium::Program,
}
impl Shaders {
    pub fn compile(display: &glium::Display) -> Self {
        Self {
            cells: shaders::compile_cells_program(display),
            gridlines: shaders::compile_lines_program(display),
        }
    }
}

struct VBOs {
    cells: glium::VertexBuffer<TexturePosVertex>,
    gridlines: glium::VertexBuffer<PointVertex>,
}
impl VBOs {
    pub fn new(display: &glium::Display) -> Self {
        Self {
            cells: glium::VertexBuffer::empty_dynamic(display, 4)
                .expect("Failed to create vertex buffer"),
            gridlines: glium::VertexBuffer::empty_dynamic(display, GRIDLINE_BATCH_SIZE)
                .expect("Failed to create vertex buffer"),
        }
    }
}

pub(super) struct RenderCache {
    chunks: HashMap<NdCachedNode<u8, Dim2D>, (CellPixelChunk, bool)>,
    shaders: Shaders,
    vbos: VBOs,
    display: Rc<glium::Display>,
}
impl RenderCache {
    pub fn new(display: Rc<glium::Display>) -> Self {
        Self {
            chunks: HashMap::default(),
            shaders: Shaders::compile(&*display),
            vbos: VBOs::new(&*display),
            display: display,
        }
    }
}

pub fn draw(grid_view: &mut GridView2D, target: &mut glium::Frame) {
    let mut rip = RenderInProgress::new(grid_view, target);
    rip.draw_cells();
    rip.draw_gridlines();
}

struct RenderInProgress<'a> {
    cache: RefMut<'a, RenderCache>,
    /// The viewport to use when rendering.
    viewport: Viewport2D,
    /// The target to render to.
    target: &'a mut glium::Frame,
    /// The node layer of a render cell.
    render_cell_layer: usize,
    /// The width in pixels of a render cell.
    render_cell_pixels: f64,
    /// A slice of the quadtree that encompasses all visible cells.
    quadtree_slice: NdTreeSlice<u8, Dim2D>,
    /// The render cell position within quadtree_slice that is centered on the screen.
    pos: FVec2D,
    /// A rectangle of render cells within quadtree_slice that is visible.
    visible_rect: BigRect2D,
    /// A rectangle of chunk coordinates (i.e. cell coordinates divided by the
    /// number of cells in a chunk) within quadtree_slice for the chunks that
    /// are visible.
    chunk_visible_rect: IRect2D,
    // /// The view matrix converting from quadtree_slice space (1 unit = 1 render
    // /// cell; (0, 0) = bottom left) to screen space ((-1, -1) = bottom left; (1,
    // /// 1) = top right).
    // view_matrix: [[f32; 4]; 4],
}
impl<'a> RenderInProgress<'a> {
    /// Performs preliminary computations and returns a RenderInProgress.
    pub fn new(g: &'a mut GridView2D, target: &'a mut glium::Frame) -> Self {
        let (target_w, target_h) = target.get_dimensions();
        let target_pixels_size: FVec2D = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let viewport = g.interpolating_viewport.clone();
        let cache = g.render_cache.borrow_mut();
        let mut zoom = viewport.zoom;

        // Compute the width of pixels for each individual cell.
        let cell_pixels: f64 = zoom.pixels_per_cell();

        // Compute the lowest layer that must be visited, which is the layer of
        // a "render cell," a node that is rendered as one unit (one pixel in
        // step #1).
        let render_cell_layer: usize = std::cmp::max(0, -zoom.floor().power() as isize) as usize;
        // Compute the width of cells represented by each render cell.
        let render_cell_len: BigInt = BigInt::from(1) << render_cell_layer;
        // The render cell layer is also the power of two we should subtract from
        // the zoom level.
        zoom = Zoom2D::from_power(zoom.power() + render_cell_layer as f64);
        // Now the zoom power should be greater than -1.0 (since -1.0 or
        // anything lower should be handled by having a higher render cell
        // layer).
        assert!(zoom.power() > -1.0);

        // Compute the width of pixels for each render cell.
        let render_cell_pixels: f64 = zoom.pixels_per_cell();

        // Compute the width of cells for each chunk. (Note that CHUNK_SIZE
        // is the number of render cells per chunk, not individual cells.)
        let chunk_cell_len: BigInt = CHUNK_SIZE * BigInt::from(1 << render_cell_layer);

        // Get an NdCachedNode that covers the entire visible area, a rectangle
        // of visible chunks relative to that node (using cell coordinates
        // divided by CHUNK_SIZE), a rectangle of visible cells relative to that
        // node, and a floating-point position relative to that node that will
        // be in the center of the screen.
        let quadtree_slice: NdTreeSlice<u8, Dim2D>;
        let visible_rect: BigRect2D;
        let chunk_visible_rect: IRect2D;
        let pos: FVec2D;
        {
            // Before computing the rectangles relative to the slice, get the
            // rectangles in global space.
            let global_visible_rect: BigRect2D;
            let global_chunk_visible_rect: BigRect2D;
            {
                // Compute the width and height of individual cells that fit on
                // the screen. Because this number is stored in an f64, which
                // cannot represent values above around 2^1022, zoom levels past
                // 2^1000 can overflow this. This is handled by Zoom2D::clamp().
                let target_cells_size: FVec2D = target_pixels_size.convert() / r64(cell_pixels);
                // Compute the cell vector pointing from the origin to the top
                // right corner of the screen; i.e. the "half diagonal."
                let half_diag: FVec2D = target_cells_size / 2.0;

                global_visible_rect =
                    BigRect2D::centered(viewport.pos.clone(), &half_diag.ceil().as_bigvec());

                // Compute the chunk coordinates for the lower and upper bounds
                // using division that rounds "outward."
                global_chunk_visible_rect = global_visible_rect.div_outward(&chunk_cell_len);
                // TODO: see what happenes if you divide "inward" instead.
            }

            // Now fetch the NdTreeSlice containing all of the visible chunks.
            quadtree_slice = g.automaton.get_projected_tree().get_slice_containing(
                // Convert chunk coordinates into normal cell coordinates.
                &(global_chunk_visible_rect.clone() * &chunk_cell_len),
            );
            // That slice should line up along chunk boundaries.
            assert!(quadtree_slice.min().mod_floor(&chunk_cell_len).is_zero());
            assert!(quadtree_slice.size().mod_floor(&chunk_cell_len).is_zero());

            // Subtract the slice offset from global_visible_rect and
            // global_chunk_visible_rect so that they are relative to the slice.
            // This is where we can convert from BigInt to isize, since the
            // quadtree slice should be relatively close to the visible rect.
            visible_rect = global_visible_rect.clone() - &quadtree_slice.offset;
            chunk_visible_rect = (global_chunk_visible_rect.clone()
                - quadtree_slice.offset.clone().div_floor(&chunk_cell_len))
            .as_irect();

            // Subtract the slice offset from the viewport position and divide
            // by the size of a render cell.
            let integer_pos =
                (viewport.pos.clone() - &quadtree_slice.offset).div_floor(&render_cell_len);
            // viewport.offset is a sub-cell offset, so they only matter when
            // zoomed in more than 1:1.
            if viewport.zoom.pixels_per_cell() > 1.0 {
                pos = integer_pos.as_fvec() + viewport.offset;
            } else {
                pos = integer_pos.as_fvec();
            }
        }

        Self {
            cache,
            viewport,
            target,
            render_cell_layer,
            render_cell_pixels,
            quadtree_slice,
            pos,
            visible_rect,
            chunk_visible_rect,
        }
    }

    /// Draw the cells that appear in the viewport.
    pub fn draw_cells(&mut self) {
        // Steps #1 and #2: draw at 1 pixel per render cell.
        let render_cells_rect = self.chunk_visible_rect * CHUNK_SIZE as isize;
        let unscaled_cells_w = render_cells_rect.len(X) as u32;
        let unscaled_cells_h = render_cells_rect.len(Y) as u32;
        let unscaled_cells_texture = glium::texture::srgb_texture2d::SrgbTexture2d::empty(
            &*self.cache.display,
            unscaled_cells_w,
            unscaled_cells_h,
        )
        .expect("Failed to create texture");
        let unscaled_cells_fbo = glium::framebuffer::SimpleFrameBuffer::new(
            &*self.cache.display,
            &unscaled_cells_texture,
        )
        .expect("Failed to create frame buffer");
        self.draw_cell_chunks(
            &unscaled_cells_texture,
            &self.quadtree_slice.root.clone(),
            Vec2D::origin(),
            self.quadtree_slice.root.layer - self.render_cell_layer - CHUNK_POW,
        );

        // Step #3: resize that texture by an integer factor, only including the
        // cells that are visible.target_pixels_
        let render_cell_visible_rect: IRect2D = (self
            .visible_rect
            .div_outward(&(BigInt::from(1) << self.render_cell_layer))
            - self.chunk_visible_rect.min() * CHUNK_SIZE as isize)
            .as_irect();
        let integer_scale_factor = self.render_cell_pixels.ceil();
        let visible_cells_w = render_cell_visible_rect.len(X) as u32;
        let visible_cells_h = render_cell_visible_rect.len(Y) as u32;
        let scaled_cells_w = visible_cells_w * integer_scale_factor as u32;
        let scaled_cells_h = visible_cells_h * integer_scale_factor as u32;
        let scaled_cells_texture = glium::texture::SrgbTexture2d::empty(
            &*self.cache.display,
            scaled_cells_w,
            scaled_cells_h,
        )
        .expect("Failed to create render buffer");

        let scaled_cells_fbo =
            glium::framebuffer::SimpleFrameBuffer::new(&*self.cache.display, &scaled_cells_texture)
                .expect("Failed to create frame buffer");
        let source_rect = glium::Rect {
            left: render_cell_visible_rect.min()[X] as u32,
            bottom: render_cell_visible_rect.min()[Y] as u32,
            width: render_cell_visible_rect.len(X) as u32,
            height: render_cell_visible_rect.len(Y) as u32,
        };
        scaled_cells_fbo.blit_from_simple_framebuffer(
            &unscaled_cells_fbo,
            &source_rect,
            &entire_blit_target(&scaled_cells_fbo),
            glium::uniforms::MagnifySamplerFilter::Nearest,
        );

        // Step #4: render that onto the screen.
        let (target_w, target_h) = self.target.get_dimensions();
        let target_size = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let render_cells_size = target_size / r64(self.render_cell_pixels);
        let render_cells_center = self.pos
            - self
                .visible_rect
                .div_outward(&(BigInt::from(1) << self.render_cell_layer))
                .min()
                .as_fvec();
        let mut render_cells_rect = FRect2D::centered(render_cells_center, render_cells_size / 2.0);
        render_cells_rect /= render_cell_visible_rect.size().as_fvec();

        let left = render_cells_rect.min()[X].raw() as f32;
        let right = render_cells_rect.max()[X].raw() as f32;
        let bottom = render_cells_rect.min()[Y].raw() as f32;
        let top = render_cells_rect.max()[Y].raw() as f32;

        self.cache.vbos.cells.write(&[
            TexturePosVertex {
                pos: [-1.0, -1.0],
                tex_coords: [left, bottom],
            },
            TexturePosVertex {
                pos: [1.0, -1.0],
                tex_coords: [right, bottom],
            },
            TexturePosVertex {
                pos: [-1.0, 1.0],
                tex_coords: [left, top],
            },
            TexturePosVertex {
                pos: [1.0, 1.0],
                tex_coords: [right, top],
            },
        ]);
        self.target
            .draw(
                &self.cache.vbos.cells,
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &self.cache.shaders.cells,
                &uniform! {
                    cells_texture: scaled_cells_texture.sampled(),
                },
                &glium::DrawParameters::default(),
            )
            .expect("Failed to draw cells");

        // Remove elements from the cache that we did not use this frame, and
        // mark the rest as being unused (to prepare for the next frame).
        self.cache.chunks.retain(|_, (_, ref mut keep)| {
            let ret = *keep;
            *keep = false;
            ret
        });
    }
    fn draw_cell_chunks(
        &mut self,
        target: &glium::texture::srgb_texture2d::SrgbTexture2d,
        root_node: &NdCachedNode<u8, Dim2D>,
        root_node_offset: IVec2D,
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
                    left: offset_in_texture[X] as u32,
                    bottom: offset_in_texture[Y] as u32,
                    width: CHUNK_SIZE as u32,
                    height: CHUNK_SIZE as u32,
                },
                &pixels,
            );
        } else {
            // Recurse into each child node.
            for (branch_idx, branch) in root_node.branch_iter() {
                let child_node = branch.node().unwrap();
                let branch_offset: IVec2D = branch_idx.branch_offset(layers_remaining);
                let child_node_offset: IVec2D = root_node_offset + branch_offset;
                self.draw_cell_chunks(target, child_node, child_node_offset, layers_remaining - 1);
            }
        }
    }
    // Get a Vec of vertices for all the cells in the given chunk, creating a
    // new one if necessary.
    fn get_color_array(&mut self, node: &NdCachedNode<u8, Dim2D>) -> CellPixelChunk {
        self.cache
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
                        // let live = match branch {
                        //     NdTreeBranch::Leaf(cell_state) => *cell_state != 0,
                        //     NdTreeBranch::Node(node) => node.population != 0,
                        // };
                        // if live {
                        //     LIVE_COLOR
                        // } else {
                        //     DEAD_COLOR
                        // }
                        let ratio = match branch {
                            NdTreeBranch::Leaf(cell_state) => *cell_state as f64,
                            NdTreeBranch::Node(node) => {
                                if node.population.is_zero() {
                                    0.0
                                } else if let Some(node_len) = node.len().to_f64() {
                                    let population = node.population.to_f64().unwrap();
                                    (population / 2.0) / node_len.powf(2.0) + 0.5
                                } else {
                                    1.0
                                }
                            }
                        };
                        let r = ((LIVE_COLOR.0 as f64).powf(2.0) * ratio
                            + (DEAD_COLOR.0 as f64).powf(2.0) * (1.0 - ratio))
                            .powf(0.5);
                        let g = ((LIVE_COLOR.1 as f64).powf(2.0) * ratio
                            + (DEAD_COLOR.1 as f64).powf(2.0) * (1.0 - ratio))
                            .powf(0.5);
                        let b = ((LIVE_COLOR.2 as f64).powf(2.0) * ratio
                            + (DEAD_COLOR.2 as f64).powf(2.0) * (1.0 - ratio))
                            .powf(0.5);
                        (r as u8, g as u8, b as u8)
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
        if self.viewport.zoom.pixels_per_cell() < 8.0 {
            return;
        }
        // Convert visible_rect from a BigVec to an UVec, which is only safe
        // because we're zoomed in enough that far less than isize::MAX cells
        // are in self.quadtree_slice.
        let visible_rect: IRect2D = self.visible_rect.as_irect();
        // Generate a pair of vertices for each gridline.
        let gridlines_indices = glium::index::NoIndices(PrimitiveType::LinesList);
        let mut gridline_vertices =
            Vec::with_capacity((visible_rect.len(X) + visible_rect.len(Y)) as usize * 2);
        let min = visible_rect.min();
        let max = visible_rect.max() + 1;
        let min_x = min[X];
        let min_y = min[Y];
        let max_x = max[X];
        let max_y = max[Y];
        for x in visible_rect.axis_range(X) {
            gridline_vertices.push(PointVertex::from([x, min_y]));
            gridline_vertices.push(PointVertex::from([x, max_y]));
        }
        for y in visible_rect.axis_range(Y) {
            gridline_vertices.push(PointVertex::from([min_x, y]));
            gridline_vertices.push(PointVertex::from([max_x, y]));
        }

        let (target_w, target_h) = self.target.get_dimensions();
        let target_size: FVec2D = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let pixels_per_cell = r64(self.viewport.zoom.pixels_per_cell());
        // Multiply by 2 because the OpenGL screen space ranged from -1 to +1.
        let scale = FVec2D::repeat(pixels_per_cell) / target_size * r64(2.0);
        let mut offset: FVec2D = self.pos * r64(1.0f64.powf(self.render_cell_layer as f64));
        // Convert offset from render-cell-space to screen-space.
        offset *= scale;
        let x_offset = offset[X].raw() as f32;
        let y_offset = offset[Y].raw() as f32;
        let x_scale = scale[X].raw() as f32;
        let y_scale = scale[Y].raw() as f32;
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
            let vbo_slice = self.cache.vbos.gridlines.slice(0..batch.len()).unwrap();
            vbo_slice.write(batch);
            // Draw gridlines.
            self.target
                .draw(
                    vbo_slice,
                    &gridlines_indices,
                    &self.cache.shaders.gridlines,
                    &uniform! {
                        matrix: view_matrix,
                        lines_color: GRID_COLOR,
                    },
                    &glium::DrawParameters {
                        line_width: Some(1.0),
                        blend: glium::Blend::alpha_blending(),
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
