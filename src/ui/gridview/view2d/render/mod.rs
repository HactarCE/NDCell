//! The data structures and routines related to rendering 2D square cells.
//!
//! Currently, only solid colors are supported, however I plan to add icons in
//! the future.
//!
//! Not including preliminary computations, there are four main stages to
//! rendering:
//!
//! 1. Create an indexed quadtree of render cells encoded in a 1D OpenGL
//!    texture. (A "render cell" is a node of the quadtree which is rendered as
//!    single unit; this is a single cell when zoomed in, but may be larger when
//!    zoomed out.)
//! 2. Render the visible portion of that quadtree into an OepnGL texture.
//!    Render that pixel data into the corresponding section of an OpenGL
//!    texture, where each pixel represents one cell.
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
use ref_thread_local::RefThreadLocal;

mod gl_quadtree;
mod shaders;
mod textures;
mod vbos;
mod vertices;

use super::*;
use crate::automaton::space::*;
use gl_quadtree::CachedGlQuadtree;
use vertices::*;

/// Minimum zoom power to draw gridlines. 2.0 = 4 pixels per cell.
pub const MIN_GRIDLINE_ZOOM_POWER: f64 = 2.0;
/// Number of zoom levels over which to fade gridlines in. 3.0 = three zoom levels
pub const GRIDLINE_FADE_RANGE: f64 = 3.0;

/// The color of the grid. This will be configurable in the future.
const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
/// The color given to the highlighted cell. This will be configurable in the
/// future.
const GRID_HIGHLIGHT_COLOR: [f32; 4] = [0.0, 0.5, 1.0, 1.0];
/// The color for dead cells. This will be configurable in the future.
const DEAD_COLOR: (u8, u8, u8) = (0, 0, 0);
/// The color for live cells. This will be configurable in the future.
const LIVE_COLOR: (u8, u8, u8) = (255, 255, 255);

/// The number of gridlines in each render batch.
const GRIDLINE_BATCH_SIZE: usize = 256;

#[derive(Default)]
pub struct RenderCache {
    gl_quadtree: CachedGlQuadtree<u8>,
}

pub struct RenderInProgress<'a> {
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
    /// The render cell position within quadtree_slice that is centered on the
    /// screen.
    pos: FVec2D,
    /// A rectangle of render cells within quadtree_slice that is visible.
    visible_rect: IRect2D,
    /// The view matrix converting from quadtree_slice space (1 unit = 1 render
    /// cell; (0, 0) = bottom left) to screen space ((-1, -1) = bottom left; (1,
    /// 1) = top right).
    view_matrix: [[f32; 4]; 4],
    /// Cached render data unique to the given GridView.
    cache: &'a mut RenderCache,
}
impl<'a> RenderInProgress<'a> {
    /// Performs preliminary computations and returns a RenderInProgress.
    pub fn new(g: &GridView2D, cache: &'a mut RenderCache, target: &'a mut glium::Frame) -> Self {
        let (target_w, target_h) = target.get_dimensions();
        let target_pixels_size: FVec2D = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let viewport = g.interpolating_viewport.clone();
        let mut zoom = viewport.zoom;

        // Compute the width of pixels for each individual cell.
        let cell_pixels: f64 = zoom.pixels_per_cell();

        // Compute the lowest layer that must be visited, which is the layer of
        // a "render cell," a node that is rendered as one unit (one pixel in
        // step #1).
        let render_cell_layer: usize = std::cmp::max(0, -zoom.ceil().power() as isize) as usize;
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

        // Get an NdCachedNode that covers the entire visible area, a rectangle
        // of visible cells relative to that node, and a floating-point render
        // cell position relative to that node that will be in the center of the
        // screen.
        let quadtree_slice: NdTreeSlice<u8, Dim2D>;
        let visible_rect: IRect2D;
        let pos: FVec2D;
        {
            // Before computing the rectangles relative to the slice, get the
            // rectangles in global space.
            let global_visible_rect: BigRect2D;
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
            }

            // Now fetch the NdTreeSlice containing all of the visible cells.
            quadtree_slice = g.automaton.get_projected_tree().get_slice_containing(
                // Convert chunk coordinates into normal cell coordinates.
                &global_visible_rect,
            );

            // Subtract the slice offset from global_visible_rect and
            // global_chunk_visible_rect so that they are relative to the slice.
            let mut tmp_visible_rect = global_visible_rect.clone() - &quadtree_slice.offset;
            // Divide by render_cell_len to get render cells.
            tmp_visible_rect = tmp_visible_rect.div_outward(&render_cell_len);
            // Now it is safe to convert from BigInt to isize.
            visible_rect = tmp_visible_rect.as_irect();

            // Subtract the slice offset from the viewport position and divide
            // by the size of a render cell to get the render cell offset from
            // the lower corner of the slice.
            let integer_pos = (&viewport.pos - &quadtree_slice.offset).div_floor(&render_cell_len);
            // viewport.offset is a sub-cell offset, so it only matters when
            // zoomed in more than 1:1.
            if viewport.zoom.pixels_per_cell() > 1.0 {
                pos = integer_pos.as_fvec() + viewport.offset;
            } else {
                pos = integer_pos.as_fvec();
            }
        }

        // Compute the render cell view matrix.
        let view_matrix: [[f32; 4]; 4];
        {
            let (target_w, target_h) = target.get_dimensions();
            let target_size: FVec2D = NdVec([r64(target_w as f64), r64(target_h as f64)]);
            let pixels_per_cell = r64(viewport.zoom.pixels_per_cell());
            // Multiply by 2 because the OpenGL screen space ranges from -1 to +1.
            let scale = FVec2D::repeat(pixels_per_cell) / target_size * r64(2.0);
            // Convert pos from render-cell-space to screen-space.
            let offset = pos * scale;
            let x_offset = offset[X].raw() as f32;
            let y_offset = offset[Y].raw() as f32;
            let x_scale = scale[X].raw() as f32;
            let y_scale = scale[Y].raw() as f32;

            view_matrix = [
                [x_scale, 0.0, 0.0, 0.0],
                [0.0, y_scale, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [-x_offset, -y_offset, 0.0, 1.0],
            ];
        }

        Self {
            viewport,
            target,
            render_cell_layer,
            render_cell_pixels,
            quadtree_slice,
            pos,
            visible_rect,
            view_matrix,
            cache,
        }
    }

    /// Draw the cells that appear in the viewport.
    pub fn draw_cells(&mut self) {
        let textures: &mut textures::TextureCache = &mut textures::CACHE.borrow_mut();
        // Steps #1: encode the quadtree as a 1D texture.
        let gl_quadtree = self.cache.gl_quadtree.from_node(
            self.quadtree_slice.root.clone(),
            self.render_cell_layer,
            Self::get_branch_pixel_color,
        );
        // Step #2: draw at 1 pixel per render cell, including only the cells
        // inside self.visible_rect.
        let unscaled_cells_w = self.visible_rect.len(X) as u32;
        let unscaled_cells_h = self.visible_rect.len(Y) as u32;
        let (_, mut unscaled_cells_fbo) = textures
            .unscaled_cells
            .at_size(unscaled_cells_w, unscaled_cells_h);
        unscaled_cells_fbo
            .draw(
                &*vbos::quadtree_quad_with_quadtree_coords(self.visible_rect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::quadtree(),
                &uniform! {
                    quadtree_texture: &gl_quadtree.texture,
                    max_layer: gl_quadtree.layers as i32,
                    root_idx: gl_quadtree.root_idx as u32,
                },
                &glium::DrawParameters::default(),
            )
            .expect("Failed to draw cells");

        // Step #3: resize that texture by an integer factor.
        let integer_scale_factor = self.render_cell_pixels.ceil();
        let visible_cells_w = self.visible_rect.len(X) as u32;
        let visible_cells_h = self.visible_rect.len(Y) as u32;
        let scaled_cells_w = visible_cells_w * integer_scale_factor as u32;
        let scaled_cells_h = visible_cells_h * integer_scale_factor as u32;
        let (scaled_cells_texture, scaled_cells_fbo) = textures
            .scaled_cells
            .at_size(scaled_cells_w, scaled_cells_h);
        scaled_cells_fbo.blit_from_simple_framebuffer(
            &unscaled_cells_fbo,
            &entire_rect(&unscaled_cells_fbo),
            &entire_blit_target(&scaled_cells_fbo),
            glium::uniforms::MagnifySamplerFilter::Nearest,
        );

        // Step #4: render that onto the screen.
        let (target_w, target_h) = self.target.get_dimensions();
        let target_size = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let render_cells_size = target_size / r64(self.render_cell_pixels);
        let render_cells_center = self.pos - self.visible_rect.min().as_fvec();
        let mut render_cells_frect =
            FRect2D::centered(render_cells_center, render_cells_size / 2.0);
        render_cells_frect /= self.visible_rect.size().as_fvec();

        self.target
            .draw(
                &*vbos::blit_quad_with_src_coords(render_cells_frect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::blit(),
                &uniform! {
                    src_texture: scaled_cells_texture.sampled(),
                    alpha: 1.0f32,
                },
                &glium::DrawParameters::default(),
            )
            .expect("Failed to draw cells");

        // // Draw a 1:1 "minimap" in the corner
        // self.target.blit_from_simple_framebuffer(
        //     &unscaled_cells_fbo,
        //     &entire_rect(&unscaled_cells_fbo),
        //     &entire_blit_target(&unscaled_cells_fbo),
        //     glium::uniforms::MagnifySamplerFilter::Linear,
        // );
    }
    fn get_branch_pixel_color(branch: &NdTreeBranch<u8, Dim2D>) -> [u8; 4] {
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
        [r as u8, g as u8, b as u8, 255]
    }

    pub fn with_gridlines_fbo<F: FnOnce(&mut Self, &mut glium::framebuffer::SimpleFrameBuffer)>(
        &mut self,
        alpha: f32,
        f: F,
    ) {
        let textures: &mut textures::TextureCache = &mut textures::CACHE.borrow_mut();
        let (target_w, target_h) = self.target.get_dimensions();
        let (gridlines_texture, mut gridlines_fbo) = textures.gridlines.at_size(target_w, target_h);
        gridlines_fbo.clear_color_srgb(0.0, 0.0, 0.0, 0.0);
        f(self, &mut gridlines_fbo);
        self.target
            .draw(
                &*vbos::blit_quad_with_src_coords(FRect2D::single_cell(NdVec::origin())),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::blit(),
                &uniform! {
                    src_texture: gridlines_texture.sampled(),
                    alpha: alpha,
                },
                &glium::DrawParameters {
                    blend: glium::Blend {
                        color: glium::BlendingFunction::Addition {
                            source: glium::LinearBlendingFactor::SourceAlpha,
                            destination: glium::LinearBlendingFactor::OneMinusSourceAlpha,
                        },
                        alpha: glium::BlendingFunction::Max,
                        constant_value: (0.0, 0.0, 0.0, 0.0),
                    },
                    ..Default::default()
                },
            )
            .expect("Failed to draw cells");
    }

    /// Draw the gridlines that appear in the viewport.
    pub fn draw_gridlines(&mut self, gridlines_fbo: &mut glium::framebuffer::SimpleFrameBuffer) {
        // Generate a pair of vertices for each gridline.
        let gridline_indices = glium::index::NoIndices(PrimitiveType::LinesList);
        let mut gridline_vertices: Vec<RgbaVertex>;
        {
            let w = self.visible_rect.len(X);
            let h = self.visible_rect.len(Y);

            gridline_vertices = Vec::with_capacity((w + h) as usize * 2);
            let min = self.visible_rect.min();
            let max = self.visible_rect.max() + 1;
            let min_x = min[X];
            let min_y = min[Y];
            let max_x = max[X];
            let max_y = max[Y];
            // Generate vertical gridlines.
            for x in self.visible_rect.axis_range(X) {
                gridline_vertices.push(([x, min_y], GRID_COLOR).into());
                gridline_vertices.push(([x, max_y], GRID_COLOR).into());
            }
            // Generate horizontal gridlines.
            for y in self.visible_rect.axis_range(Y) {
                gridline_vertices.push(([min_x, y], GRID_COLOR).into());
                gridline_vertices.push(([max_x, y], GRID_COLOR).into());
            }
        }

        // Draw the gridlines in batches, because the VBO might not be able to
        // hold all the points at once.
        for batch in gridline_vertices.chunks(GRIDLINE_BATCH_SIZE) {
            // Put the data in a slice of the VBO.
            let gridlines_vbo = vbos::gridlines();
            let vbo_slice = gridlines_vbo.slice(0..batch.len()).unwrap();
            vbo_slice.write(batch);
            // Draw gridlines.
            gridlines_fbo
                .draw(
                    vbo_slice,
                    &gridline_indices,
                    &shaders::lines(),
                    &uniform! {
                        matrix: self.view_matrix,
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

    /// Draws a highlight around the hovered cell and then returns its global
    /// coordinates.
    pub fn draw_hover_highlight(
        &mut self,
        gridlines_fbo: &mut glium::framebuffer::SimpleFrameBuffer,
        cursor_position: Option<IVec2D>,
    ) -> Option<BigVec2D> {
        if self.viewport.zoom.power() < 0.0 {
            return None;
        }
        let cursor_position = cursor_position?;
        let mut x = cursor_position[X];
        let mut y = cursor_position[Y];
        // Center the coordinates.
        let (target_w, target_h) = self.target.get_dimensions();
        x -= target_w as isize / 2;
        y -= target_h as isize / 2;
        // Glutin measures window coordinates from the top-left corner, but we
        // want the Y coordinate increasing upwards.
        y = -y;
        // Convert to float.
        let mut x = x as f64;
        let mut y = y as f64;
        // Convert to local cell space (relative to slice).
        x /= self.viewport.zoom.pixels_per_cell();
        y /= self.viewport.zoom.pixels_per_cell();
        x += self.pos[X].raw();
        y += self.pos[Y].raw();
        // Floor to integer.
        let hover_pos = NdVec([x.floor() as isize, y.floor() as isize]);
        // Convert to global cell space.
        let global_hover_pos = hover_pos.convert() + &self.quadtree_slice.offset;
        // Actually draw the highlight.
        self.internal_draw_hover_highlight(gridlines_fbo, hover_pos);
        Some(global_hover_pos)
    }
    fn internal_draw_hover_highlight(
        &mut self,
        gridlines_fbo: &mut glium::framebuffer::SimpleFrameBuffer,
        hover_pos: IVec2D,
    ) {
        let min = self.visible_rect.min();
        let max = self.visible_rect.max() + 1;
        let min_x = min[X] as f32;
        let min_y = min[Y] as f32;
        let max_x = max[X] as f32;
        let max_y = max[Y] as f32;
        let mut half_highlight_color = GRID_HIGHLIGHT_COLOR;
        half_highlight_color[3] *= 0.5;
        let mut dark_highlight_color = GRID_HIGHLIGHT_COLOR;
        dark_highlight_color[0] *= 0.5;
        dark_highlight_color[1] *= 0.5;
        dark_highlight_color[2] *= 0.5;
        dark_highlight_color[3] *= 0.75;
        let hover_x = hover_pos[X] as f32;
        let hover_y = hover_pos[Y] as f32;
        // Draw transparent fill.
        {
            // Generate vertices.
            let highlight_indices = glium::index::NoIndices(PrimitiveType::TriangleStrip);
            let highlight_vertices: Vec<RgbaVertex> = vec![
                ([hover_x, hover_y], dark_highlight_color).into(),
                ([hover_x + 1.0, hover_y], dark_highlight_color).into(),
                ([hover_x, hover_y + 1.0], dark_highlight_color).into(),
                ([hover_x + 1.0, hover_y + 1.0], dark_highlight_color).into(),
            ];
            // Put the data in a slice of the VBO.
            let gridlines_vbo = vbos::gridlines();
            let vbo_slice = gridlines_vbo.slice(0..highlight_vertices.len()).unwrap();
            vbo_slice.write(&highlight_vertices);
            // Draw.
            gridlines_fbo
                .draw(
                    vbo_slice,
                    &highlight_indices,
                    &shaders::lines(),
                    &uniform! {
                        matrix: self.view_matrix,
                    },
                    &glium::DrawParameters {
                        blend: glium::Blend::alpha_blending(),
                        ..Default::default()
                    },
                )
                .expect("Failed to draw cursor crosshairs");
        }

        // Draw crosshairs.
        {
            // Generate vertices.
            let highlight_indices = glium::index::NoIndices(PrimitiveType::LinesList);
            let mut highlight_vertices: Vec<RgbaVertex> = vec![];
            for &y in [hover_y, hover_y + 1.0].iter() {
                highlight_vertices.push(([min_x, y], half_highlight_color).into());
                highlight_vertices.push(([hover_x - 1.0, y], half_highlight_color).into());
                highlight_vertices.push(([hover_x - 1.0, y], half_highlight_color).into());
                highlight_vertices.push(([hover_x, y], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([hover_x, y], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([hover_x + 1.0, y], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([hover_x + 1.0, y], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([hover_x + 2.0, y], half_highlight_color).into());
                highlight_vertices.push(([hover_x + 2.0, y], half_highlight_color).into());
                highlight_vertices.push(([max_x, y], half_highlight_color).into());
            }
            for &x in [hover_x, hover_x + 1.0].iter() {
                highlight_vertices.push(([x, min_y], half_highlight_color).into());
                highlight_vertices.push(([x, hover_y - 1.0], half_highlight_color).into());
                highlight_vertices.push(([x, hover_y - 1.0], half_highlight_color).into());
                highlight_vertices.push(([x, hover_y], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([x, hover_y], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([x, hover_y + 1.0], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([x, hover_y + 1.0], GRID_HIGHLIGHT_COLOR).into());
                highlight_vertices.push(([x, hover_y + 2.0], half_highlight_color).into());
                highlight_vertices.push(([x, hover_y + 2.0], half_highlight_color).into());
                highlight_vertices.push(([x, max_y], half_highlight_color).into());
            }

            // Put the data in a slice of the VBO.
            let gridlines_vbo = vbos::gridlines();
            let vbo_slice = gridlines_vbo.slice(0..highlight_vertices.len()).unwrap();
            vbo_slice.write(&highlight_vertices);
            // Draw.
            gridlines_fbo
                .draw(
                    vbo_slice,
                    &highlight_indices,
                    &shaders::lines(),
                    &uniform! {
                        matrix: self.view_matrix,
                    },
                    &glium::DrawParameters {
                        line_width: Some(1.0),
                        blend: glium::Blend::alpha_blending(),
                        ..Default::default()
                    },
                )
                .expect("Failed to draw cursor crosshairs");
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
