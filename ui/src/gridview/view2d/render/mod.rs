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
use itertools::Itertools;
use noisy_float::prelude::r64;
use num::{BigInt, ToPrimitive, Zero};

use ndcell_core::space::*;

mod gl_quadtree;
mod shaders;
mod textures;
mod vbos;
mod vertices;

use super::*;
use gl_quadtree::CachedGlQuadtree;
use vertices::*;

/// Exponential base to use when fading out gridlines. 16 = 16 small gridlines
/// between each large gridline.
pub const GRIDLINE_SPACING_BASE: usize = 16;
/// Minimum number of pixels between gridlines.
pub const MIN_GRIDLINE_SPACING: usize = 4;
/// Minimum number of pixels between gridlines with full opacity.
pub const MAX_GRIDLINE_SPACING: usize = 64;
// Maximum opacity of gridlines when zoomed out beyond one cell per pixel.
pub const ZOOMED_OUT_MAX_GRID_ALPHA: f64 = 0.75;

/// Color of the grid. This will be configurable in the future.
const GRID_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 1.0];
/// Color given to the highlighted cell. This will be configurable in the
/// future.
const GRID_HIGHLIGHT_COLOR: [f32; 4] = [0.0, 0.5, 1.0, 1.0];
/// Color for dead cells. This will be configurable in the future.
const DEAD_COLOR: (u8, u8, u8) = (0, 0, 0);
/// Color for live cells. This will be configurable in the future.
const LIVE_COLOR: (u8, u8, u8) = (255, 255, 255);

/// Number of gridlines in each render batch.
const GRIDLINE_BATCH_SIZE: usize = 256;

/// Depth at which to render gridlines.
const GRIDLINE_DEPTH: f32 = 0.1;
/// Depth at which to render highlight/crosshairs.
const CROSSHAIR_DEPTH: f32 = 0.2;

/// A small offset to apply to depth values to force correct Z order.
const TINY_DEPTH_OFFSET: f32 = 0.001;

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
    /// The zoom level to draw render cells at.
    render_cell_zoom: Zoom2D,
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
        target.clear_depth(0.0);

        let (target_w, target_h) = target.get_dimensions();
        let target_pixels_size: FVec2D = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let viewport = g.interpolating_viewport.clone();

        // Compute the width of pixels for each individual cell.
        let cell_pixels: f64 = viewport.zoom.pixels_per_cell();

        // Compute the lowest layer that must be visited, which is the layer of
        // a "render cell," a node that is rendered as one unit (one pixel in
        // step #1).
        let render_cell_layer: usize =
            std::cmp::max(0, -viewport.zoom.round().power() as isize) as usize;
        // Compute the width of cells represented by each render cell.
        let render_cell_len: BigInt = BigInt::from(1) << render_cell_layer;
        // The render cell layer is also the power of two we should subtract from
        // the zoom level.
        let render_cell_zoom = Zoom2D::from_power(viewport.zoom.power() + render_cell_layer as f64);
        // Now the zoom power should be greater than -1.0 (since -1.0 or
        // anything lower should be handled by having a higher render cell
        // layer).
        assert!(render_cell_zoom.power() > -1.0);

        // Get an NdCachedNode that covers the entire visible area, a rectangle
        // of visible cells relative to that node, and a floating-point render
        // cell position relative to that node that will be in the center of the
        // screen.
        let quadtree_slice: NdTreeSlice<u8, Dim2D>;
        let visible_rect: IRect2D;
        let mut pos: FVec2D;
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
            pos = integer_pos.as_fvec();
            // Offset by half a pixel if the viewport dimensions are odd, so
            // that cells boundaries always line up with pixel boundaries.
            pos += (target_pixels_size % 2.0) / 2.0;
            // viewport.offset is a sub-cell offset, so it only matters when
            // zoomed in more than 1:1.
            if viewport.zoom.pixels_per_cell() > 1.0 {
                pos += viewport.offset;
            }
        }

        // Compute the render cell view matrix, used for rendering gridlines.
        let view_matrix: [[f32; 4]; 4];
        {
            let pixels_per_cell = r64(render_cell_zoom.pixels_per_cell());
            // Multiply by 2 because the OpenGL screen space ranges from -1 to +1.
            let scale = FVec2D::repeat(pixels_per_cell) / target_pixels_size * r64(2.0);
            // Offset by half a pixel the zoom power is less than 0.5 (somewhere
            // between 1:1 and 1:2), so that gridlines are lined up perfectly
            // with the cells themselves instead of their boundaries.
            let subpixel_offset = if viewport.zoom.power() < 0.5 {
                -render_cell_zoom.cells_per_pixel() / 2.0
            } else {
                0.0
            };
            // Convert pos from render-cell-space to screen-space.
            let offset = (pos + r64(subpixel_offset)) * scale;
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
            render_cell_zoom,
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
                &shaders::QUADTREE,
                &uniform! {
                    quadtree_texture: &gl_quadtree.texture,
                    max_layer: gl_quadtree.layers as i32,
                    root_idx: gl_quadtree.root_idx as u32,
                },
                &glium::DrawParameters::default(),
            )
            .expect("Failed to draw cells");

        // Step #3: resize that texture by an integer factor.
        // Compute the width of pixels for each render cell.
        let pixels_per_render_cell = self.render_cell_zoom.pixels_per_cell();
        let integer_scale_factor = pixels_per_render_cell.round();
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
        let render_cells_size = target_size / r64(self.render_cell_zoom.pixels_per_cell());
        let render_cells_center = self.pos - self.visible_rect.min().as_fvec();
        let mut render_cells_frect =
            FRect2D::centered(render_cells_center, render_cells_size / 2.0);
        render_cells_frect /= self.visible_rect.size().as_fvec();

        self.target
            .draw(
                &*vbos::blit_quad_with_src_coords(render_cells_frect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::BLIT,
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

    /// Returns the coordinates of the cell at the given pixel, or None if the
    /// viewport is zoomed out beyond 1:1.
    pub fn pixel_pos_to_cell_pos(&self, cursor_position: IVec2D) -> Option<BigVec2D> {
        if self.viewport.zoom.power() < 0.0 {
            return None;
        }
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
        let local_pos = NdVec([x.floor() as isize, y.floor() as isize]);
        // Convert to global cell space.
        let global_pos = local_pos.convert() + &self.quadtree_slice.offset;
        Some(global_pos)
    }

    /// Draw the gridlines that appear in the viewport.
    pub fn draw_gridlines(&mut self, width: f32) {
        // Compute the minimum pixel spacing between maximum-opacity gridlines.
        let pixel_spacing = MAX_GRIDLINE_SPACING as f64;
        // Compute the cell spacing between the gridlines that will be drawn
        // with the maximum opacity.
        let cell_spacing = pixel_spacing / self.viewport.zoom.factor();
        // Round up to the nearest power of GRIDLINE_SPACING_BASE.
        let spacing_base_log2 = (GRIDLINE_SPACING_BASE as f64).log2();
        let cell_spacing =
            2.0f64.powf((cell_spacing.log2() / spacing_base_log2).ceil() * spacing_base_log2);
        // Convert from cells to render cells.
        let render_cell_spacing = cell_spacing / 2.0f64.powf(self.render_cell_layer as f64);
        let mut spacing = render_cell_spacing as usize;
        // Convert from render cells to pixels.
        let mut pixel_spacing =
            render_cell_spacing as f64 * self.render_cell_zoom.pixels_per_cell();

        while spacing > 0 && pixel_spacing > MIN_GRIDLINE_SPACING as f64 {
            // Compute grid color, including alpha.
            let mut color = GRID_COLOR;
            let alpha = gridline_alpha(pixel_spacing, self.viewport.zoom) as f32;
            color[3] *= alpha;
            // Draw gridlines with the given spacing.
            let offset = (-self.visible_rect.min()).mod_floor(&(spacing as isize));
            self.draw_cell_borders(
                &self
                    .visible_rect
                    .axis_range(X)
                    .skip(offset[X] as usize)
                    .step_by(spacing)
                    .collect_vec(),
                &self
                    .visible_rect
                    .axis_range(Y)
                    .skip(offset[Y] as usize)
                    .step_by(spacing)
                    .collect_vec(),
                color,
                width,
                None,
                None,
                GRIDLINE_DEPTH,
            );
            // Decrease the spacing.
            spacing /= GRIDLINE_SPACING_BASE;
            pixel_spacing /= GRIDLINE_SPACING_BASE as f64;
        }
    }

    /// Draws a highlight around the given cell.
    pub fn draw_blue_crosshairs_highlight(
        &mut self,
        cell_position: &BigVec2D,
        width: f32,
        draw_gridline_backing: bool,
    ) {
        let local_pos: IVec2D = (cell_position - &self.quadtree_slice.offset).as_ivec();
        let mut fill_color = GRID_HIGHLIGHT_COLOR;
        fill_color[0] *= 0.5;
        fill_color[1] *= 0.5;
        fill_color[2] *= 0.5;
        fill_color[3] *= 0.75;
        let x = local_pos[X];
        let y = local_pos[Y];
        let highlight_rect = IRect2D::single_cell(local_pos);

        // Only draw two crosshairs (vertical & horizontal) if we are zoomed out
        // past 1:1.5 scale.
        let mut xs = vec![x];
        let mut ys = vec![y];
        if self.viewport.zoom.factor() > 1.5 {
            xs.push(x + 1);
            ys.push(y + 1);
        }

        if draw_gridline_backing {
            // Redraw normal gridlines to ensure they are full opacity and the
            // same width as the crosshairs.
            self.draw_cell_borders(
                &xs,
                &ys,
                GRID_COLOR,
                width,
                None,
                None,
                GRIDLINE_DEPTH + TINY_DEPTH_OFFSET,
            );
        }
        // Now draw the crosshairs and highlight.
        self.draw_cell_borders(
            &xs,
            &ys,
            GRID_HIGHLIGHT_COLOR,
            width,
            Some(highlight_rect),
            Some(fill_color),
            CROSSHAIR_DEPTH,
        );
    }

    fn draw_cell_borders(
        &mut self,
        columns: &[isize],
        rows: &[isize],
        color: [f32; 4],
        line_width: f32,
        highlight_range: Option<IRect2D>,
        highlight_fill_color: Option<[f32; 4]>,
        z: f32,
    ) {
        let mut base_color = color;
        if highlight_range.is_some() {
            base_color[3] *= 0.25;
        }

        // Generate a pair of vertices for each gridline.
        let gridline_indices = glium::index::NoIndices(PrimitiveType::LinesList);
        let mut gridline_vertices: Vec<RgbaVertex>;
        {
            gridline_vertices = Vec::with_capacity((columns.len() + rows.len()) as usize * 2);
            let min = self.visible_rect.min();
            let max = self.visible_rect.max() + 1;
            let min_x = min[X];
            let min_y = min[Y];
            let max_x = max[X];
            let max_y = max[Y];
            let z2 = z + TINY_DEPTH_OFFSET;
            // Generate vertical gridlines.
            for &x in columns {
                gridline_vertices.push(([x, min_y], z, base_color).into());
                if let Some(rect) = highlight_range {
                    let y1 = rect.min()[Y];
                    gridline_vertices.push(([x, y1 - 1], z, base_color).into());
                    gridline_vertices.push(([x, y1 - 1], z, base_color).into());
                    gridline_vertices.push(([x, y1], z, color).into());
                    gridline_vertices.push(([x, y1], z2, color).into());
                    let y2 = rect.max()[Y];
                    gridline_vertices.push(([x, y2 + 1], z2, color).into());
                    gridline_vertices.push(([x, y2 + 1], z, color).into());
                    gridline_vertices.push(([x, y2 + 2], z, base_color).into());
                    gridline_vertices.push(([x, y2 + 2], z, base_color).into());
                }
                gridline_vertices.push(([x, max_y], z, base_color).into());
            }
            // Generate horizontal gridlines.
            for &y in rows {
                gridline_vertices.push(([min_x, y], z, base_color).into());
                if let Some(rect) = highlight_range {
                    let x1 = rect.min()[X];
                    gridline_vertices.push(([x1 - 1, y], z, base_color).into());
                    gridline_vertices.push(([x1 - 1, y], z, base_color).into());
                    gridline_vertices.push(([x1, y], z, color).into());
                    gridline_vertices.push(([x1, y], z2, color).into());
                    let x2 = rect.max()[X];
                    gridline_vertices.push(([x2 + 1, y], z2, color).into());
                    gridline_vertices.push(([x2 + 1, y], z, color).into());
                    gridline_vertices.push(([x2 + 2, y], z, base_color).into());
                    gridline_vertices.push(([x2 + 2, y], z, base_color).into());
                }
                gridline_vertices.push(([max_x, y], z, base_color).into());
            }
        }

        let uniform = uniform! { matrix: self.view_matrix };
        let draw_params = glium::DrawParameters {
            line_width: Some(line_width),
            blend: glium::Blend::alpha_blending(),
            depth: glium::Depth {
                test: glium::DepthTest::IfMore,
                write: true,
                ..Default::default()
            },
            ..Default::default()
        };

        // Draw the gridlines in batches, because the VBO might not be able to
        // hold all the points at once.
        for batch in gridline_vertices.chunks(GRIDLINE_BATCH_SIZE) {
            // Put the data in a slice of the VBO.
            let gridlines_vbo = vbos::gridlines();
            let vbo_slice = gridlines_vbo.slice(0..batch.len()).unwrap();
            vbo_slice.write(batch);
            // Draw gridlines.
            self.target
                .draw(
                    vbo_slice,
                    &gridline_indices,
                    &shaders::POINTS,
                    &uniform,
                    &draw_params,
                )
                .expect("Failed to draw cell borders");
        }

        // Draw the highlight fill.
        if let (Some(rect), Some(fill_color)) = (highlight_range, highlight_fill_color) {
            // Generate vertices.
            let fill_indices = glium::index::NoIndices(PrimitiveType::TriangleStrip);
            let highlight_vertices: Vec<RgbaVertex> = vec![
                ([rect.min()[X], rect.min()[Y]], z, fill_color).into(),
                ([rect.max()[X] + 1, rect.min()[Y]], z, fill_color).into(),
                ([rect.min()[X], rect.max()[Y] + 1], z, fill_color).into(),
                ([rect.max()[X] + 1, rect.max()[Y] + 1], z, fill_color).into(),
            ];
            // Put the data in a slice of the VBO.
            let gridlines_vbo = vbos::gridlines();
            let vbo_slice = gridlines_vbo.slice(0..highlight_vertices.len()).unwrap();
            vbo_slice.write(&highlight_vertices);
            // Draw highlight fill.
            self.target
                .draw(
                    vbo_slice,
                    &fill_indices,
                    &shaders::POINTS,
                    &uniform,
                    &draw_params,
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

fn gridline_alpha(pixel_spacing: f64, zoom: Zoom2D) -> f64 {
    // Fade maximum grid alpha as zooming out beyond 1 cell per pixel.
    let max_alpha = clamped_interpolate(zoom.power(), 0.0, 1.0, ZOOMED_OUT_MAX_GRID_ALPHA, 1.0);
    let alpha = clamped_interpolate(
        pixel_spacing.log2(),
        (MIN_GRIDLINE_SPACING as f64).log2(),
        (MAX_GRIDLINE_SPACING as f64).log2(),
        0.0,
        1.0,
    );
    // Clamp to max alpha.
    if alpha > max_alpha {
        max_alpha
    } else {
        alpha
    }
}

fn clamped_interpolate(x: f64, min: f64, max: f64, min_result: f64, max_result: f64) -> f64 {
    if x < min {
        return min_result;
    }
    if x > max {
        return max_result;
    }
    let progress = (x - min) / (max - min);
    min_result + (max_result - min_result) * progress
}
