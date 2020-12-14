//! 2D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add icons in
//! the future.
//!
//! Not including preliminary computations and extra effects like gridlines,
//! there are four main stages to rendering a grid of cells:
//!
//! 1. Create an indexed quadtree of render cells encoded in an OpenGL texture.
//!    (A "render cell" is a node of the quadtree which is rendered as single
//!    square; this is one cell when zoomed in, but may be larger when zoomed
//!    out.)
//! 2. Render the visible portion of that quadtree into an OpenGL texture, where
//!    each pixel represents one cell.
//! 3. Blit that texture onto the screen using the "pixel mixing" scaling
//!    technique; see http://entropymine.com/imageworsener/pixelmixing/ for more
//!    info.
//!
//! There is also some subpixel adjustment happening in `Camera2D`. The position
//! is offset by 0.5 pixels when the viewport dimensions are odd, and everything
//! except cells are offset by 0.25 pixels to force consistent rounding behavior
//! for features that are directly between cells.

use anyhow::{Context, Result};
use glium::index::PrimitiveType;
use glium::{uniform, Surface};
use itertools::Itertools;
use std::cell::RefMut;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

use super::consts::*;
use super::gl_quadtree::CachedGlQuadtree;
use super::shaders;
use super::vertices::{MouseTargetVertex, RgbaVertex};
use crate::config::{MouseDisplay, MouseDragBinding};
use crate::gridview::*;
use crate::Scale;

#[derive(Default)]
pub struct RenderCache {
    /// Texture that encodes a quadtree of visbile render cells.
    gl_quadtree: CachedGlQuadtree,
}

pub struct RenderInProgress<'a> {
    /// Global lock on cached render data.
    cache: RefMut<'a, super::RenderCache>,
    /// Camera to render the grid from.
    camera: Camera2D,
    /// Target to render to.
    target: &'a mut glium::Frame,
    /// User configuration.
    config: &'a Config,
    /// Mouse cursor state.
    mouse: MouseState,
    /// Node layer of a render cell.
    render_cell_layer: Layer,
    /// Scale to draw render cells at.
    render_cell_scale: Scale,
    /// Quadtree encompassing all visible cells.
    visible_quadtree: NdTreeSlice2D<'a>,
    /// Rectangle of render cells within `visible_quadtree` that is visible.
    visible_rect: IRect2D,
    /// Transform from `visible_quadtree` space (1 unit = 1 render cell; (0, 0)
    /// = bottom left) to screen space ((-1, -1) = bottom left; (1, 1) = top
    /// right) and pixel space (1 unit = 1 pixel; (0, 0) = top left).
    transform: CellTransform2D,
    /// Mouse targets, indexed by ID.
    mouse_targets: Vec<MouseTargetData>,
    /// Cached render data maintained by the `GridView2D`.
    render_cache_deprecated: &'a mut RenderCache,
}
impl<'a> RenderInProgress<'a> {
    /// Creates a `RenderInProgress` for a gridview.
    pub fn new(
        g: &'a GridView2D,
        RenderParams { target, config }: RenderParams<'a>,
        render_cache_deprecated: &'a mut RenderCache,
    ) -> Result<Self> {
        let mut cache = super::CACHE.borrow_mut();

        // Initialize depth buffer.
        target.clear_depth(0.0);

        // Initialize mouse picker.
        cache.picker.init(target.get_dimensions());

        let camera = g.camera().clone();

        // Determine the lowest layer of the quadtree that we must visited,
        // which is the layer of a "render cell," a quadtree node that is
        // rendered as one unit (one pixel in step #1).
        let (render_cell_layer, render_cell_scale) = camera.render_cell_layer_and_scale();
        // Compute the width of cells represented by each render cell.
        let render_cell_len = render_cell_layer.big_len();

        // Get a smaller slice of the grid that covers the entire visible area
        // and a rectangle containing all visible render cells relative to that
        // node.
        let visible_quadtree: NdTreeSlice2D<'a>;
        let visible_rect: IRect2D;
        {
            let (target_w, target_h) = camera.target_dimensions();
            let target_pixels_size: IVec2D = NdVec([target_w as isize, target_h as isize]);

            // Find the global rectangle of visible cells.
            let global_visible_rect: BigRect2D;
            {
                // Compute the width and height of individual cells that fit on
                // the screen.
                let target_cells_size: FixedVec2D = camera
                    .scale()
                    .units_to_cells(target_pixels_size.to_fixedvec());
                // Compute the cell vector pointing from the origin to the top
                // right corner of the screen; i.e. the "half diagonal."
                let half_diag: FixedVec2D = target_cells_size / 2.0;

                global_visible_rect =
                    BigRect2D::centered(camera.pos().floor().0, &half_diag.ceil().0);
            }

            // Now fetch the `NdTreeSlice` containing all of the visible cells.
            visible_quadtree = g.automaton.ndtree.slice_containing(&global_visible_rect);

            // Subtract the slice offset from `global_visible_rect` and
            // `global_visible_rect` to get the rectangle of visible cells
            // within the slice.
            let mut tmp_visible_rect = global_visible_rect - &visible_quadtree.offset;
            // Divide by `render_cell_len` to get render cells.
            tmp_visible_rect = tmp_visible_rect.div_outward(&render_cell_len);
            // Now it is safe to convert from `BigInt` to `isize`, because the
            // number of visible render cells must be reasonable.
            visible_rect = tmp_visible_rect.to_irect();
        }

        // Compute the transformation from individual cells all the way to
        // pixels.
        let transform = camera.cell_transform_with_base(visible_quadtree.offset.clone())?;

        let mouse_targets = vec![];

        Ok(Self {
            cache,
            camera,
            target,
            config,
            mouse: g.mouse(),
            render_cell_layer,
            render_cell_scale,
            visible_quadtree,
            visible_rect,
            transform,
            mouse_targets,
            render_cache_deprecated,
        })
    }

    /// Returns a `RenderResult` from this render.
    pub fn finish(self) -> RenderResult {
        RenderResult {
            mouse_target: self
                .mouse
                .pos
                // Convert mouse position to `u32`.
                .and_then(|pos| pos[X].to_u32().zip(pos[Y].to_u32()))
                // Get mouse target ID underneath cursor.
                .map(|cursor_pos| self.cache.picker.get_pixel(cursor_pos) as usize)
                // Get mouse target using that ID (subtract 1 because 0 means no
                // target).
                .and_then(|i| self.mouse_targets.get(i.checked_sub(1)?))
                .cloned(),
        }
    }

    pub fn cell_transform(&self) -> &CellTransform2D {
        &self.transform
    }

    /// Draw the cells that appear in the viewport.
    pub fn draw_cells(&mut self) -> Result<()> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        // Steps #1: encode the quadtree as a texture.
        let gl_quadtree = self.render_cache_deprecated.gl_quadtree.from_node(
            (&self.visible_quadtree.root).into(),
            self.render_cell_layer,
            Self::node_pixel_color,
        )?;
        // Step #2: draw at 1 pixel per render cell, including only the cells
        // inside `self.visible_rect`.
        let cells_w = self.visible_rect.len(X) as u32;
        let cells_h = self.visible_rect.len(Y) as u32;
        let (cells_texture, mut cells_fbo, cells_texture_viewport) =
            cache.textures.cells(cells_w, cells_h);
        cells_fbo
            .draw(
                &*vbos.quadtree_quad_with_quadtree_coords(self.visible_rect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::QUADTREE,
                &uniform! {
                    quadtree_texture: &gl_quadtree.texture,
                    layer_count: gl_quadtree.layers as i32,
                    root_idx: gl_quadtree.root_idx as u32,
                },
                &glium::DrawParameters {
                    viewport: Some(cells_texture_viewport),
                    ..Default::default()
                },
            )
            .context("cells_fbo.draw()")?;

        // Step #3: scale and render that onto the screen.
        let (target_w, target_h) = self.target.get_dimensions();
        let target_size = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let render_cells_size = target_size / self.render_cell_scale.units_per_cell();
        let render_cells_center = self.camera.render_cell_pos(&self.visible_quadtree.offset)
            - self.visible_rect.min().to_fvec();
        let render_cells_rect = FRect2D::centered(render_cells_center, render_cells_size / 2.0);
        let texture_coords_rect = render_cells_rect / self.visible_rect.size().to_fvec();

        self.target
            .draw(
                &*vbos.blit_quad_with_src_coords(texture_coords_rect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::PIXMIX,
                &uniform! {
                    alpha: 1.0_f32,
                    src_texture: cells_texture.sampled(),
                    scale_factor: self.render_cell_scale.units_per_cell().raw() as f32,
                    active_tex_size: (cells_w as f32, cells_h as f32)
                },
                &glium::DrawParameters::default(),
            )
            .context("Drawing cells to target")?;

        // // Draw a 1:1 "minimap" in the corner
        // self.target.blit_from_simple_framebuffer(
        //     &cells_fbo,
        //     &entire_rect(&cells_fbo),
        //     &entire_blit_target(&cells_fbo),
        //     glium::uniforms::MagnifySamplerFilter::Linear,
        // );

        Ok(())
    }

    /// Draws gridlines at varying opacity and spacing depending on scaling.
    ///
    /// TOOD: support arbitrary exponential base and factor (a*b^n for any a, b)
    pub fn draw_gridlines(&mut self, width: f64) -> Result<()> {
        // Compute the minimum pixel spacing between maximum-opacity gridlines.
        let log2_max_pixel_spacing = r64(MAX_GRIDLINE_SPACING).log2();
        // Compute the cell spacing between the gridlines that will be drawn
        // with the maximum opacity.
        let log2_cell_spacing = log2_max_pixel_spacing - self.camera.scale().log2_factor();
        // Round up to the nearest power of GRIDLINE_SPACING_BASE.
        let log2_spacing_base = (GRIDLINE_SPACING_BASE as f64).log2();
        let log2_cell_spacing = (log2_cell_spacing / log2_spacing_base).ceil() * log2_spacing_base;
        // Convert from cells to render cells.
        let log2_render_cell_spacing = log2_cell_spacing - self.render_cell_layer.to_u32() as f64;
        let mut spacing = log2_render_cell_spacing.exp2().raw() as usize;
        // Convert from render cells to pixels.
        let mut pixel_spacing = spacing as f64 * self.render_cell_scale.units_per_cell().raw();

        while spacing > 0 && pixel_spacing > MIN_GRIDLINE_SPACING {
            // Compute grid color, including alpha.
            let mut color = crate::colors::GRIDLINES;
            let alpha = gridline_alpha(pixel_spacing, self.camera.scale()) as f32;
            color[3] *= alpha;
            // Draw gridlines with the given spacing.
            let offset = (-self.visible_rect.min()).mod_floor(&(spacing as isize));
            self.draw_cell_overlay_rects(
                &self.generate_solid_cell_borders(
                    self.visible_rect
                        .axis_range(X)
                        .skip(offset[X] as usize)
                        .step_by(spacing),
                    self.visible_rect
                        .axis_range(Y)
                        .skip(offset[Y] as usize)
                        .step_by(spacing),
                    GRIDLINE_DEPTH,
                    width,
                    color,
                ),
            )
            .context("Drawing gridlines")?;
            // Decrease the spacing.
            spacing /= GRIDLINE_SPACING_BASE;
            pixel_spacing /= GRIDLINE_SPACING_BASE as f64;
        }
        Ok(())
    }

    /// Draws a highlight on the render cell under the mouse cursor.
    pub fn draw_hover_highlight(
        &mut self,
        cell_pos: &BigVec2D,
        width: f64,
        color: [f32; 4],
    ) -> Result<()> {
        self.draw_cell_overlay_rects(&self.generate_cell_rect_outline(
            IRect2D::single_cell(self.clip_cell_pos_to_visible_render_cells(cell_pos)),
            CURSOR_DEPTH,
            width,
            color,
            RectHighlightParams {
                fill: true,
                crosshairs: true,
            },
        ))
        .context("Drawing cursor highlight")
    }
    /// Draws a highlight around the selected rectangle.
    pub fn draw_selection_highlight(
        &mut self,
        selection_rect: BigRect2D,
        width: f64,
    ) -> Result<()> {
        let visible_selection_rect = self.clip_cell_rect_to_visible_render_cells(&selection_rect);

        self.draw_cell_overlay_rects(&self.generate_cell_rect_outline(
            visible_selection_rect,
            SELECTION_DEPTH,
            width,
            crate::colors::SELECTION,
            RectHighlightParams {
                fill: true,
                crosshairs: false,
            },
        ))
        .context("Drawing selection highlight")?;

        let click_target_width = self.config.ctrl.selection_resize_drag_target_width
            / self.render_cell_scale.factor().to_f64().unwrap();
        let (min, max) = (
            visible_selection_rect.min(),
            visible_selection_rect.max() + 1,
        );
        let xs = vec![
            r64(min[X] as f64 - click_target_width * 0.75),
            r64(min[X] as f64 + click_target_width * 0.25),
            r64(max[X] as f64 - click_target_width * 0.25),
            r64(max[X] as f64 + click_target_width * 0.75),
        ];
        let ys = vec![
            r64(min[Y] as f64 - click_target_width * 0.75),
            r64(min[Y] as f64 + click_target_width * 0.25),
            r64(max[Y] as f64 - click_target_width * 0.25),
            r64(max[Y] as f64 + click_target_width * 0.75),
        ];
        let x_indices = vec![0, 1, 2, 0, 2, 0, 1, 2];
        let y_indices = vec![0, 0, 0, 1, 1, 2, 2, 2];
        let mouse_displays = vec![
            MouseDisplay::ResizeNESW,
            MouseDisplay::ResizeNS,
            MouseDisplay::ResizeNWSE,
            MouseDisplay::ResizeEW,
            MouseDisplay::ResizeEW,
            MouseDisplay::ResizeNWSE,
            MouseDisplay::ResizeNS,
            MouseDisplay::ResizeNESW,
        ];
        let mut verts = Vec::with_capacity(4 * 8);
        for ((xi, yi), display) in x_indices.into_iter().zip(y_indices).zip(mouse_displays) {
            let mut axes = AxisSet::empty();
            if xi != 1 {
                axes.add(X);
            }
            if yi != 1 {
                axes.add(Y);
            }
            let binding = Some(MouseDragBinding::Select(
                SelectDragCommand::Resize { axes, plane: None }.into(),
            ));
            verts.extend_from_slice(&self.add_mouse_target(
                FRect::span(NdVec([xs[xi], ys[yi]]), NdVec([xs[xi + 1], ys[yi + 1]])),
                MouseTargetData { binding, display },
            ));
        }

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let ibos = &mut cache.ibos;
        let vbos = &mut cache.vbos;

        let vbo = vbos.mouse_target_verts();
        let vbo_slice = vbo.slice(0..(4 * 8)).unwrap();
        vbo_slice.write(&verts);
        let (mut picker_fbo, picker_viewport) = cache.picker.fbo();
        picker_fbo
            .draw(
                vbo_slice,
                &ibos.rect_indices(8),
                &shaders::PICKER,
                &uniform! { matrix: self.transform.gl_matrix() },
                &glium::DrawParameters {
                    viewport: Some(picker_viewport),
                    ..Default::default()
                },
            )
            .context("Computing selection mouse targets")?;

        Ok(())
    }
    /// Draws a highlight indicating how the selection will be resized.
    pub fn draw_absolute_selection_resize_preview(
        &mut self,
        selection_rect: BigRect2D,
        mouse_pos: &ScreenPos2D,
        width: f64,
    ) -> Result<()> {
        let selection_preview_rect = selection::resize_selection_absolute(
            &selection_rect,
            mouse_pos.cell(),
            mouse_pos.cell(),
        );
        let visible_selection_preview_rect =
            self.clip_cell_rect_to_visible_render_cells(&selection_preview_rect);
        self.draw_cell_overlay_rects(&self.generate_cell_rect_outline(
            visible_selection_preview_rect,
            SELECTION_RESIZE_DEPTH,
            width,
            crate::colors::SELECTION_RESIZE,
            RectHighlightParams {
                fill: true,
                crosshairs: false,
            },
        ))
        .context("Drawing selection resize highlight")?;
        Ok(())
    }

    /// Returns the render cell position containing the global cell position if
    /// the cell is visible; otherwise, returns the position of the nearest
    /// render cell that is just off-screen.
    fn clip_cell_pos_to_visible_render_cells(&self, cell_pos: &BigVec2D) -> IVec2D {
        let render_cell_pos =
            (cell_pos - &self.visible_quadtree.offset).div_floor(&self.render_cell_layer.big_len());
        // Clip to lower edge minus 2 cells for padding.
        let render_cell_pos =
            NdVec::max(&render_cell_pos, &(self.visible_rect.min() - 2).to_bigvec());
        // Clip to upper edge plus 2 cells for padding.
        let render_cell_pos =
            NdVec::min(&render_cell_pos, &(self.visible_rect.max() + 2).to_bigvec());

        render_cell_pos.to_ivec()
    }

    /// Clips the edges of a rectangle using
    /// `clip_cell_pos_to_visible_render_cells()`.
    fn clip_cell_rect_to_visible_render_cells(&self, cells_rect: &BigRect2D) -> IRect2D {
        NdRect::span(
            self.clip_cell_pos_to_visible_render_cells(&cells_rect.min()),
            self.clip_cell_pos_to_visible_render_cells(&cells_rect.max()),
        )
    }

    /// Generates a cell overlay to outline the given cell rectangle, with
    /// optional fill and crosshairs.
    #[must_use = "This method only generates the rectangles; call `draw_cell_overlay_rects` to draw them"]
    fn generate_cell_rect_outline(
        &self,
        rect: IRect2D,
        z: f32,
        width: f64,
        color: [f32; 4],
        params: RectHighlightParams,
    ) -> Vec<CellOverlayRect> {
        let bright_color = color;
        let mut dull_color = color;
        dull_color[3] *= 0.25;
        let mut fill_color = color;
        fill_color[0] *= 0.5;
        fill_color[1] *= 0.5;
        fill_color[2] *= 0.5;
        fill_color[3] *= 0.75;

        let NdVec([min_x, min_y]) = self.visible_rect.min();
        let NdVec([max_x, max_y]) = self.visible_rect.max() + 1;

        // If there are more than 1.5 pixels per render cell, the upper boundary
        // should be *between* cells (+1). If there are fewer, the upper
        // boundary should be *on* the cell (+0).
        let pixels_per_cell = self.render_cell_scale.units_per_cell();
        let a = rect.min();
        let b = rect.max() + (pixels_per_cell > 1.5) as isize;
        let NdVec([ax, ay]) = a;
        let NdVec([bx, by]) = b;

        let mut h_stops = vec![
            (min_x, dull_color),
            (ax - 1, dull_color),
            (ax, bright_color),
            (bx, bright_color),
            (bx + 1, dull_color),
            (max_x, dull_color),
        ];
        let mut v_stops = vec![
            (min_y, dull_color),
            (ay - 1, dull_color),
            (ay, bright_color),
            (by, bright_color),
            (by + 1, dull_color),
            (max_y, dull_color),
        ];

        // Optionally remove crosshairs.
        if !params.crosshairs {
            h_stops = h_stops
                .into_iter()
                .filter(|&(_, color)| color == bright_color)
                .collect();
            v_stops = v_stops
                .into_iter()
                .filter(|&(_, color)| color == bright_color)
                .collect();
        }

        let mut ret = vec![];

        // In case the crosshairs/outline is transparent, render gridlines
        // beneath it. Draw order works in our favor here:
        // https://stackoverflow.com/a/20231235/4958484
        if params.crosshairs {
            ret.extend_from_slice(&self.generate_solid_cell_borders(
                vec![ax, bx],
                vec![ay, by],
                z - TINY_OFFSET,
                width,
                crate::colors::GRIDLINES,
            ));
        }

        // Generate lines.
        for &x in &[ax, bx] {
            ret.extend_from_slice(&self.generate_gradient_cell_border(
                v_stops.iter().map(|&(y, color)| (NdVec([x, y]), color)),
                z,
                width,
                Y,
            ));
        }
        for &y in &[ay, by] {
            ret.extend_from_slice(&self.generate_gradient_cell_border(
                h_stops.iter().map(|&(x, color)| (NdVec([x, y]), color)),
                z,
                width,
                X,
            ));
        }

        // Generate fill after lines.
        if params.fill {
            ret.push(CellOverlayRect {
                start: a,
                end: b,
                z,
                start_color: fill_color,
                end_color: fill_color,
                line_params: None,
            })
        }

        ret
    }

    /// Generates a cell overlay for solid borders along the given columns and
    /// rows.
    #[must_use = "This method only generates the rectangles; call `draw_cell_overlay_rects` to draw them"]
    fn generate_solid_cell_borders(
        &self,
        columns: impl IntoIterator<Item = isize>,
        rows: impl IntoIterator<Item = isize>,
        z: f32,
        width: f64,
        color: [f32; 4],
    ) -> Vec<CellOverlayRect> {
        let min = self.visible_rect.min();
        let max = self.visible_rect.max() + 1;
        let min_x = min[X];
        let min_y = min[Y];
        let max_x = max[X];
        let max_y = max[Y];

        let h_line_params = Some(LineParams {
            width,
            include_endpoints: true,
            axis: X,
        });
        let v_line_params = Some(LineParams {
            width,
            include_endpoints: true,
            axis: Y,
        });

        let mut ret = Vec::with_capacity(4 * self.visible_rect.size().sum() as usize);
        for x in columns {
            ret.push(CellOverlayRect {
                start: NdVec([x, min_y]),
                end: NdVec([x, max_y]),
                z,
                start_color: color,
                end_color: color,
                line_params: v_line_params,
            });
        }
        for y in rows {
            ret.push(CellOverlayRect {
                start: NdVec([min_x, y]),
                end: NdVec([max_x, y]),
                z,
                start_color: color,
                end_color: color,
                line_params: h_line_params,
            });
        }
        ret
    }

    /// Generates a cell overlay for a gradient cell border.
    #[must_use = "This method only generates the rectangles; call `draw_cell_overlay_rects` to draw them"]
    fn generate_gradient_cell_border(
        &self,
        stops: impl IntoIterator<Item = (IVec2D, [f32; 4])>,
        z: f32,
        width: f64,
        axis: Axis,
    ) -> Vec<CellOverlayRect> {
        // Generate a rectangle for each stop (so that there is a definitive
        // color at each point) AND a rectangle between each adjacent pair of
        // stops.
        let mut ret = vec![];
        let mut prev_stop = None;
        let btwn_stops_line_params = Some(LineParams {
            width,
            include_endpoints: false,
            axis,
        });
        let single_stop_line_params = Some(LineParams {
            width,
            include_endpoints: true,
            axis,
        });
        for stop in stops {
            let (pos, color) = stop;
            if let Some((prev_pos, prev_color)) = prev_stop {
                ret.push(CellOverlayRect {
                    start: prev_pos,
                    end: pos,
                    z,
                    start_color: prev_color,
                    end_color: color,
                    line_params: btwn_stops_line_params,
                });
            }
            ret.push(CellOverlayRect {
                start: pos,
                end: pos,
                z,
                start_color: color,
                end_color: color,
                line_params: single_stop_line_params,
            });
            prev_stop = Some(stop);
        }
        ret
    }

    /// Draws a cell overlay.
    fn draw_cell_overlay_rects(&mut self, rects: &[CellOverlayRect]) -> Result<()> {
        // Draw the rectangles in batches, because the VBO might not be able to
        // hold all the vertices at once.
        for rect_batch in rects
            .into_iter()
            .chunks(CELL_OVERLAY_BATCH_SIZE)
            .into_iter()
        {
            let rect_batch = rect_batch.into_iter().collect_vec();
            let count = rect_batch.len();
            // Generate vertices.
            let verts = rect_batch
                .iter()
                .flat_map(|&rect| rect.verts(self.render_cell_scale).to_vec())
                .collect_vec();

            // Reborrow is necessary in order to split borrow.
            let cache = &mut *self.cache;
            let ibos = &mut cache.ibos;
            let vbos = &mut cache.vbos;

            // Put the data in a slice of the VBO.
            let vbo = vbos.rgba_verts();
            let vbo_slice = vbo.slice(0..(4 * count)).unwrap();
            vbo_slice.write(&verts);
            // Draw rectangles.
            self.target
                .draw(
                    vbo_slice,
                    &ibos.rect_indices(count),
                    &shaders::RGBA,
                    &uniform! { matrix: self.transform.gl_matrix() },
                    &glium::DrawParameters {
                        blend: glium::Blend::alpha_blending(),
                        depth: glium::Depth {
                            test: glium::DepthTest::IfMore,
                            write: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )
                .context("Drawing cell-aligned rectangles")?;
        }
        Ok(())
    }

    /// Returns the color for a pixel representing the given node.
    fn node_pixel_color(node: NodeRef<'_, Dim2D>) -> [u8; 4] {
        let (r, g, b) = if node.layer() == Layer(0) {
            let cell_state = node.as_leaf().unwrap().cells()[0];
            match cell_state {
                0 => crate::colors::DEAD,
                1 => crate::colors::LIVE,
                i => colorous::TURBO
                    .eval_rational(257 - i as usize, 256)
                    .as_tuple(),
            }
        } else {
            let ratio = if node.is_empty() {
                0.0
            } else {
                // Multiply then divide by 255 to keep some precision.
                let population_ratio = (node.population() * 255_usize / node.big_num_cells())
                    .to_f64()
                    .unwrap()
                    / 255.0;
                // Bias so that 50% is the minimum brightness if there is any population.
                (population_ratio / 2.0) + 0.5
            };
            let r = ((crate::colors::LIVE.0 as f64).powf(2.0) * ratio
                + (crate::colors::DEAD.0 as f64).powf(2.0) * (1.0 - ratio))
                .powf(0.5);
            let g = ((crate::colors::LIVE.1 as f64).powf(2.0) * ratio
                + (crate::colors::DEAD.1 as f64).powf(2.0) * (1.0 - ratio))
                .powf(0.5);
            let b = ((crate::colors::LIVE.2 as f64).powf(2.0) * ratio
                + (crate::colors::DEAD.2 as f64).powf(2.0) * (1.0 - ratio))
                .powf(0.5);
            (r as u8, g as u8, b as u8)
        };
        [r, g, b, 255]
    }

    fn add_mouse_target(
        &mut self,
        cells: FRect2D,
        data: MouseTargetData,
    ) -> [MouseTargetVertex; 4] {
        self.mouse_targets.push(data);
        let target_id = self.mouse_targets.len() as u32; // IDs start at 1
        let NdVec([x1, y1]) = cells.min();
        let NdVec([x2, y2]) = cells.max();
        let z = 0.0;
        [
            MouseTargetVertex {
                pos: [x1.raw() as f32, y1.raw() as f32, z],
                target_id,
            },
            MouseTargetVertex {
                pos: [x2.raw() as f32, y1.raw() as f32, z],
                target_id,
            },
            MouseTargetVertex {
                pos: [x1.raw() as f32, y2.raw() as f32, z],
                target_id,
            },
            MouseTargetVertex {
                pos: [x2.raw() as f32, y2.raw() as f32, z],
                target_id,
            },
        ]
    }
}

fn gridline_alpha(pixel_spacing: f64, scale: Scale) -> f64 {
    // Fade maximum grid alpha as zooming out beyond 1 cell per pixel.
    let max_alpha = clamped_interpolate(
        scale.log2_factor().raw(),
        0.0,
        1.0,
        ZOOMED_OUT_MAX_GRID_ALPHA,
        1.0,
    );
    let alpha = clamped_interpolate(
        pixel_spacing.log2(),
        (MIN_GRIDLINE_SPACING).log2(),
        (MAX_GRIDLINE_SPACING).log2(),
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

/// Simple rectangle in a cell overlay.
///
/// Because `glLineWidth` is not supported on all platforms, we draw rectangles
/// to vary gridline width.
#[derive(Debug, Copy, Clone)]
struct CellOverlayRect {
    /// Start point of a line, or one corner of a rectangle.
    start: IVec2D,
    /// End point of a line, or other corner of a rectangle.
    end: IVec2D,
    /// Z order.
    z: f32,
    /// Color at the start of the line.
    start_color: [f32; 4],
    /// Color at the end of the line.
    end_color: [f32; 4],
    /// Optional parameters for lines.
    line_params: Option<LineParams>,
}
impl CellOverlayRect {
    fn solid_rect(rect: IRect2D, z: f32, color: [f32; 4]) -> Self {
        Self {
            start: rect.min(),
            end: rect.max() + 1,
            z,
            start_color: color,
            end_color: color,
            line_params: None,
        }
    }
    fn verts(self, render_cell_scale: Scale) -> [RgbaVertex; 4] {
        let mut a = self.start.to_fvec();
        let mut b = self.end.to_fvec();
        let mut colors = [
            self.start_color,
            self.start_color,
            self.end_color,
            self.end_color,
        ];
        if let Some(LineParams {
            width,
            include_endpoints,
            axis,
        }) = self.line_params
        {
            let width = width.round().max(1.0);
            // At this point, the rectangle should have zero width.
            let cells_per_pixel = render_cell_scale.cells_per_unit();
            let offset = FVec::repeat(cells_per_pixel * width / 2.0) * (b - a).signum();
            // Expand it in all directions, so now it has the correct width and
            // includes its endpoints.
            a -= offset;
            b += offset;
            // Now exclude the endpoints, if requested.
            if !include_endpoints {
                a[axis] += offset[axis] * 2.0;
                b[axis] -= offset[axis] * 2.0;
            }
            if axis == X {
                // Use horizontal gradient instead of vertical gradient.
                colors.swap(1, 2);
            }
        }
        let ax = a[X].to_f32().unwrap();
        let ay = a[Y].to_f32().unwrap();
        let bx = b[X].to_f32().unwrap();
        let by = b[Y].to_f32().unwrap();
        [
            RgbaVertex::from(([ax, ay, self.z], colors[0])),
            RgbaVertex::from(([bx, ay, self.z], colors[1])),
            RgbaVertex::from(([ax, by, self.z], colors[2])),
            RgbaVertex::from(([bx, by, self.z], colors[3])),
        ]
    }
}

#[derive(Debug, Copy, Clone)]
struct LineParams {
    /// Line width.
    pub width: f64,
    /// Whether to include the squares at the endpoints of this line.
    pub include_endpoints: bool,
    /// The axis this line is along.
    pub axis: Axis,
}

#[derive(Debug, Copy, Clone)]
struct RectHighlightParams {
    pub fill: bool,
    pub crosshairs: bool,
}
