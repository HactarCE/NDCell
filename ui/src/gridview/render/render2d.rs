//! 2D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add icons in
//! the future.
//!
//! Not including preliminary computations and extra effects like gridlines,
//! there are three main stages of rendering a grid of cells:
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
use glium::glutin::event::ModifiersState;
use glium::index::PrimitiveType;
use glium::{uniform, Surface};

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::Vertex2D;
use super::CellDrawParams;
use crate::config::MouseDragBinding;
use crate::gridview::*;
use crate::mouse::MouseDisplay;
use crate::Scale;

pub(in crate::gridview) type GridViewRender2D<'a> = GenericGridViewRender<'a, RenderDim2D>;

#[derive(Default)]
pub(in crate::gridview) struct RenderDim2D;
impl GridViewRenderDimension<'_> for RenderDim2D {
    type D = Dim2D;
    type Camera = Camera2D;

    const DEFAULT_COLOR: (f32, f32, f32, f32) = crate::colors::BACKGROUND_2D;
    const DEFAULT_DEPTH: f32 = 0.0;
}

impl GridViewRender2D<'_> {
    /// Draw an ND-tree to scale on the target.
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim2D>) -> Result<()> {
        let (visible_quadtree, visible_rect) =
            match self.clip_ndtree_to_visible_render_cells(&params) {
                Some(x) => x,
                None => return Ok(()), // There is nothing to draw.
            };

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        // Steps #1: encode the quadtree as a texture.
        let gl_quadtree = cache.gl_quadtrees.gl_ndtree_from_node(
            (&visible_quadtree.root).into(),
            self.render_cell_layer,
            Self::ndtree_node_color,
        )?;
        // Step #2: draw at 1 pixel per render cell, including only the cells
        // inside `visible_rect`.
        let cells_w = visible_rect.len(X) as u32;
        let cells_h = visible_rect.len(Y) as u32;
        let (cells_texture, mut cells_fbo, cells_texture_viewport) =
            cache.textures.cells(cells_w, cells_h);
        cells_fbo
            .draw(
                vbos.ndtree_quad(),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::QUADTREE,
                &uniform! {
                    quadtree_texture: &gl_quadtree.texture,
                    layer_count: gl_quadtree.layers,
                    root_idx: gl_quadtree.root_idx,

                    offset_into_quadtree: [
                        visible_rect.min()[X] as i32,
                        visible_rect.min()[Y] as i32,
                    ],
                },
                &glium::DrawParameters {
                    viewport: Some(cells_texture_viewport),
                    ..Default::default()
                },
            )
            .context("cells_fbo.draw()")?;

        // Step #3: scale and render that onto the screen.
        let (target_w, target_h) = self.params.target.get_dimensions();
        let target_size = NdVec([r64(target_w as f64), r64(target_h as f64)]);
        let render_cells_size = target_size / self.render_cell_scale.units_per_cell();
        let render_cells_center =
            self.camera.render_cell_pos(&visible_quadtree.offset) - visible_rect.min().to_fvec();
        let render_cells_rect = FRect2D::centered(render_cells_center, render_cells_size / 2.0);
        let texture_coords_rect = render_cells_rect / visible_rect.size().to_fvec();

        self.params
            .target
            .draw(
                &*vbos.blit_quad_with_src_coords(texture_coords_rect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::PIXMIX,
                &uniform! {
                    alpha: params.alpha,
                    src_texture: cells_texture.sampled(),
                    scale_factor: self.render_cell_scale.units_per_cell().raw() as f32,
                    active_tex_size: (cells_w as f32, cells_h as f32)
                },
                &glium::DrawParameters {
                    blend: glium::Blend::alpha_blending(),
                    ..Default::default()
                },
            )
            .context("Drawing cells to target")?;

        // // Draw a 1:1 "minimap" in the corner
        // {
        //     let (width, height) = cells_fbo.get_dimensions();
        //     self.params.target.blit_from_simple_framebuffer(
        //         &cells_fbo,
        //         &glium::Rect {
        //             left: 0,
        //             bottom: 0,
        //             width,
        //             height,
        //         },
        //         &glium::BlitTarget {
        //             left: 0,
        //             bottom: 0,
        //             width: width as i32,
        //             height: height as i32,
        //         },
        //         glium::uniforms::MagnifySamplerFilter::Linear,
        //     );
        // }

        Ok(())
    }

    /// Draws gridlines at varying opacity and spacing depending on scaling.
    ///
    /// TOOD: support arbitrary exponential base and factor (a*b^n for any a, b)
    pub fn draw_gridlines(&mut self) -> Result<()> {
        let mut gridline_overlay_rects = vec![];

        let max_cell_spacing_exponent = self.gridline_cell_spacing_exponent(MAX_GRIDLINE_SPACING);
        let min_cell_spacing_exponent = self.gridline_cell_spacing_exponent(MIN_GRIDLINE_SPACING);
        let range = (min_cell_spacing_exponent..=max_cell_spacing_exponent).rev();

        for cell_spacing_exponent in range {
            gridline_overlay_rects.extend(self.generate_gridlines(cell_spacing_exponent, X, Y));
            gridline_overlay_rects.extend(self.generate_gridlines(cell_spacing_exponent, Y, X));
        }

        self.draw_cell_overlay_rects(&gridline_overlay_rects)
            .context("Drawing gridlines")
    }
    fn gridline_cell_spacing_exponent(&self, max_pixel_spacing: f64) -> u32 {
        // Compute the global cell spacing between gridlines.
        let log2_max_pixel_spacing = r64(max_pixel_spacing).log2();
        let log2_max_cell_spacing = log2_max_pixel_spacing - self.camera.scale().log2_factor();

        // Undo the `a * b^n` formula, rounding up to the nearest power of
        // `GRIDLINE_SPACING_BASE`.
        let log2_a = (GRIDLINE_SPACING_COEFF as f64).log2();
        let log2_b = (GRIDLINE_SPACING_BASE as f64).log2();
        ((log2_max_cell_spacing - log2_a) / log2_b)
            .ceil()
            .to_u32()
            .unwrap_or(0)
    }
    /// Generate gridlines overlay at one exponential power along one axis.
    fn generate_gridlines(
        &mut self,
        cell_spacing_exponent: u32,
        parallel_axis: Axis,
        perpendicular_axis: Axis,
    ) -> impl Iterator<Item = CellOverlayRect> {
        let alpha = gridline_alpha(cell_spacing_exponent, self.camera.scale());
        let mut color = crate::colors::GRIDLINES;
        color[3] *= alpha as f32;

        let cell_spacing = BigInt::from(GRIDLINE_SPACING_COEFF)
            * BigInt::from(GRIDLINE_SPACING_BASE).pow(cell_spacing_exponent);

        // Compute the coordinate of the first gridline by rounding to the
        // nearest multiple of `cell_spacing`.
        let cell_start_coord = self.global_visible_rect.min()[perpendicular_axis]
            .div_ceil(&cell_spacing)
            * &cell_spacing;
        let cell_end_coord = self.global_visible_rect.max()[perpendicular_axis].clone();

        let cell_coords = itertools::iterate(cell_start_coord, move |coord| coord + &cell_spacing)
            .take_while(move |coord| *coord <= cell_end_coord);

        let mut start = self.visible_rect.min();
        let mut end = self.visible_rect.max() + 1;

        let render_cell_len = self.render_cell_layer.big_len();
        let origin_coordinate = self.origin[perpendicular_axis].clone();

        let render_cell_coords = cell_coords.map(move |cell_coordinate| {
            (FixedPoint::from(cell_coordinate - &origin_coordinate) / &render_cell_len)
                .to_isize()
                .unwrap()
        });

        render_cell_coords.map(move |render_cell_coordinate| {
            // Generate an individual line.
            start[perpendicular_axis] = render_cell_coordinate;
            end[perpendicular_axis] = render_cell_coordinate;

            CellOverlayRect {
                start,
                end,
                z: GRIDLINE_DEPTH,
                start_color: color,
                end_color: color,
                line_params: Some(LineParams {
                    width: GRIDLINE_WIDTH,
                    include_endpoints: true,
                    axis: parallel_axis,
                }),
            }
        })
    }

    /// Draws a highlight on the render cell under the mouse cursor.
    pub fn draw_hover_highlight(&mut self, cell_pos: &BigVec2D, color: [f32; 4]) -> Result<()> {
        self.draw_cell_overlay_rects(&self.generate_cell_rect_outline(
            IRect2D::single_cell(self.clip_cell_pos_to_visible_render_cells(cell_pos)),
            CURSOR_DEPTH,
            HOVER_HIGHLIGHT_WIDTH,
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
        fill: bool,
    ) -> Result<()> {
        let visible_selection_rect = self.clip_cell_rect_to_visible_render_cells(&selection_rect);

        self.draw_cell_overlay_rects(&self.generate_cell_rect_outline(
            visible_selection_rect,
            SELECTION_DEPTH,
            SELECTION_HIGHLIGHT_WIDTH,
            crate::colors::SELECTION,
            RectHighlightParams {
                fill,
                crosshairs: false,
            },
        ))
        .context("Drawing selection highlight")?;

        // "Move selected cells" target.
        self.add_mouse_target_quad(
            ModifiersState::empty(),
            visible_selection_rect.to_frect(),
            MouseTargetData {
                binding: Some(MouseDragBinding::Select(
                    SelectDragCommand::MoveCells.into(),
                )),
                display: MouseDisplay::Move,
            },
        );
        // "Move selection" target.
        self.add_mouse_target_quad(
            ModifiersState::SHIFT,
            visible_selection_rect.to_frect(),
            MouseTargetData {
                binding: Some(MouseDragBinding::Select(
                    SelectDragCommand::MoveSelection.into(),
                )),
                display: MouseDisplay::Move,
            },
        );
        // "Move copy of cells" target.
        self.add_mouse_target_quad(
            ModifiersState::CTRL,
            visible_selection_rect.to_frect(),
            MouseTargetData {
                binding: Some(MouseDragBinding::Select(
                    SelectDragCommand::CopyCells.into(),
                )),
                display: MouseDisplay::Move,
            },
        );

        // "Resize selection" target.
        let click_target_width = self.params.config.ctrl.selection_resize_drag_target_width
            * self.render_cell_scale.inv_factor().to_f64().unwrap();
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
            self.add_mouse_target_quad(
                ModifiersState::empty(),
                FRect::span(NdVec([xs[xi], ys[yi]]), NdVec([xs[xi + 1], ys[yi + 1]])),
                MouseTargetData { binding, display },
            );
        }

        Ok(())
    }
    /// Draws a highlight indicating how the selection will be resized.
    pub fn draw_absolute_selection_resize_preview(
        &mut self,
        selection_rect: BigRect2D,
        mouse_pos: &ScreenPos2D,
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
            SELECTION_RESIZE_PREVIEW_WIDTH,
            crate::colors::SELECTION_RESIZE,
            RectHighlightParams {
                fill: true,
                crosshairs: false,
            },
        ))
        .context("Drawing selection resize highlight")?;
        Ok(())
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
        let mut verts = vec![];

        // Draw the rectangles in batches, because the VBO might not be able to
        // hold all the vertices at once.
        for rect_batch in rects.chunks(QUAD_BATCH_SIZE) {
            let count = rect_batch.len();
            // Generate vertices.
            verts.clear();
            for &rect in rect_batch {
                verts.extend_from_slice(&self.make_cell_overlay_verts(rect));
            }

            // Reborrow is necessary in order to split borrow.
            let cache = &mut *self.cache;
            let ibos = &mut cache.ibos;
            let vbos = &mut cache.vbos;

            // Put the data in a slice of the VBO.
            let vbo_slice = vbos.quad_verts_2d(count);
            vbo_slice.write(&verts);
            // Draw rectangles.
            self.params
                .target
                .draw(
                    vbo_slice,
                    &ibos.quad_indices(count),
                    &shaders::RGBA_2D,
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

    fn make_cell_overlay_verts(&self, rect: CellOverlayRect) -> [Vertex2D; 4] {
        let mut a = rect.start.to_fvec();
        let mut b = rect.end.to_fvec();
        let mut colors = [
            rect.start_color,
            rect.start_color,
            rect.end_color,
            rect.end_color,
        ];
        if let Some(LineParams {
            width,
            include_endpoints,
            axis,
        }) = rect.line_params
        {
            // At this point, the rectangle should have zero extra width.
            let min_width = self.render_cell_scale.cells_per_unit(); // 1 pixel
            let width = if self.render_cell_layer == Layer(0) {
                std::cmp::max(r64(width), min_width)
            } else {
                min_width
            };
            let offset = FVec::repeat(width / 2.0) * (b - a).signum();
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
            Vertex2D::from(([ax, ay, rect.z], colors[0])),
            Vertex2D::from(([bx, ay, rect.z], colors[1])),
            Vertex2D::from(([ax, by, rect.z], colors[2])),
            Vertex2D::from(([bx, by, rect.z], colors[3])),
        ]
    }
}

fn gridline_alpha(cell_spacing_exponent: u32, scale: Scale) -> f64 {
    // Logarithms turn multiplication into addition and exponentiation into
    // multiplication.
    let log2_cell_spacing = (GRIDLINE_SPACING_COEFF as f64).log2()
        + (GRIDLINE_SPACING_BASE as f64).log2() * cell_spacing_exponent as f64;
    let log2_pixel_spacing = log2_cell_spacing + scale.log2_factor().raw();

    // Fade maximum grid alpha as zooming out beyond 1:1.
    let max_alpha = clamped_interpolate(
        scale.log2_factor().raw(),
        0.0,
        1.0,
        ZOOMED_OUT_MAX_GRID_ALPHA,
        1.0,
    );
    let alpha = clamped_interpolate(
        log2_pixel_spacing,
        MIN_GRIDLINE_SPACING.log2(),
        MAX_GRIDLINE_SPACING.log2(),
        0.0,
        max_alpha,
    );

    alpha
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
