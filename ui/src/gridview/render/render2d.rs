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

use anyhow::{Context, Result};
use glium::glutin::event::ModifiersState;
use glium::index::PrimitiveType;
use glium::{uniform, Surface};
use itertools::Itertools;
use palette::{Srgb, Srgba};

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension, LineEndpoint2D, OverlayFill};
use super::shaders;
use super::vertices::Vertex2D;
use super::CellDrawParams;
use crate::commands::{DragCmd, DrawMode};
use crate::ext::*;
use crate::gridview::*;
use crate::{Direction, Face, Scale, CONFIG, DIRECTIONS};

pub(in crate::gridview) type GridViewRender2D<'a> = GenericGridViewRender<'a, RenderDim2D>;

pub(in crate::gridview) struct RenderDim2D;
impl<'a> GridViewRenderDimension<'a> for RenderDim2D {
    type D = Dim2D;
    type Viewpoint = Viewpoint2D;
    type OverlayQuad = OverlayQuad;

    const DEFAULT_COLOR: Srgb = crate::colors::BACKGROUND_2D;
    const DEFAULT_DEPTH: f32 = 0.0;
    const LINE_MIN_PIXEL_WIDTH: f64 = LINE_MIN_PIXEL_WIDTH_2D;

    fn init(_: &GridViewRender2D<'a>) -> Self {
        Self
    }

    fn draw_overlay_quads(this: &mut GridViewRender2D<'a>) -> Result<()> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *this.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        let gl_matrix = this.xform.gl_matrix_with_subpixel_offset();

        let mut verts = vec![];
        for chunk in this.overlay_quads.chunks(QUAD_BATCH_SIZE) {
            // Generate vertices.
            verts.clear();
            for quad in chunk {
                verts.extend_from_slice(&quad.verts());
            }

            // Populate VBO and IBO.
            let quad_count = chunk.len();
            let vbo = vbos.quad_verts_2d(quad_count);
            vbo.write(&verts);
            let ibo = ibos.quad_indices(quad_count);

            // Draw quads.
            this.params
                .target
                .draw(
                    vbo,
                    &ibo,
                    &shaders::RGBA_2D.load(),
                    &uniform! { matrix: gl_matrix },
                    &glium::DrawParameters {
                        blend: glium::Blend::alpha_blending(),
                        depth: glium::Depth {
                            test: glium::DepthTest::Overwrite,
                            write: true,
                            ..Default::default()
                        },
                        multisampling: false,
                        ..Default::default()
                    },
                )
                .context("Drawing 2D overlay quads")?;
        }
        Ok(())
    }
}

impl GridViewRender2D<'_> {
    /// Draw an ND-tree to scale on the target.
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim2D>) -> Result<()> {
        let visible_quadtree = match self.clip_ndtree_to_visible(&params) {
            Some(x) => x,
            None => return Ok(()), // There is nothing to draw.
        };

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        // Steps #1: encode the quadtree as a texture.
        let gl_quadtree = cache.gl_quadtrees.gl_ndtree_from_node(
            (&visible_quadtree.root).into(),
            self.xform.render_cell_layer,
        )?;
        // Step #2: draw at 1 pixel per render cell, including only the render
        // cells inside `self.local_visible_rect`.
        let cells_w = self.local_visible_rect.len(X) as u32;
        let cells_h = self.local_visible_rect.len(Y) as u32;
        let (cells_texture, mut cells_fbo, cells_texture_viewport) =
            cache.textures.cells(cells_w, cells_h);
        let local_quadtree_base = self
            .xform
            .global_to_local_int(&visible_quadtree.base_pos)
            .unwrap();
        let relative_quadtree_base = local_quadtree_base - self.local_visible_rect.min();
        cells_fbo
            .draw(
                vbos.ndtree_quad(),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::QUADTREE.load(),
                &uniform! {
                    quadtree_texture: &gl_quadtree.texture,
                    layer_count: gl_quadtree.layers,
                    root_idx: gl_quadtree.root_idx,

                    quadtree_base: relative_quadtree_base.to_i32_array(),
                },
                &glium::DrawParameters {
                    viewport: Some(cells_texture_viewport),
                    ..Default::default()
                },
            )
            .context("cells_fbo.draw()")?;

        // Step #3: scale and render that onto the screen.
        let local_screen_rect = self.xform.local_screen_rect();
        let vis_rect = self.local_visible_rect.to_frect();
        let texture_coords_rect = (local_screen_rect - vis_rect.min()) / vis_rect.size();

        self.params
            .target
            .draw(
                &*vbos.blit_quad_with_src_coords(texture_coords_rect),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::PIXMIX.load(),
                &uniform! {
                    alpha: params.alpha,
                    src_texture: cells_texture.sampled(),
                    scale_factor: self.xform.render_cell_scale.units_per_cell().raw() as f32,
                    active_tex_size: (cells_w as f32, cells_h as f32)
                },
                &glium::DrawParameters {
                    blend: glium::Blend::alpha_blending(),
                    multisampling: false,
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

    /// Adds gridlines to the overlay.
    pub fn add_gridlines_overlay(&mut self) {
        let min_gridline_exponent =
            self.gridline_cell_spacing_exponent(GRIDLINE_ALPHA_GRADIENT_LOW_PIXEL_SPACING);
        let max_gridline_exponent =
            self.gridline_cell_spacing_exponent(GRIDLINE_ALPHA_GRADIENT_HIGH_PIXEL_SPACING);
        let range = (min_gridline_exponent..=max_gridline_exponent).rev();

        for gridline_exponent in range {
            self.add_gridline_set_overlay(gridline_exponent, X);
            self.add_gridline_set_overlay(gridline_exponent, Y);
        }
    }

    /// Adds a set of gridlines at one exponential power along one axis.
    fn add_gridline_set_overlay(&mut self, gridline_exponent: u32, perpendicular_axis: Axis) {
        let alpha = gridline_alpha(gridline_exponent, self.viewpoint.scale()) as f32;

        let global_spacing: BigInt = BigInt::from(GRIDLINE_SPACING_COEFF)
            * BigInt::from(GRIDLINE_SPACING_BASE).pow(gridline_exponent);
        let global_gridline_origin = self.xform.origin.div_floor(&global_spacing) * &global_spacing;
        let local_gridline_origin = self
            .xform
            .global_to_local_float(&global_gridline_origin.to_fixedvec())
            .unwrap();
        let local_offset: R64 = local_gridline_origin[perpendicular_axis];
        let global_spacing: FixedPoint = global_spacing.into();
        let local_spacing: FixedPoint = global_spacing >> self.xform.render_cell_layer.to_u32();
        let local_spacing: R64 = r64(local_spacing.to_f64().unwrap());

        // Compute the coordinate of the first gridline by rounding to the
        // nearest multiple of `local_spacing` (offset by `local_offset`).
        let cell_start_coord: R64 = r64(self.local_visible_rect.min()[perpendicular_axis] as f64);
        let cell_start_coord = ((cell_start_coord - local_offset) / local_spacing).ceil()
            * local_spacing
            + local_offset;

        let cell_end_coord = r64(self.local_visible_rect.max()[perpendicular_axis] as f64);

        let cell_coords = itertools::iterate(cell_start_coord, |&coord| coord + local_spacing)
            .take_while(|&coord| coord <= cell_end_coord);

        let visible_rect = self.local_visible_rect.to_frect();

        for coordinate in cell_coords {
            // Generate an individual line.
            let mut color = crate::colors::GRIDLINES;
            color.alpha *= alpha; // (Alpha channel is linear according to OpenGL.)

            let mut a = visible_rect.min();
            let mut b = visible_rect.max();
            a[perpendicular_axis] = coordinate;
            b[perpendicular_axis] = coordinate;

            self.add_line_overlay(
                LineEndpoint2D::include(a, color),
                LineEndpoint2D::include(b, color),
                r64(GRIDLINE_WIDTH),
            );
        }
    }

    /// Adds a highlight on the render cell under the mouse cursor when using
    /// the drawing tool.
    pub fn add_hover_draw_overlay(&mut self, cell_pos: &BigVec2D, draw_mode: DrawMode) {
        self.add_hover_overlay(cell_pos, draw_mode.fill_color(), draw_mode.outline_color());
    }
    /// Adds a highlight on the render cell under the mouse cursor when using
    /// the selection tool.
    pub fn add_hover_select_overlay(&mut self, cell_pos: &BigVec2D) {
        use crate::colors::hover::*;
        self.add_hover_overlay(cell_pos, SELECT_FILL, SELECT_OUTLINE);
    }
    /// Adds a highlight on the render cell under the mouse cursor.
    fn add_hover_overlay(&mut self, cell_pos: &BigVec2D, fill_color: Srgba, outline_color: Srgb) {
        let local_rect = IRect::single_cell(self.clamp_int_pos_to_visible(cell_pos));
        let width = r64(HOVER_HIGHLIGHT_WIDTH);
        self.add_rect_fill_overlay(local_rect, fill_color);
        self.add_rect_crosshairs_overlay(local_rect, outline_color, width);
    }

    /// Adds a highlight around the selection when the selection includes cells.
    pub fn add_selection_cells_highlight_overlay(&mut self, selection_rect: &BigRect2D) {
        use crate::colors::selection::*;
        self.add_selection_highlight_overlay(selection_rect, CELLS_FILL, CELLS_OUTLINE)
    }
    /// Adds a highlight around the selection when the selection does not
    /// include cells.
    pub fn add_selection_region_highlight_overlay(&mut self, selection_rect: &BigRect2D) {
        use crate::colors::selection::*;
        self.add_selection_highlight_overlay(selection_rect, REGION_FILL, REGION_OUTLINE)
    }
    /// Adds a highlight around the selection.
    fn add_selection_highlight_overlay(
        &mut self,
        selection_rect: &BigRect2D,
        fill_color: Srgba,
        outline_color: Srgb,
    ) {
        let local_rect = self.clip_int_rect_to_visible(selection_rect);
        let width = r64(SELECTION_HIGHLIGHT_WIDTH);
        self.add_rect_fill_overlay(local_rect, fill_color);
        self.add_rect_outline_overlay(local_rect, outline_color, width);

        let local_rect = local_rect.to_frect();

        // "Move selected cells" target.
        self.add_mouse_target_quad(
            local_rect,
            Some(ModifiersState::empty()),
            MouseTargetData {
                binding: DragCmd::MoveSelectedCells(None),
            },
        );

        // "Move selection" target.
        self.add_mouse_target_quad(
            local_rect,
            Some(ModifiersState::SHIFT),
            MouseTargetData {
                binding: DragCmd::MoveSelection(None),
            },
        );

        // "Move copy of cells" target.
        self.add_mouse_target_quad(
            local_rect,
            Some(ModifiersState::CTRL),
            MouseTargetData {
                binding: DragCmd::CopySelectedCells(None),
            },
        );

        // "Resize selection" target.
        let click_target_width = r64(CONFIG.lock().ctrl.selection_resize_drag_target_width)
            * self.xform.render_cell_scale.inv_factor().to_f64().unwrap();
        let (min, max) = (local_rect.min(), local_rect.max());
        let xs = vec![
            min[X] - click_target_width * 0.75,
            min[X] + click_target_width * 0.25,
            max[X] - click_target_width * 0.25,
            max[X] + click_target_width * 0.75,
        ];
        let ys = vec![
            min[Y] - click_target_width * 0.75,
            min[Y] + click_target_width * 0.25,
            max[Y] - click_target_width * 0.25,
            max[Y] + click_target_width * 0.75,
        ];
        for &direction in &DIRECTIONS {
            let binding = DragCmd::ResizeSelection2D(direction);
            let NdVec([dx, dy]) = direction.vector();
            self.add_mouse_target_quad(
                FRect::span(
                    NdVec([xs[(dx + 1) as usize], ys[(dy + 1) as usize]]),
                    NdVec([xs[(dx + 2) as usize], ys[(dy + 2) as usize]]),
                ),
                Some(ModifiersState::empty()),
                MouseTargetData { binding },
            );
        }
    }
    /// Adds a highlight indicating how the selection will be resized.
    pub fn add_selection_resize_preview_overlay(&mut self, selection_preview_rect: &BigRect2D) {
        let local_rect = self.clip_int_rect_to_visible(&selection_preview_rect);
        let width = r64(SELECTION_HIGHLIGHT_WIDTH);
        use crate::colors::selection::*;
        self.add_rect_fill_overlay(local_rect, RESIZE_FILL);
        self.add_rect_outline_overlay(local_rect, RESIZE_OUTLINE, width);
    }
    /// Adds a highlight indicating which edge(s) of the selection will be resized.
    pub fn add_selection_edge_resize_overlay(
        &mut self,
        selection_rect: &BigRect2D,
        direction: Direction,
    ) {
        let color = crate::colors::selection::RESIZE_OUTLINE;
        let rect = self._adjust_rect_for_overlay(self.clip_int_rect_to_visible(selection_rect));
        for &ax in Dim2D::axes() {
            let mut min = rect.min();
            let mut max = rect.max();
            match direction.vector()[ax] {
                -1 => max[ax] = min[ax],
                1 => min[ax] = max[ax],
                _ => continue,
            };
            self.add_line_overlay(
                LineEndpoint2D::include(min, color),
                LineEndpoint2D::include(max, color),
                r64(SELECTION_HIGHLIGHT_WIDTH),
            );
        }
    }

    /// Adds a filled-in rectangle with a solid color.
    fn add_rect_fill_overlay(&mut self, rect: IRect2D, color: Srgba) {
        self.overlay_quads.push(OverlayQuad {
            rect: self._adjust_rect_for_overlay(rect),
            fill: OverlayFill::Solid(color),
        });
    }
    /// Adds a rectangular outline with a solid color.
    fn add_rect_outline_overlay(&mut self, rect: IRect2D, color: Srgb, width: R64) {
        let rect = self._adjust_rect_for_overlay(rect);
        let NdVec([ax, ay]) = rect.min();
        let NdVec([bx, by]) = rect.max();

        let corners = [
            NdVec([ax, ay]),
            NdVec([bx, ay]),
            NdVec([bx, by]),
            NdVec([ax, by]),
        ];

        for (&corner1, &corner2) in corners.iter().circular_tuple_windows() {
            self.add_line_overlay(
                LineEndpoint2D::include(corner1, color),
                LineEndpoint2D::include(corner2, color),
                width,
            );
        }
    }
    /// Adds crosshairs around a rectangle with a solid color that fades into
    /// the gridline color toward the edges.
    fn add_rect_crosshairs_overlay(&mut self, rect: IRect2D, color: Srgb, width: R64) {
        let rect = self._adjust_rect_for_overlay(rect);
        let NdVec([ax, ay]) = rect.min();
        let NdVec([bx, by]) = rect.max();

        self.add_single_crosshair_overlay(X, ax, bx, ay, width, color); // bottom
        self.add_single_crosshair_overlay(X, ax, bx, by, width, color); // top
        self.add_single_crosshair_overlay(Y, ay, by, ax, width, color); // left
        self.add_single_crosshair_overlay(Y, ay, by, bx, width, color); // right
    }
    fn add_single_crosshair_overlay(
        &mut self,
        parallel_axis: Axis,
        a: R64,
        b: R64,
        perpendicular_coord: R64,
        width: R64,
        color: Srgb,
    ) {
        let mut pos = NdVec::repeat(perpendicular_coord);
        pos[parallel_axis] = a;
        let endpoint_pairs = self.make_crosshair_endpoints(parallel_axis, a, b, pos, color);
        for [start, end] in endpoint_pairs {
            self.add_line_overlay(start, end, width);
        }
    }
    fn add_line_overlay(&mut self, mut start: LineEndpoint2D, mut end: LineEndpoint2D, width: R64) {
        let (rect, axis) = self.make_line_ndrect(&mut start, &mut end, width);
        let fill = OverlayFill::gradient(axis, start.color, end.color);
        self.overlay_quads.push(OverlayQuad { rect, fill })
    }

    fn add_mouse_target_quad(
        &mut self,
        rect: FRect2D,
        modifiers: Option<ModifiersState>,
        data: MouseTargetData,
    ) {
        let NdVec([x1, y1]) = rect.min();
        let NdVec([x2, y2]) = rect.max();
        let z = r64(0.0);
        let corners = [
            NdVec([x1, y1, z]),
            NdVec([x2, y1, z]),
            NdVec([x1, y2, z]),
            NdVec([x2, y2, z]),
        ];
        let target_id = self.add_mouse_target(data);
        self.add_mouse_target_tri(modifiers, [corners[0], corners[1], corners[2]], target_id);
        self.add_mouse_target_tri(modifiers, [corners[3], corners[2], corners[1]], target_id);
    }

    fn _adjust_rect_for_overlay(&self, rect: IRect2D) -> FRect2D {
        // If there are more than 1.5 pixels per render cell, the upper outline
        // should be *between* cells (+0.0). If there are fewer, the upper
        // outline should be *on* the cell below (-1.0).
        let pix_per_cell = self.xform.render_cell_scale.units_per_cell();
        let upper_offset = if pix_per_cell > 1.5 { 0.0 } else { -1.0 };
        rect.to_frect().offset_min_max(r64(0.0), r64(upper_offset))
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
        GRIDLINE_ALPHA_ZOOMED_OUT_MULT_2D,
        1.0,
    );
    let alpha = clamped_interpolate(
        log2_pixel_spacing,
        GRIDLINE_ALPHA_GRADIENT_LOW_PIXEL_SPACING.log2(),
        GRIDLINE_ALPHA_GRADIENT_HIGH_PIXEL_SPACING.log2(),
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
pub struct OverlayQuad {
    /// 2D region.
    rect: FRect2D,
    /// Color fill.
    fill: OverlayFill,
}
impl OverlayQuad {
    fn verts(self) -> [Vertex2D; 4] {
        let [x1, y1] = self.rect.min().to_f32_array();
        let [x2, y2] = self.rect.max().to_f32_array();
        let colors = self.fill.vertex_colors(Face::PosZ);
        [
            Vertex2D::new([x1, y1], colors[0]),
            Vertex2D::new([x2, y1], colors[1]),
            Vertex2D::new([x1, y2], colors[2]),
            Vertex2D::new([x2, y2], colors[3]),
        ]
    }
}
