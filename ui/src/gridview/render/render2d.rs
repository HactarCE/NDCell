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
use palette::{Mix, Srgb, Srgba};

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::Vertex2D;
use super::CellDrawParams;
use crate::config::MouseDragBinding;
use crate::ext::*;
use crate::gridview::*;
use crate::mouse::MouseDisplay;
use crate::{Scale, CONFIG};

pub(in crate::gridview) type GridViewRender2D<'a> = GenericGridViewRender<'a, RenderDim2D>;

pub(in crate::gridview) struct RenderDim2D;
impl<'a> GridViewRenderDimension<'a> for RenderDim2D {
    type D = Dim2D;
    type Viewpoint = Viewpoint2D;
    type OverlayQuad = OverlayQuad;

    const DEFAULT_COLOR: Srgb = crate::colors::BACKGROUND_2D;
    const DEFAULT_DEPTH: f32 = 0.0;

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
            Self::ndtree_node_color,
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
        let quadtree_offset = self.local_visible_rect.min() - local_quadtree_base;
        cells_fbo
            .draw(
                vbos.ndtree_quad(),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::QUADTREE.load(),
                &uniform! {
                    quadtree_texture: &gl_quadtree.texture,
                    layer_count: gl_quadtree.layers,
                    root_idx: gl_quadtree.root_idx,

                    quadtree_offset: quadtree_offset.to_i32_array(),
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
                LineEndpoint::include(a, color),
                LineEndpoint::include(b, color),
                r64(GRIDLINE_WIDTH),
            );
        }
    }

    /// Adds a highlight on the render cell under the mouse cursor when using
    /// the drawing tool.
    pub fn add_hover_draw_overlay(&mut self, cell_pos: &BigVec2D) {
        use crate::colors::hover::*;
        self.add_hover_overlay(cell_pos, DRAW_FILL, DRAW_OUTLINE);
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
            ModifiersState::empty(),
            local_rect,
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
            local_rect,
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
            local_rect,
            MouseTargetData {
                binding: Some(MouseDragBinding::Select(
                    SelectDragCommand::CopyCells.into(),
                )),
                display: MouseDisplay::Move,
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
    }
    /// Adds a highlight indicating how the selection will be resized.
    pub fn add_selection_resize_preview_overlay(
        &mut self,
        selection_rect: &BigRect2D,
        mouse_pos: &ScreenPos2D,
    ) {
        let selection_preview_rect = selection::resize_selection_absolute(
            selection_rect,
            &mouse_pos.pos(),
            &mouse_pos.rect(),
        );
        let local_rect = self.clip_int_rect_to_visible(&selection_preview_rect);
        let width = r64(SELECTION_RESIZE_PREVIEW_WIDTH);
        self.add_rect_fill_overlay(local_rect, crate::colors::selection::RESIZE_FILL);
        self.add_rect_outline_overlay(local_rect, crate::colors::selection::RESIZE_OUTLINE, width);
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
                LineEndpoint::include(corner1, color),
                LineEndpoint::include(corner2, color),
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
        let bright_color = color;
        let dull_color = Srgb::from_linear(Mix::mix(
            &color.into_linear(),
            &crate::colors::BACKGROUND_2D.into_linear(),
            crate::colors::CROSSHAIR_OPACITY_2D,
        ));

        let (pos1, pos2, pos3, pos4, pos5, pos6);
        {
            let visible_rect = self.local_visible_rect.to_frect();
            let pos_along_line = |coord| {
                let mut ret = FVec2D::repeat(perpendicular_coord);
                ret[parallel_axis] = coord;
                ret
            };
            pos1 = pos_along_line(visible_rect.min()[parallel_axis]);
            pos2 = pos_along_line(a - r64(1.0));
            pos3 = pos_along_line(a);
            pos4 = pos_along_line(b);
            pos5 = pos_along_line(b + r64(1.0));
            pos6 = pos_along_line(visible_rect.max()[parallel_axis]);
        }

        self.add_line_overlay(
            LineEndpoint::include(pos1, dull_color),
            LineEndpoint::include(pos2, dull_color),
            width,
        );
        self.add_line_overlay(
            LineEndpoint::exclude(pos2, dull_color),
            LineEndpoint::exclude(pos3, bright_color),
            width,
        );
        self.add_line_overlay(
            LineEndpoint::include(pos3, bright_color),
            LineEndpoint::include(pos4, bright_color),
            width,
        );
        self.add_line_overlay(
            LineEndpoint::exclude(pos4, bright_color),
            LineEndpoint::exclude(pos5, dull_color),
            width,
        );
        self.add_line_overlay(
            LineEndpoint::include(pos5, dull_color),
            LineEndpoint::include(pos6, dull_color),
            width,
        );
    }
    fn add_line_overlay(&mut self, mut start: LineEndpoint, mut end: LineEndpoint, width: R64) {
        let min_width = self.xform.render_cell_scale.cells_per_unit(); // 1 pixel
        let width = if self.xform.render_cell_layer == Layer(0) {
            std::cmp::max(width, min_width)
        } else {
            min_width
        };

        let mut rect = FRect::span(start.pos, end.pos);
        let axis = rect.size().max_axis();
        if start.pos[axis] > end.pos[axis] {
            std::mem::swap(&mut start, &mut end);
        }

        {
            let mut min_offset = NdVec::repeat(-width / 2.0);
            let mut max_offset = NdVec::repeat(width / 2.0);
            if !start.include_endpoint {
                min_offset[axis] *= -1.0;
            }
            if !end.include_endpoint {
                max_offset[axis] *= -1.0;
            }
            rect = rect.offset_min_max(min_offset, max_offset);
        }

        let fill = if start.color == end.color {
            OverlayFill::Solid(start.color)
        } else {
            OverlayFill::Gradient(axis, start.color, end.color)
        };

        self.overlay_quads.push(OverlayQuad { rect, fill })
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

#[derive(Debug, Copy, Clone)]
struct LineEndpoint {
    pub pos: FVec2D,
    pub color: Srgba,
    pub include_endpoint: bool,
}
impl LineEndpoint {
    pub fn include(pos: FVec2D, color: impl Into<Srgba>) -> Self {
        Self {
            pos,
            color: color.into(),
            include_endpoint: true,
        }
    }
    pub fn exclude(pos: FVec2D, color: impl Into<Srgba>) -> Self {
        Self {
            pos,
            color: color.into(),
            include_endpoint: false,
        }
    }
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
        let colors = self.fill.vertex_colors();
        [
            Vertex2D::new([x1, y1], colors[0]),
            Vertex2D::new([x2, y1], colors[1]),
            Vertex2D::new([x1, y2], colors[2]),
            Vertex2D::new([x2, y2], colors[3]),
        ]
    }
}

#[derive(Debug, Copy, Clone)]
enum OverlayFill {
    Solid(Srgba),
    Gradient(Axis, Srgba, Srgba),
}
impl OverlayFill {
    fn vertex_colors(self) -> [Srgba; 4] {
        match self {
            OverlayFill::Solid(color) => [color; 4],
            OverlayFill::Gradient(gradient_axis, c1, c2) => match gradient_axis {
                X => [c1, c2, c1, c2],
                Y => [c1, c1, c2, c2],
                other => panic!("Gradient axis {:?} is not in XY plane", other),
            },
        }
    }
}
