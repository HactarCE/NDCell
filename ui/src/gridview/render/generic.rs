use anyhow::{Context, Result};
use glium::glutin::event::ModifiersState;
use glium::index::PrimitiveType;
use glium::{uniform, Surface};
use palette::{Mix, Srgb, Srgba};
use std::cell::RefMut;

use ndcell_core::prelude::*;
use Axis::{X, Y};

use super::consts::*;
use super::shaders;
use super::vertices::MouseTargetVertex;
use super::CellDrawParams;
use crate::ext::*;
use crate::gridview::*;
use crate::Face;

pub struct GenericGridViewRender<'a, R: GridViewRenderDimension<'a>> {
    pub(super) cache: RefMut<'a, super::RenderCache>,
    pub(super) params: RenderParams<'a>,
    pub(super) dim: Option<R>,

    /// Viewpoint to render the grid from.
    pub(super) viewpoint: &'a R::Viewpoint,
    /// Transform from global space to local space (1 unit = 1 render cell; (0,
    /// 0) = `origin`), to NDC ((-1, -1) = bottom left; (1, 1) = top right), and
    ///    to pixel space (1 unit = 1 pixel; (0, 0) = top left).
    pub(super) xform: NdCellTransform<R::D>,
    /// Global rectangle of visible cells, rounded to render cell boundaries.
    pub(super) global_visible_rect: BigRect<R::D>,
    /// Local rectangle of visible render cells.
    pub(super) local_visible_rect: IRect<R::D>,

    /// Mouse targets, indexed by ID.
    mouse_targets: Vec<MouseTargetData>,
    /// Vertex data for mouse targets.
    mouse_target_tris: Vec<MouseTargetVertex>,

    /// Overlay rectangles to draw batched for performance and transparency.
    pub(super) overlay_quads: Vec<R::OverlayQuad>,
}
impl<'a, R: GridViewRenderDimension<'a>> GenericGridViewRender<'a, R> {
    /// Creates a `GridViewRender` for a gridview.
    pub fn new(params: RenderParams<'a>, viewpoint: &'a R::Viewpoint) -> Self {
        let mut cache = super::CACHE.borrow_mut();

        // Initialize color and depth buffers.
        let color = R::DEFAULT_COLOR;
        params.target.clear_color_srgb_and_depth(
            (color.red, color.green, color.blue, 1.0),
            R::DEFAULT_DEPTH,
        );

        // Initialize mouse picker.
        cache.picker.init(params.target.get_dimensions());

        // Compute the transformation from individual cells all the way to
        // pixels.
        let xform = viewpoint.cell_transform();

        // Determine the rectangle of visible cells in global coordinate space,
        // rounded to the nearest render cell.
        let global_visible_rect: BigRect<R::D> = viewpoint.global_visible_rect();
        let local_visible_rect: IRect<R::D> = xform
            .global_to_local_int_rect(&global_visible_rect)
            .expect("Unreasonable visible rectangle");

        let mut ret = Self {
            cache,
            params,
            dim: None,

            viewpoint,
            xform,
            global_visible_rect,
            local_visible_rect,

            mouse_targets: vec![],
            mouse_target_tris: vec![],

            overlay_quads: vec![],
        };
        ret.dim = Some(R::init(&ret));
        ret
    }

    /// Returns a `RenderResult` from this render.
    pub fn finish(mut self) -> Result<RenderResult> {
        R::draw_overlay_quads(&mut self).context("Drawing overlay")?;
        Ok(RenderResult {
            mouse_target: self.render_mouse_targets()?,
        })
    }

    fn render_mouse_targets(&mut self) -> Result<Option<MouseTargetData>> {
        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        // Draw the triangles in batches, because the VBO might not be able to
        // hold all the vertices at once.
        let (mut picker_fbo, picker_viewport) = cache.picker.fbo();
        for tri_batch in self.mouse_target_tris.chunks(MOUSE_TARGET_BATCH_SIZE * 3) {
            let count = tri_batch.len();
            // Put the data in a slice of the VBO.
            let vbo_slice = vbos.mouse_target_verts(count / 3);
            vbo_slice.write(&tri_batch);

            picker_fbo
                .draw(
                    vbo_slice,
                    &glium::index::NoIndices(PrimitiveType::TrianglesList),
                    &shaders::PICKER.load(),
                    &uniform! { matrix: self.xform.gl_matrix() },
                    &glium::DrawParameters {
                        depth: glium::Depth {
                            test: glium::DepthTest::IfLessOrEqual,
                            write: true,
                            ..Default::default()
                        },
                        viewport: Some(picker_viewport),
                        ..Default::default()
                    },
                )
                .context("Rendering selection mouse targets")?;
        }

        Ok(self
            .params
            .mouse
            .pos
            // Convert mouse position to `u32`.
            .and_then(|pos| pos[X].to_u32().zip(pos[Y].to_u32()))
            // Get mouse target ID underneath cursor.
            .map(|cursor_pos| self.cache.picker.get_pixel(cursor_pos) as usize)
            // Get mouse target using that ID (subtract 1 because 0 means no
            // target).
            .and_then(|i| self.mouse_targets.get(i.checked_sub(1)?))
            .cloned())
    }
    pub(super) fn add_mouse_target(&mut self, data: MouseTargetData) -> u32 {
        self.mouse_targets.push(data);
        self.mouse_targets.len() as u32 // IDs start at 1
    }
    pub(super) fn add_mouse_target_tri(
        &mut self,
        modifiers: Option<ModifiersState>,
        points: [FVec3D; 3],
        target_id: u32,
    ) {
        if let Some(mods) = modifiers {
            if self.params.modifiers != mods {
                return;
            }
        }

        for &point in &points {
            let pos = point.to_f32_array();
            self.mouse_target_tris
                .push(MouseTargetVertex { pos, target_id })
        }
    }

    /// Converts a global position to a local position, clamping it to the
    /// visible rectangle plus a few cells on each side for padding. This method
    /// always returns a local position that is either within the visible
    /// rectangle or just barely outside it.
    pub(super) fn clamp_int_pos_to_visible(&self, global_pos: &BigVec<R::D>) -> IVec<R::D> {
        // Padding = 2 render cells
        let padding = self.xform.render_cell_layer.big_len() * 2;

        let lower = self.global_visible_rect.min() - &padding;
        let global_pos = NdVec::max(global_pos, &lower);
        let upper = self.global_visible_rect.max() + &padding;
        let global_pos = NdVec::min(&global_pos, &upper);

        self.xform.global_to_local_int(&global_pos).unwrap()
    }
    /// Converts a global rectangle to local space using
    /// `clamp_int_pos_to_visible()`.
    pub(super) fn clip_int_rect_to_visible(&self, cells_rect: &BigRect<R::D>) -> IRect<R::D> {
        NdRect::span(
            self.clamp_int_pos_to_visible(&cells_rect.min()),
            self.clamp_int_pos_to_visible(&cells_rect.max()),
        )
    }

    /// Returns a slice of an ND-tree encompassing the visible area. It is safe
    /// to convert the offset of that slice to local coordinates.
    pub(super) fn clip_ndtree_to_visible<'b>(
        &self,
        params: &'b CellDrawParams<'b, R::D>,
    ) -> Option<NdTreeSlice<'b, R::D>> {
        // Clip the global rectangle of visible cells according to the draw
        // parameters.
        let global_visible_rect = match &params.rect {
            Some(rect) => match rect.intersection(&self.global_visible_rect) {
                // Only draw the intersection of the viewport and the rectangle
                // in the draw parameters.
                Some(intersection) => self.xform.render_cell_layer.round_rect(&intersection),
                // The rectangle in the draw parameters does not intersect the
                // viewport, so there is nothing to draw.
                None => return None,
            },
            // There is no rectangle in the parameters, so draw everything in
            // the viewport.
            None => self.global_visible_rect.clone(),
        };

        // Get the `NdTreeSlice` containing all of the visible cells.
        let visible_ndtree = params.ndtree.slice_containing(&global_visible_rect);

        Some(visible_ndtree)
    }

    pub(super) fn gridline_cell_spacing_exponent(&self, max_pixel_spacing: f64) -> u32 {
        // Compute the global cell spacing between gridlines.
        let log2_max_pixel_spacing = r64(max_pixel_spacing).log2();
        let log2_max_cell_spacing = log2_max_pixel_spacing - self.viewpoint.scale().log2_factor();

        // Undo the `a * b^n` formula, rounding up to the nearest power of
        // `GRIDLINE_SPACING_BASE`.
        let log2_a = (GRIDLINE_SPACING_COEFF as f64).log2();
        let log2_b = (GRIDLINE_SPACING_BASE as f64).log2();
        ((log2_max_cell_spacing - log2_a) / log2_b)
            .ceil()
            .to_u32()
            .unwrap_or(0)
    }

    /// Returns the endpoint pairs for a single crosshair.
    pub(super) fn make_crosshair_endpoints(
        &mut self,
        parallel_axis: Axis,
        a: R64,
        b: R64,
        position: FVec<R::D>,
        color: Srgb,
    ) -> Vec<[LineEndpoint<R::D>; 2]>
    where
        FVec<R::D>: Copy,
    {
        let bright_color = color;
        let dull_color = Srgb::from_linear(Mix::mix(
            &R::DEFAULT_COLOR.into_linear(),
            &color.into_linear(),
            crate::colors::CROSSHAIR_OPACITY,
        ));

        let gradient_len = std::cmp::max(
            r64(CROSSHAIR_GRADIENT_MIN_PIXEL_LEN) * self.xform.render_cell_scale.cells_per_unit(),
            r64(CROSSHAIR_GRADIENT_MIN_CELL_LEN),
        );

        let (pos1, pos2, pos3, pos4, pos5, pos6);
        {
            let visible_rect = self.local_visible_rect.to_frect();
            let pos_along_line = |coord| {
                let mut ret = position;
                ret[parallel_axis] = coord;
                ret
            };
            pos1 = pos_along_line(visible_rect.min()[parallel_axis]);
            pos2 = pos_along_line(a - gradient_len);
            pos3 = pos_along_line(a);
            pos4 = pos_along_line(b);
            pos5 = pos_along_line(b + gradient_len);
            pos6 = pos_along_line(visible_rect.max()[parallel_axis]);
        }

        vec![
            [
                LineEndpoint::include(pos1, dull_color),
                LineEndpoint::include(pos2, dull_color),
            ],
            [
                LineEndpoint::exclude(pos2, dull_color),
                LineEndpoint::exclude(pos3, bright_color),
            ],
            [
                LineEndpoint::include(pos3, bright_color),
                LineEndpoint::include(pos4, bright_color),
            ],
            [
                LineEndpoint::exclude(pos4, bright_color),
                LineEndpoint::exclude(pos5, dull_color),
            ],
            [
                LineEndpoint::include(pos5, dull_color),
                LineEndpoint::include(pos6, dull_color),
            ],
        ]
    }
    /// Returns an `FRect` for the line, swapping the endpoints if necessary so
    /// that `start[line_axis] < end[line_axis]`.
    pub(super) fn make_line_ndrect(
        &mut self,
        start: &mut LineEndpoint<R::D>,
        end: &mut LineEndpoint<R::D>,
        width: R64,
    ) -> (FRect<R::D>, Axis)
    where
        FVec<R::D>: Copy,
    {
        let min_width = self.xform.render_cell_scale.cells_per_unit() * R::LINE_MIN_PIXEL_WIDTH;
        let width = if self.xform.render_cell_layer == Layer(0) {
            std::cmp::max(width, min_width)
        } else {
            min_width
        };

        let rect = FRect::span(start.pos, end.pos);
        let axis = rect.size().max_axis();
        if start.pos[axis] > end.pos[axis] {
            std::mem::swap(start, end);
        }

        let mut min_offset = NdVec::repeat(-width / 2.0);
        let mut max_offset = NdVec::repeat(width / 2.0);
        if !start.include_endpoint {
            min_offset[axis] *= -1.0;
        }
        if !end.include_endpoint {
            max_offset[axis] *= -1.0;
        }
        (rect.offset_min_max(min_offset, max_offset), axis)
    }
}

pub trait GridViewRenderDimension<'a>: Sized {
    type D: Dim;
    type Viewpoint: Viewpoint<Self::D>;
    type OverlayQuad: Copy;

    const DEFAULT_COLOR: Srgb;
    const DEFAULT_DEPTH: f32;
    const LINE_MIN_PIXEL_WIDTH: f64;

    fn init(gvr: &GenericGridViewRender<'a, Self>) -> Self;

    fn draw_overlay_quads(this: &mut GenericGridViewRender<'a, Self>) -> Result<()>;
}

pub(super) type LineEndpoint2D = LineEndpoint<Dim2D>;
pub(super) type LineEndpoint3D = LineEndpoint<Dim3D>;

#[derive(Debug, Clone)]
pub(super) struct LineEndpoint<D: Dim> {
    pub pos: FVec<D>,
    pub color: Srgba,
    pub include_endpoint: bool,
}
impl<D: Dim> Copy for LineEndpoint<D> where FVec<D>: Copy {}
impl<D: Dim> LineEndpoint<D> {
    pub fn include(pos: FVec<D>, color: impl Into<Srgba>) -> Self {
        Self {
            pos,
            color: color.into(),
            include_endpoint: true,
        }
    }
    pub fn exclude(pos: FVec<D>, color: impl Into<Srgba>) -> Self {
        Self {
            pos,
            color: color.into(),
            include_endpoint: false,
        }
    }
}

/// Fill style for an overlay quad.
#[derive(Debug, Copy, Clone)]
pub(super) enum OverlayFill {
    Solid(Srgba),
    Gradient(Axis, Srgba, Srgba),
    Gridlines3D,
}
impl From<Srgba> for OverlayFill {
    fn from(color: Srgba) -> Self {
        Self::Solid(color)
    }
}
impl OverlayFill {
    pub fn gradient(axis: Axis, color1: Srgba, color2: Srgba) -> Self {
        if color1 == color2 {
            Self::Solid(color1)
        } else {
            Self::Gradient(axis, color1, color2)
        }
    }
    pub fn vertex_colors(self, face: Face) -> [Srgba; 4] {
        match self {
            OverlayFill::Gridlines3D => [Srgba::new(0.0, 0.0, 0.0, 0.0); 4], // ignored in vertex shader
            OverlayFill::Solid(color) => [color; 4],
            OverlayFill::Gradient(gradient_axis, c1, c2) => {
                let [ax1, ax2] = face.plane_axes();
                if gradient_axis == ax1 {
                    [c1, c2, c1, c2]
                } else if gradient_axis == ax2 {
                    [c1, c1, c2, c2]
                } else {
                    match face.sign() {
                        Sign::Minus => [c1; 4],
                        Sign::NoSign => unreachable!(),
                        Sign::Plus => [c2; 4],
                    }
                }
            }
        }
    }
    /// Returns `true` if the quad is definitely 100% opaque.
    pub fn is_opaque(self) -> bool {
        match self {
            Self::Solid(color) => color.alpha >= 1.0,
            Self::Gradient(_, c1, c2) => c1.alpha >= 1.0 && c2.alpha >= 1.0,
            Self::Gridlines3D => false,
        }
    }
}
