//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use anyhow::{Context, Result};
use glium::glutin::event::ModifiersState;
use glium::index::PrimitiveType;
use glium::uniforms::UniformBuffer;
use glium::Surface;
use itertools::Itertools;
use palette::{Mix, Pixel, Srgb, Srgba};
use sloth::Lazy;

use ndcell_core::prelude::*;

mod fog;
mod gridlines;
mod lighting;
mod octree;

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension, LineEndpoint3D, OverlayFill};
use super::shaders;
use super::vertices::Vertex3D;
use super::CellDrawParams;
use crate::commands::{DragCmd, DrawMode};
use crate::ext::*;
use crate::gridview::*;
use crate::{Face, DISPLAY, FACES};
use fog::FogParams;
use gridlines::GridlineParams;
use lighting::LightingParams;
use octree::OctreeParams;

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

type QuadVerts = [Vertex3D; 4];
type CuboidVerts = [Option<QuadVerts>; 6];

type LazyUbo<T> = Lazy<UniformBuffer<T>, Box<dyn FnOnce() -> UniformBuffer<T>>>;

pub(in crate::gridview) struct RenderDim3D {
    fog_uniform: LazyUbo<FogParams>,
    gridline_uniform: LazyUbo<GridlineParams>,
    lighting_uniform: LazyUbo<LightingParams>,
}
impl<'a> GridViewRenderDimension<'a> for RenderDim3D {
    type D = Dim3D;
    type Viewpoint = Viewpoint3D;
    type OverlayQuad = OverlayQuad;

    const DEFAULT_COLOR: Srgb = crate::colors::BACKGROUND_3D;
    const DEFAULT_DEPTH: f32 = f32::INFINITY;
    const LINE_MIN_PIXEL_WIDTH: f64 = LINE_MIN_PIXEL_WIDTH_3D;

    fn init(gvr3d: &GridViewRender3D<'a>) -> Self {
        let fog_params = FogParams::from(gvr3d);
        let gridline_params = GridlineParams::from(gvr3d);
        let lighting_params = LightingParams::default();

        fn lazy_ubo<U: 'static + Copy>(u: U) -> LazyUbo<U> {
            Lazy::new(Box::new(move || {
                UniformBuffer::new(&**DISPLAY, u).expect("Failed to create uniform buffer")
            }))
        }

        Self {
            fog_uniform: lazy_ubo(fog_params),
            gridline_uniform: lazy_ubo(gridline_params),
            lighting_uniform: lazy_ubo(lighting_params),
        }
    }

    fn draw_overlay_quads(this: &mut GridViewRender3D<'a>) -> Result<()> {
        this.cull_backface_quads();
        this.depth_sort_quads();

        let gl_matrix = this.xform.gl_matrix();

        let fog_params = &**this.dim.as_ref().unwrap().fog_uniform;
        let lighting_params = &**this.dim.as_ref().unwrap().lighting_uniform;
        let gridline_params = &**this.dim.as_ref().unwrap().gridline_uniform;

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *this.cache;
        let vbos = &mut cache.vbos;
        let ibos = &mut cache.ibos;

        // `group_by()` is defined using an extension trait in this crate; it is
        // semantically identical to `Itertools::group_by()`, but operates on a
        // slice instead of an iterator. `chunks()` is part of the standard
        // library.

        let mut verts = vec![];
        // Group by what shader to use and whether the quad is opaque ...
        for ((shader_program, is_opaque), quads_group) in this
            .overlay_quads
            .group_by(|quad| (quad.shader_program(), quad.is_opaque()))
        {
            // ... and then process these in batches, because the VBO has
            // limited capacity.
            for chunk in quads_group.chunks(QUAD_BATCH_SIZE) {
                // Generate vertices.
                verts.clear();
                for quad in chunk {
                    verts.extend_from_slice(&quad.verts());
                }

                // Populate VBO and IBO.
                let quad_count = chunk.len();
                let vbo = vbos.quad_verts_3d(quad_count);
                vbo.write(&verts);
                let ibo = ibos.quad_indices(quad_count);

                // Draw quads.
                this.params
                    .target
                    .draw(
                        vbo,
                        &ibo,
                        &shader_program.load(),
                        &uniform! {
                            matrix: gl_matrix,

                            FogParams: fog_params,
                            LightingParams: lighting_params,
                            GridlineParams: gridline_params,
                        },
                        &glium::DrawParameters {
                            blend: glium::Blend::alpha_blending(),
                            depth: glium::Depth {
                                test: glium::DepthTest::IfLessOrEqual,
                                write: is_opaque,
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                    )
                    .context("Drawing 3D overlay quads")?;
            }
        }
        Ok(())
    }
}

impl GridViewRender3D<'_> {
    /// Draw an ND-tree to scale on the target.
    pub fn draw_cells(&mut self, params: CellDrawParams<'_, Dim3D>) -> Result<()> {
        let visible_octree = match self.clip_ndtree_to_visible(&params) {
            Some(x) => x,
            None => return Ok(()), // There is nothing to draw.
        };

        let octree_base = self
            .xform
            .global_to_local_int(&visible_octree.base_pos)
            .unwrap();

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;
        let textures = &mut cache.textures;

        let gl_octree = cache
            .gl_octrees
            .gl_ndtree_from_node((&visible_octree.root).into(), self.xform.render_cell_layer)?;

        let octree_params = UniformBuffer::new(
            &**DISPLAY,
            OctreeParams {
                matrix: self.xform.gl_matrix(),
                inv_matrix: self.xform.inv_gl_matrix(),

                octree_base: octree_base.to_i32_array(),
                layer_count: gl_octree.layers,
                root_idx: gl_octree.root_idx,
            },
        )
        .expect("Failed to create uniform buffer");

        let vbo = &*vbos.ndtree_quad();
        let indices = glium::index::NoIndices(PrimitiveType::TriangleStrip);

        let (w, h) = self.params.target.get_dimensions();
        const BLOCK_SIZE: u32 = 8; // Some experimentation suggests 8 is optimal.
        let first_pass_w = w.div_ceil(&BLOCK_SIZE) + 1;
        let first_pass_h = h.div_ceil(&BLOCK_SIZE) + 1;
        let (init_t_texture, mut init_t_fbo, init_t_texture_viewport) =
            textures.octree_init_t(first_pass_w, first_pass_h);

        // First pass: render a depth map at 1/8 scale (1/64 number of pixels).
        init_t_fbo
            .draw(
                vbo,
                &indices,
                &shaders::OCTREE_PASS_1.load(),
                &uniform! {
                    initial_t_tex_dimensions: (first_pass_w, first_pass_h),

                    FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                    LightingParams: &**self.dim.as_ref().unwrap().lighting_uniform,
                    OctreeParams: &octree_params,
                    octree_texture: &gl_octree.texture,
                },
                &glium::DrawParameters {
                    multisampling: false,
                    viewport: Some(init_t_texture_viewport),
                    ..Default::default()
                },
            )
            .expect("Drawing cells (first pass)");

        // Second pass: render the final result, using the first pass to
        // determine the starting point for each ray.
        self.params
            .target
            .draw(
                vbo,
                &indices,
                &shaders::OCTREE_PASS_2.load(),
                &uniform! {
                    initial_t_texture: init_t_texture.sampled(),
                    initial_t_tex_dimensions: (first_pass_w, first_pass_h),

                    alpha: params.alpha,

                    FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                    LightingParams: &**self.dim.as_ref().unwrap().lighting_uniform,
                    OctreeParams: &octree_params,
                    octree_texture: &gl_octree.texture,
                },
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLessOrEqual,
                        write: true,
                        ..Default::default()
                    },
                    blend: glium::Blend::alpha_blending(),
                    multisampling: false,
                    ..Default::default()
                },
            )
            .expect("Drawing cells (second pass)");

        // If the mouse is hovering over a cell, and these cells are
        // interactive, draw that on the mouse picker.
        if params.interactive {
            if let Some(pixel) = self.params.mouse.pos {
                let (start, delta) = self.xform.pixel_to_local_ray(pixel);
                let raycast = crate::gridview::algorithms::raycast::intersect_octree(
                    start - octree_base.to_fvec(),
                    delta,
                    self.xform.render_cell_layer,
                    visible_octree.root.as_ref(),
                );
                if let Some(hit) = raycast {
                    let cuboid = IRect::single_cell(hit.pos_int + octree_base).to_frect();
                    let face = hit.face;
                    let modifiers = None;
                    let target_data = None;
                    self.add_mouse_target_face(cuboid, face, modifiers, target_data);
                }
            }
        }

        Ok(())
    }

    /// Adds a plane of gridlines to the overlay.
    pub fn add_gridlines_overlay(
        &mut self,
        perpendicular_axis: Axis,
        perpendicular_coordinate: BigInt,
    ) {
        let local_perpendicular_coordinate;
        if let Some(local_coord) = self
            .xform
            .global_to_local_visible_coord(perpendicular_axis, &perpendicular_coordinate)
        {
            local_perpendicular_coordinate = r64(local_coord as f64);
        } else {
            return; // The gridline plane isn't even visible.
        };

        // Compute rectangle.
        let rect: FRect3D;
        {
            let mut min = self.local_visible_rect.min().to_fvec();
            let mut max = self.local_visible_rect.max().to_fvec();
            min[perpendicular_axis] = local_perpendicular_coordinate - Z_EPSILON;
            max[perpendicular_axis] = local_perpendicular_coordinate + Z_EPSILON;
            rect = NdRect::span(min, max);
        }

        self.overlay_quads.push(OverlayQuad {
            rect,
            face: Face::positive(perpendicular_axis),
            fill: OverlayFill::Gridlines3D,
        });
        self.overlay_quads.push(OverlayQuad {
            rect,
            face: Face::negative(perpendicular_axis),
            fill: OverlayFill::Gridlines3D,
        });
    }

    /// Adds a highlight on the render cell face under the mouse cursor when
    /// using the drawing tool.
    pub fn add_hover_draw_overlay(&mut self, cell_pos: &BigVec3D, face: Face, draw_mode: DrawMode) {
        self.add_hover_overlay(
            cell_pos,
            face,
            draw_mode.fill_color(),
            draw_mode.outline_color(),
        );
    }
    /// Adds a highlight on the render cell face under the mouse cursor when
    /// using the selection tool.
    pub fn add_hover_select_overlay(&mut self, cell_pos: &BigVec3D, face: Face) {
        use crate::colors::hover::*;
        self.add_hover_overlay(cell_pos, face, SELECT_FILL, SELECT_OUTLINE);
    }
    /// Adds a highlight on the render cell under the mouse cursor.
    fn add_hover_overlay(
        &mut self,
        cell_pos: &BigVec3D,
        face: Face,
        fill_color: Srgba,
        outline_color: Srgb,
    ) {
        let local_rect = IRect::single_cell(self.clamp_int_pos_to_visible(cell_pos));
        let local_frect = self._adjust_rect_for_overlay(local_rect);
        let width = r64(HOVER_HIGHLIGHT_WIDTH);
        self.add_cuboid_fill_overlay(local_frect, fill_color);
        self.add_face_outline_overlay(local_rect, face, outline_color, width);
    }

    /// Adds a highlight around the selection when the selection includes cells.
    pub fn add_selection_cells_highlight_overlay(&mut self, selection_rect: &BigRect3D) {
        use crate::colors::selection::*;
        self.add_selection_highlight_overlay(selection_rect, CELLS_FILL, CELLS_OUTLINE)
    }
    /// Adds a highlight around the selection when the selection does not
    /// include cells.
    pub fn add_selection_region_highlight_overlay(&mut self, selection_rect: &BigRect3D) {
        use crate::colors::selection::*;
        self.add_selection_highlight_overlay(selection_rect, REGION_FILL, REGION_OUTLINE)
    }
    /// Adds a highlight around the selection.
    fn add_selection_highlight_overlay(
        &mut self,
        selection_rect: &BigRect3D,
        fill_color: Srgba,
        outline_color: Srgb,
    ) {
        let local_rect = self.clip_int_rect_to_visible(selection_rect);
        let local_frect = self._adjust_rect_for_overlay(local_rect);
        let width = r64(SELECTION_HIGHLIGHT_WIDTH);
        self.add_cuboid_fill_overlay(local_frect, fill_color);
        self.add_cuboid_outline_overlay(local_rect, outline_color, width);

        for &face in &FACES {
            // "Move selected cells" target.
            let binding = DragCmd::MoveSelectedCells(Some(face));
            self.add_mouse_target_face(
                local_frect,
                face,
                Some(ModifiersState::empty()),
                Some(MouseTargetData { binding }),
            );

            // "Move selection" target.
            let binding = DragCmd::MoveSelection(Some(face));
            self.add_mouse_target_face(
                local_frect,
                face,
                Some(ModifiersState::SHIFT),
                Some(MouseTargetData { binding }),
            );

            // "Move copy of cells" target.
            let binding = DragCmd::CopySelectedCells(Some(face));
            self.add_mouse_target_face(
                local_frect,
                face,
                Some(ModifiersState::CTRL),
                Some(MouseTargetData { binding }),
            );

            // "Resize selection" target.
            let binding = DragCmd::ResizeSelection3D(face);
            self.add_mouse_target_face(
                local_frect,
                face,
                Some(ModifiersState::CTRL | ModifiersState::SHIFT),
                Some(MouseTargetData { binding }),
            )
        }
    }
    /// Adds a highlight indicating how the selection will be resized.
    pub fn add_selection_resize_preview_overlay(&mut self, selection_preview_rect: &BigRect3D) {
        let local_rect = self.clip_int_rect_to_visible(selection_preview_rect);
        let local_frect = self._adjust_rect_for_overlay(local_rect);
        let width = r64(SELECTION_HIGHLIGHT_WIDTH * WIDTH_FUDGE_FACTOR_3D);
        use crate::colors::selection::*;
        self.add_cuboid_fill_overlay(local_frect, RESIZE_FILL);
        self.add_cuboid_outline_overlay(local_rect, RESIZE_OUTLINE, width);
    }
    /// Adds a highlight indicating which face of the selection will be resized.
    pub fn add_selection_face_resize_overlay(&mut self, selection_rect: &BigRect3D, face: Face) {
        let local_rect = self.clip_int_rect_to_visible(selection_rect);
        let local_frect = self._adjust_rect_for_overlay(local_rect);
        let width = r64(SELECTION_HIGHLIGHT_WIDTH * WIDTH_FUDGE_FACTOR_3D);
        let fill_color = crate::colors::selection::RESIZE_FILL;
        let outline_color = crate::colors::selection::RESIZE_OUTLINE;
        self.add_face_fill_overlay(local_frect, face, fill_color);
        self.add_back_face_fill_overlay(local_frect, face, fill_color);
        self.add_face_outline_overlay(local_rect, face, outline_color, width);
    }

    /// Adds all six faces of a filled-in cuboid with a solid color.
    fn add_cuboid_fill_overlay(&mut self, cuboid: FRect3D, fill: impl Copy + Into<OverlayFill>) {
        let fill = fill.into();
        for &face in &FACES {
            self.add_face_fill_overlay(cuboid, face, fill);
            if !fill.is_opaque() {
                self.add_back_face_fill_overlay(cuboid, face, fill);
            }
        }
    }
    /// Adds an outline around all faces of a cuboid with a solid color.
    fn add_cuboid_outline_overlay(&mut self, cuboid: IRect3D, color: Srgb, width: R64) {
        let cuboid = cuboid.to_frect();
        for &ax in Dim3D::axes() {
            let neg_corners = Face::negative(ax).corners_of(cuboid);
            let pos_corners = Face::positive(ax).corners_of(cuboid);
            for &(i1, i2) in &[(0, 0), (1, 2), (2, 1), (3, 3)] {
                self.add_line_overlay(
                    LineEndpoint3D::include(neg_corners[i1], color),
                    LineEndpoint3D::include(pos_corners[i2], color),
                    width,
                );
            }
        }
    }
    /// Adds a filled-in face of a cuboid with a solid color.
    fn add_face_fill_overlay(&mut self, cuboid: FRect3D, face: Face, fill: impl Into<OverlayFill>) {
        let rect = face.of(cuboid);
        let fill = fill.into();
        self.overlay_quads.push(OverlayQuad { rect, face, fill });
    }
    /// Adds a filled-in back-face of a cuboid with a solid color.
    fn add_back_face_fill_overlay(
        &mut self,
        cuboid: FRect3D,
        face: Face,
        fill: impl Into<OverlayFill>,
    ) {
        let rect = face.of(cuboid);
        let face = face.opposite();
        let fill = fill.into();
        self.overlay_quads.push(OverlayQuad { rect, face, fill });
    }
    /// Adds an outline around a face of a cuboid with a solid color.
    fn add_face_outline_overlay(&mut self, cuboid: IRect3D, face: Face, color: Srgb, width: R64) {
        let rect = face.of(cuboid.to_frect());
        let min = rect.min();
        let max = rect.max();
        let [ax1, ax2] = face.plane_axes();
        let mut corners = [min; 5];
        corners[1][ax1] = max[ax1];
        corners[2][ax1] = max[ax1];
        corners[2][ax2] = max[ax2];
        corners[3][ax2] = max[ax2];

        for (&c1, &c2) in corners.iter().tuple_windows() {
            self.add_line_overlay(
                LineEndpoint3D::include(c1, color),
                LineEndpoint3D::include(c2, color),
                width,
            );
        }
    }
    /// Adds crosshairs around a face of a cuboid with a solid color that fades into
    /// the gridline color toward the edges.
    fn add_face_crosshairs_overlay(
        &mut self,
        cuboid: IRect3D,
        face: Face,
        color: Srgb,
        width: R64,
    ) {
        let rect = face.of(cuboid.to_frect());
        let min = rect.min();
        let max = rect.max();
        let [ax1, ax2] = face.plane_axes();
        let a1 = min[ax1];
        let a2 = min[ax2];
        let b1 = max[ax1];
        let b2 = max[ax2];

        self.add_single_crosshair_overlay(ax1, a1, b1, min, width, color); // bottom
        self.add_single_crosshair_overlay(ax1, a1, b1, max, width, color); // top
        self.add_single_crosshair_overlay(ax2, a2, b2, min, width, color); // left
        self.add_single_crosshair_overlay(ax2, a2, b2, max, width, color); // right
    }
    fn add_single_crosshair_overlay(
        &mut self,
        parallel_axis: Axis,
        a: R64,
        b: R64,
        position: FVec3D,
        width: R64,
        color: Srgb,
    ) {
        let endpoint_pairs = self.make_crosshair_endpoints(parallel_axis, a, b, position, color);
        for [start, end] in endpoint_pairs {
            self.add_line_overlay(start, end, width);
        }
    }
    fn add_line_overlay(&mut self, mut start: LineEndpoint3D, mut end: LineEndpoint3D, width: R64) {
        let (rect, axis) = self.make_line_ndrect(&mut start, &mut end, width);
        let fill = OverlayFill::gradient(axis, start.color, end.color);
        self.add_cuboid_fill_overlay(rect, fill);
    }

    /// Adds a visualization for the ND-tree path to a cell.
    pub fn add_ndtree_visualization(
        &mut self,
        min_layer: Layer,
        max_layer: Layer,
        base_pos: &BigVec3D,
        cell_pos: &BigVec3D,
    ) {
        if min_layer >= max_layer {
            return;
        }

        let single_cell = BigRect::single_cell(cell_pos.clone());
        let color1 = crate::colors::debug::NDTREE_COLOR_1.into_linear();
        let color2 = crate::colors::debug::NDTREE_COLOR_2.into_linear();
        let width = r64(0.0);

        for layer in (min_layer.0..=max_layer.0).map(Layer) {
            let cuboid = layer.round_rect_with_base_pos(single_cell.clone(), base_pos);
            let proportion = {
                let cur = layer.0 as f32;
                let min = min_layer.0 as f32;
                let max = max_layer.0 as f32;
                (cur - min) / (max - min)
            };
            let color = Srgb::from_linear(color1.mix(&color2, proportion));
            self.add_cuboid_outline_overlay(self.clip_int_rect_to_visible(&cuboid), color, width);
        }
    }

    /// Draws many transparent intersecting planes of different colors to test
    /// transparent splitting and sorting.
    pub fn draw_transparency_test(&mut self) {
        let min = NdVec([0, 0, 5]);
        let max = NdVec([5, 5, 10]);

        for &ax in Dim3D::axes() {
            for coord in (min[ax] + 1)..(max[ax] + 1) {
                let mut min = min;
                let mut max = max;
                min[ax] = coord;
                max[ax] = coord;
                let global_rect = IRect3D::span(min, max).to_bigrect();
                let rect = self.clip_int_rect_to_visible(&global_rect).to_frect();
                let r = (ax == Axis::X) as u8 as f32;
                let g = (ax == Axis::Y) as u8 as f32;
                let b = (ax == Axis::Z) as u8 as f32;
                self.overlay_quads.push(OverlayQuad {
                    rect,
                    face: Face::positive(ax),
                    fill: OverlayFill::Solid(Srgba::new(r, g, b, 0.25)),
                });
                self.overlay_quads.push(OverlayQuad {
                    rect,
                    face: Face::negative(ax),
                    fill: OverlayFill::Solid(Srgba::new(r, g, b, 0.25)),
                });
            }
        }
    }

    /// Remove quads that are not visible from the current camera position.
    fn cull_backface_quads(&mut self) {
        // Get global camera positiion.
        let camera_pos = self.viewpoint.camera_pos();
        // Get local camera position.
        let camera_pos = self.xform.global_to_local_float(&camera_pos).unwrap();
        // Cull those quads!
        self.overlay_quads
            .retain(|quad| quad.is_visible_from(camera_pos));
    }
    /// Sort quads by depth, splitting as necessary.
    fn depth_sort_quads(&mut self) {
        let transparent_quads = self
            .overlay_quads
            .iter()
            .copied()
            .filter(|quad| !quad.is_opaque())
            .collect_vec();

        let x_quads = Self::filter_and_sort_quads_on_axis(&transparent_quads, Axis::X);
        let y_quads = Self::filter_and_sort_quads_on_axis(&transparent_quads, Axis::Y);
        let z_quads = Self::filter_and_sort_quads_on_axis(&transparent_quads, Axis::Z);

        let mut sorted_transparent_quads = x_quads;
        sorted_transparent_quads =
            Self::split_and_intersperse_quads(sorted_transparent_quads, &y_quads);
        sorted_transparent_quads =
            Self::split_and_intersperse_quads(sorted_transparent_quads, &z_quads);

        // Draw all opaque quads first, then all the transparent quads,
        // back-to-front.
        self.overlay_quads.retain(|quad| quad.is_opaque());
        self.overlay_quads
            .extend_from_slice(&sorted_transparent_quads);
    }
    /// Filters the quads to only those with a particular normal axis, then sort
    /// those quads back-to-front.
    fn filter_and_sort_quads_on_axis(quads: &[OverlayQuad], axis: Axis) -> Vec<OverlayQuad> {
        quads
            .iter()
            .copied()
            .filter(|q| q.face.normal_axis() == axis)
            .sorted_by_key(|q| q.plane())
            .collect_vec()
    }
    /// Splits the quads in the first list at each intersection with a quad from
    /// the second list, and sort all quads back-to-front. Both initial lists
    /// must be initially sorted back-to-front.
    fn split_and_intersperse_quads(
        quads: Vec<OverlayQuad>,
        splits: &[OverlayQuad],
    ) -> Vec<OverlayQuad> {
        let mut ret = vec![];
        let mut remaining_in_front = quads;
        for &split_quad in splits {
            remaining_in_front = remaining_in_front
                .into_iter()
                .filter_map(|remaining_quad| {
                    let [behind, in_front] = remaining_quad.split_at_plane(split_quad.plane());
                    ret.extend(behind);
                    in_front
                })
                .collect_vec();
            ret.push(split_quad);
        }
        ret.extend_from_slice(&remaining_in_front);
        ret
    }

    fn add_mouse_target_face(
        &mut self,
        cuboid: FRect3D,
        face: Face,
        modifiers: Option<ModifiersState>,
        data: Option<MouseTargetData>,
    ) {
        let corners = face.corners_of(cuboid);
        let target_id = data.map(|data| self.add_mouse_target(data)).unwrap_or(0);
        self.add_mouse_target_tri(modifiers, [corners[0], corners[1], corners[2]], target_id);
        self.add_mouse_target_tri(modifiers, [corners[3], corners[2], corners[1]], target_id);
    }

    fn _adjust_rect_for_overlay(&self, rect: IRect3D) -> FRect3D {
        // Avoid Z-fighting with cells by expanding the rectangle a tiny bit.
        rect.to_frect()
            .offset_min_max(r64(-CUBOID_OVERLAY_PADDING), r64(CUBOID_OVERLAY_PADDING))
    }
}

/// Simple rectangle in a cell overlay.
#[derive(Debug, Copy, Clone)]
pub struct OverlayQuad {
    /// 3D region; all coordinates must be in `face` plane.
    rect: FRect3D,
    /// Facing direction of the quad.
    face: Face,
    /// Fill (determines color and shader program).
    fill: OverlayFill,
}
impl OverlayQuad {
    /// Returns `true` if the quad is definitely 100% opaque.
    pub fn is_opaque(self) -> bool {
        self.fill.is_opaque()
    }
    /// Returns `true` if the front of the quad faces the camera, or `false` if
    /// the back of the quad faces the camera.
    pub fn is_visible_from(self, camera_pos: FVec3D) -> bool {
        let quad_coordinate = self.perpendicular_coordinate();
        let camera_coordinate = camera_pos[self.face.normal_axis()];
        match self.face.sign() {
            Sign::Minus => camera_coordinate < quad_coordinate,
            Sign::NoSign => unreachable!(),
            Sign::Plus => camera_coordinate > quad_coordinate,
        }
    }

    /// Returns the infinite plane that the quad is inside.
    pub fn plane(self) -> SignedPlane {
        SignedPlane {
            face: self.face,
            coordinate: self.perpendicular_coordinate(),
        }
    }
    /// Returns the coordinate of the face along its normal axis.
    pub fn perpendicular_coordinate(self) -> R64 {
        self.rect.min()[self.face.normal_axis()]
    }

    /// Splits the quad at the given plane, returning the portion behind the
    /// plane and the portion in front of the plane in that order.
    pub fn split_at_plane(self, plane: SignedPlane) -> [Option<Self>; 2] {
        let split_axis = plane.face.normal_axis();
        let split_coordinate = plane.coordinate;

        let mut negative_side = None;
        let mut positive_side = None;
        if self.face.normal_axis() == split_axis {
            match self.perpendicular_coordinate().cmp(&split_coordinate) {
                std::cmp::Ordering::Less => negative_side = Some(self),
                std::cmp::Ordering::Equal => return [None, Some(self)], // preserve original order
                std::cmp::Ordering::Greater => positive_side = Some(self),
            }
        } else {
            let min = self.rect.min();
            let max = self.rect.max();
            if max[split_axis] <= split_coordinate {
                negative_side = Some(self);
            } else if min[split_axis] >= split_coordinate {
                positive_side = Some(self);
            } else {
                // We actually have to split the rectangle.

                let mut neg_max = max;
                neg_max[split_axis] = split_coordinate;
                negative_side = Some(Self {
                    rect: NdRect::span(min, neg_max),
                    face: self.face,
                    fill: self.fill,
                });

                let mut pos_min = min;
                pos_min[split_axis] = split_coordinate;
                positive_side = Some(Self {
                    rect: NdRect::span(pos_min, max),
                    face: self.face,
                    fill: self.fill,
                });
            }
        }

        match plane.face.sign() {
            Sign::Minus => [positive_side, negative_side],
            Sign::NoSign => unreachable!(),
            Sign::Plus => [negative_side, positive_side],
        }
    }

    /// Returns the four vertices to render the quad.
    pub fn verts(self) -> [Vertex3D; 4] {
        let [ax1, ax2] = self.face.plane_axes();
        let min = self.rect.min();
        let max = self.rect.max();
        let mut positions = [min; 4];
        positions[1][ax1] = max[ax1];
        positions[2][ax2] = max[ax2];
        positions[3][ax1] = max[ax1];
        positions[3][ax2] = max[ax2];

        let normal = self.face.normal();
        let colors = self.fill.vertex_colors(self.face);
        [
            Vertex3D::new(positions[0].to_f32_array(), normal, colors[0]),
            Vertex3D::new(positions[1].to_f32_array(), normal, colors[1]),
            Vertex3D::new(positions[2].to_f32_array(), normal, colors[2]),
            Vertex3D::new(positions[3].to_f32_array(), normal, colors[3]),
        ]
    }
    /// Returns the shader program that should be used to render the quad:
    /// either `RGBA_3D` or `GRIDLINES`.
    pub fn shader_program(self) -> &'static shaders::WrappedShader {
        match self.fill {
            OverlayFill::Solid(_) | OverlayFill::Gradient(_, _, _) => &*shaders::RGBA_3D,
            OverlayFill::Gridlines3D => &*shaders::GRIDLINES_3D,
        }
    }
}

/// Axis-aligned plane in local space.
///
/// A total ordering is defined on this type that is guaranteed to sort parallel
/// planes in back-to-front draw order.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SignedPlane {
    pub face: Face,
    pub coordinate: R64,
}
impl PartialOrd for SignedPlane {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for SignedPlane {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.face
            .normal_axis()
            .cmp(&other.face.normal_axis())
            .then(self.face.sign().cmp(&other.face.sign()))
            .then_with(|| match self.face.sign() {
                Sign::Minus => self.coordinate.cmp(&other.coordinate).reverse(),
                Sign::NoSign => unreachable!(),
                Sign::Plus => self.coordinate.cmp(&other.coordinate),
            })
    }
}
