//! 3D grid rendering.
//!
//! Currently, only solid colors are supported, however I plan to add custom
//! models and maybe textures in the future.

use anyhow::{Context, Result};
use glium::index::PrimitiveType;
use glium::uniforms::UniformBuffer;
use glium::Surface;
use palette::{Pixel, Srgb, Srgba};
use sloth::Lazy;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

mod fog;
mod gridlines;
mod lighting;

use super::consts::*;
use super::generic::{GenericGridViewRender, GridViewRenderDimension};
use super::shaders;
use super::vertices::Vertex3D;
use super::CellDrawParams;
use crate::ext::*;
use crate::gridview::*;
use crate::math::Face;
use crate::{CONFIG, DISPLAY};
use fog::FogParams;
use gridlines::GridlineParams;
use lighting::LightingParams;

pub(in crate::gridview) type GridViewRender3D<'a> = GenericGridViewRender<'a, RenderDim3D>;

type QuadVerts = [Vertex3D; 4];
type CuboidVerts = [Option<QuadVerts>; 6];

type LazyUbo<T> = Lazy<UniformBuffer<T>, Box<dyn FnOnce() -> UniformBuffer<T>>>;

const DEPTH_TEST: glium::Depth = glium::Depth {
    test: glium::DepthTest::IfLessOrEqual,
    write: true,
    range: (0.0, 1.0),                                  // default
    clamp: glium::draw_parameters::DepthClamp::NoClamp, // default
};

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

        let octree_offset = self
            .xform
            .global_to_local_int(&visible_octree.base_pos)
            .unwrap();

        // Reborrow is necessary in order to split borrow.
        let cache = &mut *self.cache;
        let vbos = &mut cache.vbos;

        let gl_octree = cache.gl_octrees.gl_ndtree_from_node(
            (&visible_octree.root).into(),
            self.xform.render_cell_layer,
            Self::ndtree_node_color,
        )?;

        self.params
            .target
            .draw(
                &*vbos.ndtree_quad(),
                &glium::index::NoIndices(PrimitiveType::TriangleStrip),
                &shaders::OCTREE.load(),
                &uniform! {
                    matrix: self.xform.gl_matrix(),

                    octree_texture: &gl_octree.texture,
                    layer_count: gl_octree.layers,
                    root_idx: gl_octree.root_idx,

                    octree_offset: octree_offset.to_i32_array(),

                    perf_view: CONFIG.lock().gfx.octree_perf_view,

                    FogParams: &**self.dim.as_ref().unwrap().fog_uniform,
                    LightingParams: &**self.dim.as_ref().unwrap().lighting_uniform,
                },
                &glium::DrawParameters {
                    depth: DEPTH_TEST,
                    blend: glium::Blend::alpha_blending(),
                    multisampling: false,
                    ..Default::default()
                },
            )
            .expect("Drawing cells");

        Ok(())
    }

    /// Adds a plane of gridlines to the overlay.
    pub fn add_gridlines_overlay(
        &mut self,
        perpendicular_axis: Axis,
        perpendicular_coordinate: BigInt,
    ) {
        // Convert `perpendicular_coordinate` to local space (still
        // `FixedPoint`, because this number may be very large).
        let mut tmp_global_pos = self.xform.origin.clone();
        tmp_global_pos[perpendicular_axis] = perpendicular_coordinate;
        let local_perpendicular_coordinate: R64;
        if let Some(tmp_local_pos) = self.xform.global_to_local_int(&tmp_global_pos) {
            if self.local_visible_rect.contains(&tmp_local_pos) {
                local_perpendicular_coordinate = r64(tmp_local_pos[perpendicular_axis] as f64);
            } else {
                return; // The gridline plane isn't even visible.
            }
        } else {
            return; // The gridline plane isn't even visible.
        };

        // Compute rectangle.
        let rect: FRect3D;
        {
            let mut min = self.local_visible_rect.min().to_fvec();
            let mut max = self.local_visible_rect.max().to_fvec();
            min[perpendicular_axis] = local_perpendicular_coordinate;
            max[perpendicular_axis] = local_perpendicular_coordinate;
            rect = NdRect::span(min, max);
        }

        self.overlay_quads.push(OverlayQuad {
            rect,
            face: Face::positive(perpendicular_axis),
            fill: OverlayFill::Gridlines,
        });
        self.overlay_quads.push(OverlayQuad {
            rect,
            face: Face::negative(perpendicular_axis),
            fill: OverlayFill::Gridlines,
        });
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
        // TODO
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
        match self.fill {
            OverlayFill::Gridlines => false,
            OverlayFill::Solid(color) => color.alpha >= 1.0,
        }
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

        let color = match self.fill {
            OverlayFill::Gridlines => Srgba::new(0.0, 0.0, 0.0, 0.0), // ignored in vertex shader
            OverlayFill::Solid(color) => color,
        };

        [
            Vertex3D::new(positions[0].to_f32_array(), normal, color),
            Vertex3D::new(positions[1].to_f32_array(), normal, color),
            Vertex3D::new(positions[2].to_f32_array(), normal, color),
            Vertex3D::new(positions[3].to_f32_array(), normal, color),
        ]
    }
    /// Returns the shader program that should be used to render the quad:
    /// either `RGBA_3D` or `GRIDLINES`.
    pub fn shader_program(self) -> &'static shaders::WrappedShader {
        match self.fill {
            OverlayFill::Gridlines => &*shaders::GRIDLINES_3D,
            OverlayFill::Solid(_) => &*shaders::RGBA_3D,
        }
    }
}

/// Fill style for an overlay quad.
#[derive(Debug, Copy, Clone)]
pub enum OverlayFill {
    Gridlines,
    Solid(Srgba),
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
