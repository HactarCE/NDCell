use cgmath::prelude::*;
use cgmath::{Deg, Matrix4};
use std::convert::TryFrom;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

pub type CellTransform2D = NdCellTransform<Dim2D>;
pub type CellTransform3D = NdCellTransform<Dim3D>;

/// Vertical field-of-view.
const FOV: Deg<f32> = Deg(60.0);
/// Near clipping plane.
pub const NEAR_PLANE: f32 = 1.0;
/// Far clipping plane.
pub const FAR_PLANE: f32 = 56536.0;

#[derive(Debug, Clone)]
pub enum CellTransform {
    None,
    Some2D(CellTransform2D),
    Some3D(CellTransform3D),
}
impl Default for CellTransform {
    fn default() -> Self {
        Self::None
    }
}
impl From<CellTransform2D> for CellTransform {
    fn from(ct: CellTransform2D) -> Self {
        Self::Some2D(ct)
    }
}
impl From<CellTransform3D> for CellTransform {
    fn from(ct: CellTransform3D) -> Self {
        Self::Some3D(ct)
    }
}
impl CellTransform {
    pub fn as_2d(&self) -> Option<&CellTransform2D> {
        match self {
            Self::Some2D(ret) => Some(ret),
            _ => None,
        }
    }
    pub fn as_3d(&self) -> Option<&CellTransform3D> {
        match self {
            Self::Some3D(ret) => Some(ret),
            _ => None,
        }
    }
    pub fn pixel_to_cell_2d(&self, pixel: FVec2D) -> Option<FixedVec2D> {
        self.as_2d()
            .and_then(|this| this.pixel_to_global_cell(pixel))
    }
    pub fn pixel_to_cell_3d(&self, pixel: FVec2D, z: f32) -> Option<FixedVec3D> {
        self.as_3d().and_then(|this| this.pixel_to_cell(pixel, z))
    }
}

/// Transformation between four coordinate spaces for rendering.
///
/// 1. Cell space
///     - Unit: individual cell
///     - Origin: global origin
///     - Numeric type: `BigInt` or `FixedPoint`
/// 2. Render cell space
///     - Unit: render cell (power-of-2 square of cells)
///     - Origin: bottom-left corner of visible ND-tree slice
///     - Orientation: global
///     - Numeric type: `usize` or floating-point
/// 3. Camera space
///     - Unit: scaled unit (the "scale" is displayed as the ratio of scaled
///       units per cell)
///     - Origin: camera position/pivot
///     - Orientation: relative to camera (+Z = forward)
///     - Numeric type: floating-point
/// 4. Screen space
///     - Unit: half-viewport (viewport X & Y range from -1.0 to +1.0)
///     - Origin: center of viewport
///     - Orientation: relative to viewport (+Z = backward; +Y = up)
///     - Numeric type: floating-point
/// 5. Pixel space
///     - Unit: pixel
///     - Origin: top left of viewport
///     - Orientation: relative to viewport (+Z = backward; +Y = down)
///     - Numeric type: floating-point
///
/// In 3D, pixel space and screen space have the perspective transformation
/// applied while camera space does not.
///
/// Coordinates in cell space may be arbitrarily large, so conversion from cell
/// space to render cell space cannot be done using floating-point numbers; all
/// other conversions use `cgmath` matrices. To convert from cell space to
/// render cell space, we subtract the global cell offset and divide by the size
/// of a render cell (equivalent to a right-shift by the render cell layer).
///
/// To convert from render cell space to camera space, we use the
/// `render_cell_transform`. To convert from camera space to screen space, we
/// use the `projection_transform`. To convert from screen space to pixel space,
/// we use the `pixel_transform`.
#[derive(Debug, Clone, PartialEq)]
pub struct NdCellTransform<D: Dim> {
    /// Global position of the origin of render cell space.
    pub global_cell_offset: BigVec<D>,
    /// Layer of render cells.
    pub render_cell_layer: Layer,
    /// Transformation matrix from render cell space to camera space.
    pub render_cell_transform: Matrix4<f32>,
    /// Transformation matrix from camera space to screen space.
    pub projection_transform: Matrix4<f32>,
    /// Transformation matrix from screen space to pixel space.
    pub pixel_transform: Matrix4<f32>,
}

impl<D: Dim> NdCellTransform<D> {
    pub fn new_perspective(
        global_cell_offset: BigVec<D>,
        render_cell_layer: Layer,
        render_cell_transform: Matrix4<f32>,
        (target_w, target_h): (f32, f32),
    ) -> Self {
        // Perspective projection
        let perspective_projection = cgmath::PerspectiveFov {
            fovy: FOV.into(),
            aspect: target_w / target_h,
            near: NEAR_PLANE,
            far: FAR_PLANE,
        };
        // Negate Z axis, so that +Z is forward.
        let projection_transform =
            Matrix4::from(perspective_projection) * Matrix4::from_nonuniform_scale(1.0, 1.0, -1.0);

        let pixel_transform = Self::pixel_transform(target_w, target_h);

        Self {
            global_cell_offset,
            render_cell_layer,
            render_cell_transform,
            projection_transform,
            pixel_transform,
        }
    }

    pub fn new_ortho(
        global_cell_offset: BigVec<D>,
        render_cell_layer: Layer,
        render_cell_transform: Matrix4<f32>,
        (target_w, target_h): (f32, f32),
    ) -> Self {
        // Orthographic projection (just scaling to screen coordinates)
        let projection_transform =
            Matrix4::from_nonuniform_scale(2.0 / target_w, 2.0 / target_h, 1.0);

        let pixel_transform = Self::pixel_transform(target_w, target_h);

        Self {
            global_cell_offset,
            render_cell_layer,
            render_cell_transform,
            projection_transform,
            pixel_transform,
        }
    }

    pub fn gl_matrix(&self) -> [[f32; 4]; 4] {
        (self.projection_transform * self.render_cell_transform).into()
    }

    fn pixel_transform(target_w: f32, target_h: f32) -> Matrix4<f32> {
        // Negate the vertical axis because Glutin measures window coordinates
        // from the top-left corner, but we want the Y coordinate increasing
        // upwards.
        Matrix4::from_nonuniform_scale(target_w as f32 / 2.0, -(target_h as f32 / 2.0), 1.0)
            * Matrix4::from_translation(cgmath::vec3(1.0, -1.0, 0.0))
    }
}

impl NdCellTransform<Dim2D> {
    pub fn pixel_to_local_render_cell(&self, pixel: FVec2D) -> Option<FVec2D> {
        // Convert to `cgmath` type for matrix math.
        let pixel = cgmath::Point3::new(pixel[X].raw() as f32, pixel[Y].raw() as f32, 0.0);
        // Convert from pixels to OpenGL coordinates.
        let opengl_pos = self
            .pixel_transform
            .inverse_transform()?
            .transform_point(pixel);
        // Convert from OpenGL coordinates to local render cell coordinates.
        let local_render_cell = self
            .render_cell_transform
            .inverse_transform()?
            .transform_point(opengl_pos);
        Some(NdVec([
            R64::try_new(local_render_cell.x as f64)?,
            R64::try_new(local_render_cell.y as f64)?,
        ]))
    }
    pub fn pixel_to_global_cell(&self, pixel: FVec2D) -> Option<FixedVec2D> {
        let local_render_cell = self.pixel_to_local_render_cell(pixel)?;
        // Convert from local render cell coordinates to local cell coordinates.
        let local_cell = local_render_cell.to_fixedvec() << self.render_cell_layer.to_u32();
        // Convert from local cell coordinates to global cell coordinates.
        Some(local_cell + self.global_cell_offset.to_fixedvec())
    }
}

impl NdCellTransform<Dim3D> {
    pub fn pixel_to_cell(&self, pixel: FVec2D, mut z: f32) -> Option<FixedVec3D> {
        // Apply the perspective transformation to the Z coordinate to see where
        // it ends up.
        let z = self
            .projection_transform
            .transform_point(cgmath::Point3::new(0.0, 0.0, z))
            .z;
        // Convert to `cgmath` type for matrix math.
        let pixel = cgmath::Point3::new(pixel[X].raw() as f32, pixel[Y].raw() as f32, z);
        // Convert from pixels to screen space to camera space to render cell space.
        let local_render_cell =
            (self.pixel_transform * self.projection_transform * self.render_cell_transform)
                .inverse_transform()?
                .transform_point(pixel);
        // Convert to `ndcell_core` type for big precision.
        let local_render_cell: FixedVec3D = NdVec([
            FixedPoint::try_from(local_render_cell.x as f64).ok()?,
            FixedPoint::try_from(local_render_cell.y as f64).ok()?,
            FixedPoint::try_from(local_render_cell.z as f64).ok()?,
        ]);
        // Convert from local render cell coordinates to local cell coordinates.
        let local_cell = local_render_cell << self.render_cell_layer.to_u32();
        // Convert from local cell coordinates to global cell coordinates.
        Some(local_cell + self.global_cell_offset.to_fixedvec())
    }
}
