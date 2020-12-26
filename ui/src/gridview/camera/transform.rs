use cgmath::prelude::*;
use cgmath::{Deg, Matrix4};
use std::convert::TryFrom;

use ndcell_core::prelude::*;
use Axis::{X, Y};

pub type CellTransform2D = NdCellTransform<Dim2D>;
pub type CellTransform3D = NdCellTransform<Dim3D>;

/// Vertical field-of-view.
const FOV: Deg<f32> = Deg(60.0);
/// Near clipping plane.
pub const NEAR_PLANE: f32 = 1.0;
/// Far clipping plane.
pub const FAR_PLANE: f32 = 65536.0;

#[derive(Debug, Clone)]
pub enum CellTransform {
    Some2D(CellTransform2D),
    Some3D(CellTransform3D),
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
impl<'a, D: Dim> TryFrom<&'a CellTransform> for &'a NdCellTransform<D> {
    type Error = ();

    fn try_from(value: &'a CellTransform) -> Result<Self, Self::Error> {
        match D::NDIM {
            2 => Ok(unsafe {
                std::mem::transmute::<&'a NdCellTransform<Dim2D>, &'a NdCellTransform<D>>(
                    value.as_2d().ok_or(())?,
                )
            }),
            3 => Ok(unsafe {
                std::mem::transmute::<&'a NdCellTransform<Dim3D>, &'a NdCellTransform<D>>(
                    value.as_3d().ok_or(())?,
                )
            }),
            _ => Err(()),
        }
    }
}
impl CellTransform {
    pub fn ndim(&self) -> usize {
        match self {
            CellTransform::Some2D(_) => 2,
            CellTransform::Some3D(_) => 3,
        }
    }
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
}

/// Transformation between five coordinate spaces for rendering and input
/// handling.
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
/// When displaying large patterns, individual cells may be smaller than a
/// single pixel, so we group cells into "render cells;" each render cell is a
/// power-of-2-sized square/cube of cells that is rendered as a single unit.
///
/// Because coordinates in cell space may be arbitrarily large, conversion from
/// cell space to render cell space cannot be done using floating-point numbers;
/// all other conversions use `cgmath::Matrix4<f32>`, which can be easily
/// converted to `[[f32; 4]; 4]` to pass to OpenGL. To convert from cell space
/// to render cell space, we subtract the global cell offset and divide by the
/// size of a render cell (equivalent to a right-shift by the render cell
/// layer).
///
/// `render_cell_transform` converts from render cell space to camera space,
/// `projection_transform` converts from camera space to screen space, and
/// `pixel_transform` converts from screen space to pixel space.
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

    pub fn global_cell_to_local_render_cell(&self, pos: &BigVec<D>) -> Option<IVec<D>> {
        let local_render_cell = (pos - &self.global_cell_offset) >> self.render_cell_layer.to_u32();
        IVec::try_from_fn(|ax| local_render_cell[ax].to_isize())
    }
    pub fn global_cell_to_local_render_cell_rect(&self, rect: &BigRect<D>) -> Option<IRect<D>> {
        let min = self.global_cell_to_local_render_cell(&rect.min())?;
        let max = self.global_cell_to_local_render_cell(&rect.max())?;
        for &ax in D::axes() {
            // Ensure the size of the rectangle won't overflow `isize`.
            max[ax].checked_sub(min[ax])?.checked_add(1)?;
        }
        Some(IRect::span(min, max))
    }

    fn pixel_transform(target_w: f32, target_h: f32) -> Matrix4<f32> {
        // Negate the vertical axis because Glutin measures window coordinates
        // from the top-left corner, but we want the Y coordinate increasing
        // upwards. Also, offset by half a pixel because OpenGL considers pixels
        // centered on half-integers.

        // Offset by half a pixel because OpenGL samples the center of a pixel to determine its value.
        Matrix4::from_translation(cgmath::vec3(-0.5, -0.5, 0.0))
            * Matrix4::from_nonuniform_scale(target_w as f32 / 2.0, -(target_h as f32 / 2.0), 1.0)
            * Matrix4::from_translation(cgmath::vec3(1.0, -1.0, 0.0))
    }
}

impl CellTransform2D {
    /// Returns the local render cell position at the given pixel position on
    /// the screen.
    pub fn pixel_to_local_render_cell(&self, pixel: FVec2D) -> Option<FVec2D> {
        // Convert to `cgmath` type for matrix math.
        let pixel = cgmath::Point3::new(pixel[X].raw() as f32, pixel[Y].raw() as f32, 0.0);
        // Convert from pixels to screen space to camera space to render cell space.
        let local_render_cell =
            (self.pixel_transform * self.projection_transform * self.render_cell_transform)
                .inverse_transform()?
                .transform_point(pixel);
        Some(NdVec([
            R64::try_new(local_render_cell.x as f64)?,
            R64::try_new(local_render_cell.y as f64)?,
        ]))
    }
    /// Returns the global cell position at the given pixel position on the
    /// screen.
    pub fn pixel_to_global_cell(&self, pixel: FVec2D) -> Option<FixedVec2D> {
        let local_render_cell = self.pixel_to_local_render_cell(pixel)?;
        // Convert from local render cell coordinates to local cell coordinates.
        let local_cell = local_render_cell.to_fixedvec() << self.render_cell_layer.to_u32();
        // Convert from local cell coordinates to global cell coordinates.
        Some(local_cell + self.global_cell_offset.to_fixedvec())
    }
}

impl CellTransform3D {
    /// Returns the local render cell position at the given pixel position on
    /// the screen at the given scaled-unit Z coordinate.
    pub fn pixel_to_local_render_cell(&self, pixel: FVec2D, z: f32) -> Option<FVec3D> {
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
        Some(NdVec([
            R64::try_new(local_render_cell.x as f64)?,
            R64::try_new(local_render_cell.y as f64)?,
            R64::try_new(local_render_cell.z as f64)?,
        ]))
    }
    /// Returns the global cell position at the given pixel position on the
    /// screen at the given scaled-unit Z coordiniate.
    pub fn pixel_to_global_cell(&self, pixel: FVec2D, z: f32) -> Option<FixedVec3D> {
        let local_render_cell = self.pixel_to_local_render_cell(pixel, z)?;
        // Convert from local render cell coordinates to local cell coordinates.
        let local_cell = local_render_cell.to_fixedvec() << self.render_cell_layer.to_u32();
        // Convert from local cell coordinates to global cell coordinates.
        Some(local_cell + self.global_cell_offset.to_fixedvec())
    }

    /// Returns the global cell position at the given pixel position on the
    /// screen intersecting an axis-aligned plane.
    pub fn pixel_to_global_cell_in_plane(
        &self,
        pixel: FVec2D,
        plane: (Axis, &FixedPoint),
    ) -> Option<FixedVec3D> {
        let (plane_axis, plane_pos) = plane;
        let global_cell_0 = self.pixel_to_global_cell(pixel, NEAR_PLANE)?;
        let global_cell_1 = self.pixel_to_global_cell(pixel, FAR_PLANE)?;
        let delta = global_cell_1 - &global_cell_0;

        if delta[plane_axis].is_zero() {
            // The delta vector is parallel to the plane.
            return None;
        }

        // How many times do we have to add `delta` to reach the plane?
        let t = (plane_pos - &global_cell_0[plane_axis]) / &delta[plane_axis];

        if !t.is_positive() {
            // The plane is behind the camera.
            return None;
        }

        Some(global_cell_0 + delta * t)
    }
}
