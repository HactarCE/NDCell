use cgmath::prelude::*;
use cgmath::{Deg, Matrix4};
use std::convert::TryFrom;

use ndcell_core::prelude::*;

use crate::ext::*;
use crate::Scale;

pub type CellTransform2D = NdCellTransform<Dim2D>;
pub type CellTransform3D = NdCellTransform<Dim3D>;

/// Vertical field-of-view.
const FOV: Deg<f32> = Deg(60.0);
/// Near clipping plane.
pub const NEAR_PLANE: f32 = 1.0;
/// Far clipping plane.
pub const FAR_PLANE: f32 = 65536.0;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ProjectionType {
    Orthographic,
    Perspective,
}

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
/// 1. Global space
///     - Unit: individual cell
///     - Origin: global origin
///     - Numeric type: `BigInt` or `FixedPoint`
/// 2. Local space
///     - Unit: render cell (power-of-2 square of cells)
///     - Origin: local origin, near viewpoint center
///     - Orientation: global
///     - Numeric type: `isize` or floating-point
/// 3. Camera space
///     - Unit: scaled unit (the "scale" is displayed as the ratio of scaled
///       units per cell)
///     - Origin: camera center
///     - Orientation: relative to camera (+Z = forward)
///     - Numeric type: floating-point
/// 4. Screen space (normalized device coordinates)
///     - Unit: half-target (render target X & Y range from -1.0 to +1.0)
///     - Origin: center of render target
///     - Orientation: relative to render target (+Z = backward; +Y = up)
///     - Numeric type: floating-point
/// 5. Pixel space
///     - Unit: pixel
///     - Origin: top left of render target
///     - Orientation: relative to render target (+Z = backward; +Y = down)
///     - Numeric type: floating-point
///
/// In 3D, pixel space and screen space have the perspective transformation
/// applied while camera space does not.
///
/// When displaying large patterns, individual cells may be smaller than a
/// single pixel, so we group cells into "render cells;" each render cell is a
/// power-of-2-sized square/cube of cells that is rendered as a single unit.
///
/// Because coordinates in global space may be arbitrarily large, conversion
/// from global space to local space cannot be done using floating-point
/// numbers; all other conversions use `cgmath::Matrix4<f32>`, which can be
/// easily converted to `[[f32; 4]; 4]` to pass to OpenGL. To convert from
/// global space to local space, we subtract the global cell offset and divide
/// by the size of a render cell (equivalent to a right-shift by the render cell
/// layer).
///
/// `render_cell_transform` converts from local space to camera space,
/// `projection_transform` converts from camera space to screen space, and
/// `pixel_transform` converts from screen space to pixel space.
#[derive(Debug, Clone, PartialEq)]
pub struct NdCellTransform<D: Dim> {
    /// Cell origin for "local" local space.
    pub origin: BigVec<D>,
    /// Layer of render cells.
    pub render_cell_layer: Layer,
    /// Scale of render cells.
    pub render_cell_scale: Scale,
    /// Transformation matrix from local space to camera space (including
    /// scale).
    pub render_cell_transform: Matrix4<f32>,
    /// Transformation matrix from camera space to screen space.
    pub projection_transform: Matrix4<f32>,
    /// Transformation matrix from screen space to pixel space.
    pub pixel_transform: Matrix4<f32>,

    /// Width of the render target.
    pub target_w: f32,
    /// Height of the render target.
    pub target_h: f32,
}

impl<D: Dim> NdCellTransform<D> {
    pub fn new(
        viewpoint_center: FixedVec<D>,
        render_cell_layer: Layer,
        render_cell_scale: Scale,
        camera_transform: Matrix4<f32>,
        projection_type: ProjectionType,
        (target_w, target_h): (u32, u32),
    ) -> Self {
        // Round `origin` to the nearest render cell.
        let origin = (viewpoint_center.clone() / &FixedPoint::from(render_cell_layer.big_len()))
            .round()
            * &render_cell_layer.big_len();

        let w = target_w as f32;
        let h = target_h as f32;

        Self {
            origin: origin.clone(),
            render_cell_layer,
            render_cell_scale,
            render_cell_transform: camera_transform
                * Self::scale_transform(render_cell_scale)
                * Self::translate_transform(viewpoint_center, origin, render_cell_layer),
            projection_transform: match projection_type {
                ProjectionType::Orthographic => Self::orthographic_projection(w, h),
                ProjectionType::Perspective => Self::perspective_projection(w, h),
            },
            pixel_transform: Self::pixel_transform(w, h),

            target_w: w,
            target_h: h,
        }
    }

    fn scale_transform(scale: Scale) -> Matrix4<f32> {
        let factor = scale.units_per_cell().raw() as f32;
        match D::NDIM {
            1 => Matrix4::from_nonuniform_scale(factor, 1.0, 1.0),
            2 => Matrix4::from_nonuniform_scale(factor, factor, 1.0),
            3 => Matrix4::from_scale(factor),
            _ => unimplemented!(),
        }
    }
    fn translate_transform(
        global_viewpoint_center: FixedVec<D>,
        origin: BigVec<D>,
        render_cell_layer: Layer,
    ) -> Matrix4<f32> {
        let local_viewport_center = ((global_viewpoint_center - origin.to_fixedvec())
            >> render_cell_layer.to_u32())
        .to_fvec();
        let offset = -local_viewport_center;
        Matrix4::from_translation(match AnyDimFVec::from(offset) {
            AnyDimVec::Vec1D(v) => v.to_cgmath_vec3(),
            AnyDimVec::Vec2D(v) => v.to_cgmath_vec3(),
            AnyDimVec::Vec3D(v) => v.to_cgmath_vec3(),
            _ => unimplemented!(),
        })
    }
    fn orthographic_projection(target_w: f32, target_h: f32) -> Matrix4<f32> {
        // Scale to GL normalized device coordinates.
        Matrix4::from_nonuniform_scale(2.0 / target_w, 2.0 / target_h, 1.0)
    }
    fn perspective_projection(target_w: f32, target_h: f32) -> Matrix4<f32> {
        let perspective_transform = Matrix4::from(cgmath::PerspectiveFov {
            fovy: FOV.into(),
            aspect: target_w / target_h,
            near: NEAR_PLANE,
            far: FAR_PLANE,
        });
        // Negate Z axis, so that +Z is forward.
        perspective_transform * Matrix4::from_nonuniform_scale(1.0, 1.0, -1.0)
    }
    fn pixel_transform(target_w: f32, target_h: f32) -> Matrix4<f32> {
        // Negate the vertical axis because Glutin measures window coordinates
        // from the top-left corner, but we want the Y coordinate increasing
        // upwards. Also, offset by half a pixel because OpenGL considers pixels
        // to be centered on half-integers.
        Matrix4::from_translation(cgmath::vec3(0.5, 0.5, 0.0))
            * Matrix4::from_nonuniform_scale(target_w as f32 / 2.0, -(target_h as f32 / 2.0), 1.0)
            * Matrix4::from_translation(cgmath::vec3(1.0, -1.0, 0.0))
    }

    /// Returns a GL-compatible matrix representing the transformation from
    /// local space to screen space.
    pub fn gl_matrix(&self) -> [[f32; 4]; 4] {
        (self.projection_transform * self.render_cell_transform).into()
    }
    /// Returns a GL-compatible matrix representing the transformation from
    /// local space to screen space, plus a quarter-pixel offset to align
    /// elements that would otherwise land on a pixel boundary (such as
    /// single-pixel-wide gridlines).
    pub fn gl_matrix_with_subpixel_offset(&self) -> [[f32; 4]; 4] {
        let local_to_screen_transform = self.projection_transform * self.render_cell_transform;

        let quarter_pixel = self.render_cell_scale.cells_per_unit().raw() as f32 / 4.0;
        let subpixel_translation = cgmath::vec3(quarter_pixel, quarter_pixel, 0.0);
        let subpixel_translation = Matrix4::from_translation(
            local_to_screen_transform.transform_vector(subpixel_translation),
        );

        (subpixel_translation * local_to_screen_transform).into()
    }

    /// Converts a fixed-point position in global space to a floating-point
    /// position in local space. Returns `None` if it doesn't fit.
    pub fn global_to_local_float(&self, pos: &FixedVec<D>) -> Option<FVec<D>> {
        let local_pos = (pos - &self.origin.to_fixedvec()) >> self.render_cell_layer.to_u32();
        FVec::try_from_fn(|ax| R64::try_new(local_pos[ax].to_f64()?))
    }
    /// Converts an integer position from global space to local space. Returns
    /// `None` if it doesn't fit.
    pub fn global_to_local_int(&self, pos: &BigVec<D>) -> Option<IVec<D>> {
        let local_pos = (pos - &self.origin) >> self.render_cell_layer.to_u32();
        IVec::try_from_fn(|ax| local_pos[ax].to_isize())
    }

    /// Converts a floating-point position in local space to a fixed-point
    /// position in global space.
    pub fn local_to_global_float(&self, pos: FVec<D>) -> FixedVec<D> {
        (pos.to_fixedvec() << self.render_cell_layer.to_u32()) + self.origin.to_fixedvec()
    }
    /// Converts an integer position from local space to global space.
    pub fn local_to_global_int(&self, pos: IVec<D>) -> BigVec<D> {
        (pos.to_bigvec() << self.render_cell_layer.to_u32()) + &self.origin
    }

    /// Converts an integer rectangle from global space to local space. Returns
    /// `None` if it doesn't fit.
    pub fn global_to_local_int_rect(&self, rect: &BigRect<D>) -> Option<IRect<D>> {
        let min = self.global_to_local_int(&rect.min())?;
        let max = self.global_to_local_int(&rect.max())?;
        for &ax in D::axes() {
            // Ensure the size of the rectangle won't overflow `isize`.
            max[ax].checked_sub(min[ax])?.checked_add(1)?;
        }
        Some(IRect::span(min, max))
    }
}

impl CellTransform2D {
    /// Returns the global position at the given pixel position on the screen.
    pub fn pixel_to_global_pos(&self, pixel: FVec2D) -> Option<FixedVec2D> {
        let local_pos = self.pixel_to_local_pos(pixel)?;
        Some(self.local_to_global_float(local_pos))
    }
    /// Returns the local position at the given pixel position on the screen.
    pub fn pixel_to_local_pos(&self, pixel: FVec2D) -> Option<FVec2D> {
        // Convert to `cgmath` type for matrix math.
        let pixel = pixel.to_cgmath_point3();
        // Convert from pixels to screen space to camera space to local space.
        let local_pos =
            (self.pixel_transform * self.projection_transform * self.render_cell_transform)
                .inverse_transform()?
                .transform_point(pixel);
        Some(NdVec([
            R64::try_new(local_pos.x as f64)?,
            R64::try_new(local_pos.y as f64)?,
        ]))
    }
    pub fn screen_to_local_pos(&self, screen_pos: FVec2D) -> Option<FVec2D> {
        // Convert to `cgmath` type for matrix math.
        let screen_pos = screen_pos.to_cgmath_point3();
        // Convert from pixels to screen space.
        let local_pos = (self.projection_transform * self.render_cell_transform)
            .inverse_transform()?
            .transform_point(screen_pos);
        Some(NdVec([
            R64::try_new(local_pos.x as f64)?,
            R64::try_new(local_pos.y as f64)?,
        ]))
    }

    /// Returns the local rectangle spanning the screen.
    pub fn local_screen_rect(&self) -> FRect2D {
        FRect::span(
            self.screen_to_local_pos(FVec2D::repeat(r64(-1.0))).unwrap(),
            self.screen_to_local_pos(FVec2D::repeat(r64(1.0))).unwrap(),
        )
    }
}

impl CellTransform3D {
    /// Returns the local position at the given pixel position on the screen at
    /// the given scaled-unit Z coordinate.
    pub fn pixel_to_local_pos(&self, pixel: FVec2D, z: f32) -> Option<FVec3D> {
        // Convert to `cgmath` type for matrix math.
        let mut pixel = pixel.to_cgmath_point3();
        // Apply the perspective transformation to the Z coordinate to see where
        // it ends up.
        pixel.z = self
            .projection_transform
            .transform_point(cgmath::Point3::new(0.0, 0.0, z))
            .z;
        // Convert from pixels to screen space to camera space to local space.
        let local_pos =
            (self.pixel_transform * self.projection_transform * self.render_cell_transform)
                .inverse_transform()?
                .transform_point(pixel);
        Some(NdVec([
            R64::try_new(local_pos.x as f64)?,
            R64::try_new(local_pos.y as f64)?,
            R64::try_new(local_pos.z as f64)?,
        ]))
    }
    /// Returns the global position at the given pixel position on the screen at
    /// the given scaled-unit Z coordiniate.
    pub fn pixel_to_global_pos(&self, pixel: FVec2D, z: f32) -> Option<FixedVec3D> {
        let local_pos = self.pixel_to_local_pos(pixel, z)?;
        Some(self.local_to_global_float(local_pos))
    }

    /// Returns the global position at the given pixel position on the screen
    /// intersecting an axis-aligned plane.
    pub fn pixel_to_global_pos_in_plane(
        &self,
        pixel: FVec2D,
        plane: (Axis, &FixedPoint),
    ) -> Option<FixedVec3D> {
        let (plane_axis, plane_pos) = plane;
        let global_cell_0 = self.pixel_to_global_pos(pixel, NEAR_PLANE)?;
        let global_cell_1 = self.pixel_to_global_pos(pixel, FAR_PLANE)?;
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
