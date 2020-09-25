use cgmath::prelude::*;
use cgmath::Matrix4;
use std::convert::TryFrom;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

pub type CellTransform2D = NdCellTransform<Dim2D>;
pub type CellTransform3D = NdCellTransform<Dim3D>;

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
    pub fn pixel_to_cell_3d(&self, pixel: FVec2D) -> Option<FixedVec3D> {
        self.as_3d().and_then(|this| this.pixel_to_cell(pixel))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NdCellTransform<D: Dim> {
    /// Global position of the origin of render cell space.
    pub global_cell_offset: BigVec<D>,
    /// Layer of render cells.
    pub render_cell_layer: Layer,
    /// Transformation matrix from render cell space to screen space.
    pub render_cell_transform: Matrix4<f32>,
    /// Transformation matrix from screen space to pixel space.
    pub pixel_transform: Matrix4<f32>,
}

impl<D: Dim> NdCellTransform<D> {
    pub fn new(
        global_offset: BigVec<D>,
        render_cell_layer: Layer,
        render_cell_transform: Matrix4<f32>,
        target_dimensions: (u32, u32),
    ) -> Self {
        // Negate the vertical axis because Glutin measures window coordinates
        // from the top-left corner, but we want the Y coordinate increasing
        // upwards.
        let (target_w, target_h) = target_dimensions;
        let pixel_transform =
            Matrix4::from_nonuniform_scale(target_w as f32 / 2.0, -(target_h as f32 / 2.0), 1.0)
                * &Matrix4::from_translation(cgmath::vec3(1.0, -1.0, 0.0));

        Self {
            global_cell_offset: global_offset,
            render_cell_layer,
            render_cell_transform,
            pixel_transform,
        }
    }

    pub fn gl_render_cell_transform(&self) -> [[f32; 4]; 4] {
        self.render_cell_transform.into()
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
    pub fn pixel_to_cell(&self, pixel: FVec2D) -> Option<FixedVec3D> {
        // Convert to `cgmath` type for matrix math.
        let pixel = cgmath::vec3(
            pixel[X].raw() as f32,
            pixel[Y].raw() as f32,
            -super::Camera3D::DISTANCE_TO_PIVOT as f32,
        );
        // Convert from pixels to OpenGL coordinates.
        let opengl_pos = self.pixel_transform.inverse_transform_vector(pixel)?;
        // Convert from OpenGL coordinates to local render cell coordinates.
        let local_render_cell = self
            .render_cell_transform
            .inverse_transform_vector(opengl_pos)?;
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
