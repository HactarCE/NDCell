use cgmath::{Deg, Euler, Matrix3, Quaternion, Rad};

use ndcell_core::prelude::*;

use super::{Camera, Scale};

#[derive(Debug, Clone, PartialEq)]
pub struct Camera3D {
    /// Scale.
    scale: Scale,
    /// Rotation.
    orientation: Euler<Rad<f32>>,
    /// Translation.
    pivot: FixedVec3D,
}

impl Default for Camera3D {
    fn default() -> Self {
        Self {
            scale: Scale::default(),
            orientation: Euler::new(Deg(30.0).into(), Deg(20.0).into(), Deg(0.0).into()),
            pivot: FixedVec3D::repeat(r64(0.5).into()),
        }
    }
}

impl Camera3D {
    /// Number of scaled units away from the pivot to position the camera.
    pub const DISTANCE_TO_PIVOT: f64 = 512.0;

    /// Returns the orientation of the camera.
    pub fn orientation(&self) -> Euler<Rad<f32>> {
        self.orientation
    }
    /// Returns the orientation of the camera.
    pub fn set_orientation(&mut self, orientation: impl Into<Euler<Rad<f32>>>) {
        self.orientation = orientation.into()
    }
    /// Returns the vector along which the camera is looking.
    pub fn look_vector(&self) -> FVec3D {
        let v = Matrix3::from(self.orientation) * cgmath::Vector3::unit_z();
        NdVec([r64(v.x as f64), r64(v.y as f64), r64(v.z as f64)])
    }
    /// Returns the real position of the camera (not the pivot).
    pub fn camera_pos(&self) -> FixedVec3D {
        let distance = self.scale.factor() * FixedPoint::from(r64(Self::DISTANCE_TO_PIVOT));
        &self.pivot + self.look_vector().to_fixedvec() * &distance
    }
}

impl Camera<Dim3D> for Camera3D {
    fn pos(&self) -> &FixedVec<Dim3D> {
        &self.pivot
    }
    fn set_pos(&mut self, pos: FixedVec<Dim3D>) {
        self.pivot = pos
    }

    fn scale(&self) -> Scale {
        self.scale
    }
    fn set_scale(&mut self, scale: Scale) {
        self.scale = scale.clamp();
    }

    fn lerp(a: &Self, b: &Self, t: R64) -> Self {
        let mut ret = a.clone();

        // The math to interpolate scale and translation is exactly the same as
        // in `Camera2D::lerp()`, so read the comments there.

        let delta_scale_factor = b.scale / a.scale;
        ret.scale_by_factor(delta_scale_factor.powf(t), None);

        let avg_scale = super::average_lerped_scale(a.scale, b.scale);
        let total_pixels_delta = avg_scale.cells_to_pixels(&b.pivot - &a.pivot);

        let zt = super::average_lerped_scale(a.scale, ret.scale);
        let pixels_delta = &total_pixels_delta * &FixedPoint::from(t);
        let cells_delta = zt.pixels_to_cells(pixels_delta);
        ret.pivot += cells_delta;

        // Interpolate rotation by converting to quaternions, doing spherical
        // interpolation, and then converting back to euler angles.
        let ori_a = Quaternion::from(a.orientation);
        let ori_b = Quaternion::from(b.orientation);
        ret.set_orientation(Quaternion::slerp(ori_a, ori_b, t.raw() as f32));

        ret
    }
}
