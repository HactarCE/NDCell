use cgmath::{Deg, Euler, Matrix3, Quaternion, Rad};

use ndcell_core::prelude::*;

use super::{Camera, Scale};
use crate::config::{Config, ForwardAxis3D, UpAxis3D};

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
    /// Returns the unit vector along which the camera is looking.
    pub fn look_vector(&self) -> FVec3D {
        fvec3d_from_euler(Axis::Z, self.orientation())
    }
    /// Returns the real position of the camera (not the pivot).
    pub fn camera_pos(&self) -> FixedVec3D {
        let distance = self.scale.factor() * FixedPoint::from(r64(Self::DISTANCE_TO_PIVOT));
        &self.pivot + self.look_vector().to_fixedvec() * &distance
    }

    fn flat_orientation(&self) -> Euler<Rad<f32>> {
        let mut rot = self.orientation();
        rot.x = Rad(0.0); // pitch = 0
        rot
    }

    /// Returns the unit vector for forward motion.
    pub fn forward_vector(&self, config: ForwardAxis3D) -> FVec3D {
        let rot = match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Aligned => self.orientation(),
            ForwardAxis3D::Flat | ForwardAxis3D::FlatAligned => self.flat_orientation(),
        };
        let look = fvec3d_from_euler(Axis::Z, rot);
        match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Flat => look,
            ForwardAxis3D::Aligned | ForwardAxis3D::FlatAligned => {
                let ax = look.max_axis(|_ax, component| component.abs());
                FVec3D::unit(ax) * look[ax].signum()
            }
        }
    }
    /// Returns the unit vector for upward motion.
    pub fn up_vector(&self, config: UpAxis3D) -> FVec3D {
        match config {
            UpAxis3D::Camera => fvec3d_from_euler(Axis::Y, self.orientation()),
            UpAxis3D::Fixed => FVec3D::unit(Axis::Y),
        }
    }
    /// Returns the unit vector for sideways motion.
    pub fn right_vector(&self, config: ForwardAxis3D) -> FVec3D {
        let look = fvec3d_from_euler(Axis::X, self.orientation());
        match config {
            ForwardAxis3D::Camera | ForwardAxis3D::Flat => look,
            ForwardAxis3D::Aligned | ForwardAxis3D::FlatAligned => {
                let ax = look.max_axis(|_ax, component| component.abs());
                FVec3D::unit(ax) * look[ax].signum()
            }
        }
    }

    pub fn move_pivot_by_pixels(&mut self, mut delta: FixedVec3D, config: &Config) {
        delta = self.scale().pixels_to_cells(delta);
        let right = self.right_vector(config.ctrl.fwd_axis_3d);
        let up = self.up_vector(config.ctrl.up_axis_3d);
        let fwd = self.forward_vector(config.ctrl.fwd_axis_3d);
        println!("{:?}, {:?}, {:?}", right, up, fwd);
        self.pivot += right.to_fixedvec() * &delta[Axis::X];
        self.pivot += up.to_fixedvec() * &delta[Axis::Y];
        self.pivot += fwd.to_fixedvec() * &delta[Axis::Z];
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

fn fvec3d_from_euler(axis: Axis, euler: impl Into<Euler<Rad<f32>>>) -> FVec3D {
    let unit_vec = match axis {
        Axis::X => cgmath::Vector3::unit_x(),
        Axis::Y => cgmath::Vector3::unit_y(),
        Axis::Z => cgmath::Vector3::unit_z(),
        _ => panic!("Invalid 3D axis"),
    };
    let v = Matrix3::from(euler.into()) * unit_vec;
    NdVec([r64(v.x as f64), r64(v.y as f64), r64(v.z as f64)])
}
