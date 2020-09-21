use cgmath::prelude::*;
use cgmath::{Deg, Rad};

use ndcell_core::prelude::*;

use super::{Camera, Scale};

#[derive(Debug, Clone, PartialEq)]
pub struct Camera3D {
    pivot: FixedVec3D,
    pitch: Rad<f32>, // -PI/2 to +PI/2
    yaw: Rad<f32>,   // 0 to 2*PI
    scale: Scale,
}

impl Default for Camera3D {
    fn default() -> Self {
        Self {
            pivot: FixedVec3D::repeat(r64(0.5).into()),
            pitch: Deg(30.0).into(),
            yaw: Deg(20.0).into(),
            scale: Scale::default(),
        }
    }
}

impl Camera3D {
    /// Number of scaled units away from the pivot to position the camera.
    pub const DISTANCE_TO_PIVOT: f64 = 256.0;

    pub fn pitch(&self) -> Rad<f32> {
        self.pitch
    }
    pub fn yaw(&self) -> Rad<f32> {
        self.yaw
    }
    pub fn look_vector(&self) -> FVec3D {
        NdVec([
            r64(self.pitch().cos() as f64 * self.yaw().cos() as f64),
            r64(self.pitch().cos() as f64 * self.yaw().sin() as f64),
            r64(self.pitch().sin() as f64),
        ])
    }
    pub fn pos(&self) -> FixedVec3D {
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
        todo!()
    }
}
