use cgmath::prelude::*;
use cgmath::{Deg, Rad};

use super::*;

#[derive(Debug, Clone)]
pub struct Camera3D {
    pub pivot: FixedVec3D,
    pitch: Rad<f32>, // -PI/2 to +PI/2
    yaw: Rad<f32>,   // 0 to 2*PI
    pub log2_distance: f32,
}
impl Default for Camera3D {
    fn default() -> Self {
        Self {
            pivot: FixedVec3D::repeat(r64(0.5).into()),
            pitch: Deg(30.0).into(),
            yaw: Deg(20.0).into(),
            log2_distance: 2.0,
        }
    }
}
impl Camera3D {
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
        let distance = FixedPoint::from(r64(self.log2_distance as f64)).exp_base2();
        &self.pivot + self.look_vector().to_fixedvec() * &distance
    }
}
