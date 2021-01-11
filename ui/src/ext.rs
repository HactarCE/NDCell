//! Extension traits to monkeypatch functionality on types from external crates.

use cgmath::EuclideanSpace;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

pub trait NdVecConvertExt: Sized {
    type F32Array;

    fn to_cgmath_point3(self) -> cgmath::Point3<f32>;
    fn to_cgmath_vec3(self) -> cgmath::Vector3<f32> {
        self.to_cgmath_point3().to_vec()
    }
    fn to_f32_array(self) -> Self::F32Array;
}

impl NdVecConvertExt for FVec1D {
    type F32Array = f32;

    fn to_cgmath_point3(self) -> cgmath::Point3<f32> {
        let x = self.to_f32_array();
        cgmath::point3(x, 0.0, 0.0)
    }
    fn to_f32_array(self) -> Self::F32Array {
        self[X].raw() as f32
    }
}
impl NdVecConvertExt for FVec2D {
    type F32Array = [f32; 2];

    fn to_cgmath_point3(self) -> cgmath::Point3<f32> {
        let [x, y] = self.to_f32_array();
        cgmath::point3(x, y, 0.0)
    }
    fn to_f32_array(self) -> Self::F32Array {
        [self[X].raw() as f32, self[Y].raw() as f32]
    }
}
impl NdVecConvertExt for FVec3D {
    type F32Array = [f32; 3];

    fn to_cgmath_point3(self) -> cgmath::Point3<f32> {
        let [x, y, z] = self.to_f32_array();
        cgmath::point3(x, y, z)
    }
    fn to_f32_array(self) -> Self::F32Array {
        [
            self[X].raw() as f32,
            self[Y].raw() as f32,
            self[Z].raw() as f32,
        ]
    }
}
