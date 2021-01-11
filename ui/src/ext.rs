//! Extension traits to monkeypatch functionality on types from external crates.

use cgmath::EuclideanSpace;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

pub trait FVecConvertExt: Sized {
    type F32Array;

    fn to_cgmath_point3(self) -> cgmath::Point3<f32>;
    fn to_cgmath_vec3(self) -> cgmath::Vector3<f32> {
        self.to_cgmath_point3().to_vec()
    }
    fn to_f32_array(self) -> Self::F32Array;
}
impl FVecConvertExt for FVec1D {
    type F32Array = f32;

    fn to_cgmath_point3(self) -> cgmath::Point3<f32> {
        let x = self.to_f32_array();
        cgmath::point3(x, 0.0, 0.0)
    }
    fn to_f32_array(self) -> Self::F32Array {
        self[X].raw() as f32
    }
}
impl FVecConvertExt for FVec2D {
    type F32Array = [f32; 2];

    fn to_cgmath_point3(self) -> cgmath::Point3<f32> {
        let [x, y] = self.to_f32_array();
        cgmath::point3(x, y, 0.0)
    }
    fn to_f32_array(self) -> Self::F32Array {
        [self[X].raw() as f32, self[Y].raw() as f32]
    }
}
impl FVecConvertExt for FVec3D {
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

pub trait IVecConvertExt {
    type I32Array;

    fn to_i32_array(self) -> Self::I32Array;
}
impl IVecConvertExt for IVec2D {
    type I32Array = [i32; 2];

    fn to_i32_array(self) -> Self::I32Array {
        [self[X] as i32, self[Y] as i32]
    }
}
impl IVecConvertExt for IVec3D {
    type I32Array = [i32; 3];

    fn to_i32_array(self) -> Self::I32Array {
        [self[X] as i32, self[Y] as i32, self[Z] as i32]
    }
}
