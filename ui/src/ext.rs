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

pub trait GroupSliceByExt<'a, T> {
    fn group_slice_by<K: PartialEq, F: FnMut(&'a T) -> K>(
        &'a self,
        key: F,
    ) -> SliceGroupIter<'a, T, F>;
}
impl<'a, T> GroupSliceByExt<'a, T> for [T] {
    fn group_slice_by<K: PartialEq, F: FnMut(&'a T) -> K>(
        &'a self,
        key: F,
    ) -> SliceGroupIter<'a, T, F> {
        let remaining = self;
        SliceGroupIter { remaining, key }
    }
}
pub struct SliceGroupIter<'a, T, F> {
    remaining: &'a [T],
    key: F,
}
impl<'a, T, K: PartialEq, F: FnMut(&'a T) -> K> Iterator for SliceGroupIter<'a, T, F> {
    type Item = (K, &'a [T]);

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining.is_empty() {
            return None;
        }
        let current_key = (self.key)(&self.remaining[0]);
        let i = self
            .remaining
            .iter()
            .take_while(|t| (self.key)(t) == current_key)
            .count();
        let (current_slice, remaining) = self.remaining.split_at(i);
        self.remaining = remaining;
        Some((current_key, current_slice))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_group_slice_by() {
        let xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        let mut groups = xs.group_slice_by(|x| x / 5);
        assert_eq!(groups.next(), Some((0, &xs[0..5])));
        assert_eq!(groups.next(), Some((1, &xs[5..10])));
        assert_eq!(groups.next(), Some((2, &xs[10..15])));
        assert_eq!(groups.next(), Some((3, &xs[15..])));
        assert_eq!(groups.next(), None);
        assert_eq!(groups.next(), None);
    }
}
