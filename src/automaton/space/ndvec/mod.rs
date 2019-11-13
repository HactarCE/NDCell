use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Index, IndexMut};

mod axis;
mod dim;
mod ops_scalar;
mod ops_vector;

pub use axis::*;
pub use dim::*;

/// A set of coordinates for a given dimensionality.
///
/// Unlike ndarray's NdIndex, this uses isize and so supports negative numbers.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NdVec<D: Dim>(D);

/// A 1D NdVec.
pub type Vec1D = NdVec<[isize; 1]>;
/// A 2D NdVec.
pub type Vec2D = NdVec<[isize; 2]>;
/// A 3D NdVec.
pub type Vec3D = NdVec<[isize; 3]>;
/// A 4D NdVec.
pub type Vec4D = NdVec<[isize; 4]>;
/// A 5D NdVec.
pub type Vec5D = NdVec<[isize; 5]>;
/// A 6D NdVec.
pub type Vec6D = NdVec<[isize; 6]>;

// Implement generic vector operations.
impl<D: Dim> NdVec<D> {
    const NDIM: usize = D::NDIM;

    fn origin() -> Self {
        Self(D::origin())
    }
}

// Implement indexing by usize.
impl<D: Dim> Index<Axis> for NdVec<D> {
    type Output = isize;
    fn index(&self, axis: Axis) -> &isize {
        self.0.get(axis)
    }
}
impl<D: Dim> IndexMut<Axis> for NdVec<D> {
    fn index_mut(&mut self, axis: Axis) -> &mut isize {
        self.0.get_mut(axis)
    }
}

#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
pub fn coords_strategy<R: Strategy<Value = isize>>(
    value_strategy: R,
) -> impl Strategy<Value = Vec3D> {
    prop::collection::vec(value_strategy, 3)
        .prop_flat_map(|vec| Just(NdVec([vec[0], vec[1], vec[2]])))
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::proptest;

    proptest! {
        #[test]
        fn test_ops(
            pos1 in coords_strategy(-100..=100isize),
            pos2 in coords_strategy(-100..=50isize),
            scalar in -100..=100isize,
            shift in 0..10isize,
        ) {
            for axis in Dim3D::axes() {
                assert_eq!(-(pos1[axis]), (-pos1)[axis]);
                assert_eq!(pos1[axis] + pos2[axis], (pos1 + pos2  )[axis]);
                assert_eq!(pos1[axis] - pos2[axis], (pos1 - pos2  )[axis]);
                assert_eq!(pos1[axis] + scalar,     (pos1 + scalar)[axis]);
                assert_eq!(pos1[axis] - scalar,     (pos1 - scalar)[axis]);
                assert_eq!(pos1[axis] * scalar,     (pos1 * scalar)[axis]);
                if scalar != 0 {
                    assert_eq!(pos1[axis] / scalar, (pos1 / scalar)[axis]);
                    assert_eq!(pos1[axis] % scalar, (pos1 % scalar)[axis]);
                }
                assert_eq!(pos1[axis] & scalar,     (pos1 & scalar)[axis]);
                assert_eq!(pos1[axis] | scalar,     (pos1 | scalar)[axis]);
                assert_eq!(pos1[axis] ^ scalar,     (pos1 ^ scalar)[axis]);
                assert_eq!(pos1[axis] << shift,     (pos1 << shift)[axis]);
                assert_eq!(pos1[axis] >> shift,     (pos1 >> shift)[axis]);
            }
            let mut result = pos1; result += pos2;   assert_eq!(result, pos1 + pos2);
            let mut result = pos1; result -= pos2;   assert_eq!(result, pos1 - pos2);
            let mut result = pos1; result += scalar; assert_eq!(result, pos1 + scalar);
            let mut result = pos1; result -= scalar; assert_eq!(result, pos1 - scalar);
            let mut result = pos1; result *= scalar; assert_eq!(result, pos1 * scalar);
            if scalar != 0 {
                let mut result = pos1; result /= scalar;  assert_eq!(result, pos1 / scalar);
                let mut result = pos1; result %= scalar;  assert_eq!(result, pos1 % scalar);
            }
            let mut result = pos1; result &= scalar; assert_eq!(result, pos1 & scalar);
            let mut result = pos1; result |= scalar; assert_eq!(result, pos1 | scalar);
            let mut result = pos1; result ^= scalar; assert_eq!(result, pos1 ^ scalar);
            let mut result = pos1; result <<= shift; assert_eq!(result, pos1 << shift);
            let mut result = pos1; result >>= shift; assert_eq!(result, pos1 >> shift);
        }
    }
}
