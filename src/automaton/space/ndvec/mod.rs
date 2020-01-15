use num::{BigInt, Num};
use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::*;

mod aliases;
mod axis;
mod dim;
// mod ops_scalar;
// mod ops_vector;

pub use aliases::*;
pub use axis::*;
pub use dim::*;

pub trait NdVec<N: Num, D: Dim>: Sized + Default + PartialEq + AsRef<[N]> + AsMut<[N]> {
    fn origin() -> Self {
        Self::default()
    }
    fn is_zero(&self) -> bool {
        *self == Self::origin()
    }
}

macro_rules! MakeNdVecType {
    ($vec_type_name: ident, $coord_type: ty, $dim_array_type_name: ident) => {
        #[derive(Debug, Default, Clone, Eq, PartialEq, Hash)]
        pub struct $vec_type_name<D: Dim>(D::$dim_array_type_name);

        impl<D: Dim> NdVec<$coord_type, D> for $vec_type_name<D> {}

        impl<D: Dim> Index<Axis> for $vec_type_name<D> {
            type Output = $coord_type;
            fn index(&self, axis: Axis) -> &$coord_type {
                &self.as_ref()[axis as usize]
            }
        }
        impl<D: Dim> IndexMut<Axis> for $vec_type_name<D> {
            fn index_mut(&mut self, axis: Axis) -> &mut $coord_type {
                &mut self.as_mut()[axis as usize]
            }
        }

        impl<D: Dim> AsRef<[$coord_type]> for $vec_type_name<D> {
            fn as_ref(&self) -> &[$coord_type] {
                self.0.as_ref()
            }
        }
        impl<D: Dim> AsMut<[$coord_type]> for $vec_type_name<D> {
            fn as_mut(&mut self) -> &mut [$coord_type] {
                self.0.as_mut()
            }
        }
    };
}

MakeNdVecType!(BigVec, BigInt, BigIntArray);
MakeNdVecType!(FloatVec, f32, F32Array);
MakeNdVecType!(IVec, isize, IsizeArray);
MakeNdVecType!(ByteVec, u8, U8Array);
MakeNdVecType!(UVec, usize, UsizeArray);

// /// A set of coordinates for a given dimensionality.
// ///
// /// Unlike ndarray's NdIndex, this uses isize and so supports negative numbers.
// #[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
// pub struct NdVec<N: Num, D: Dim<N>>(D);

// impl<D: Dim> NdVec<D> {
//     /// Returns the NdVec pointing to the origin; i.e. an NdVec consisting of
//     /// all zeros.
//     pub fn origin() -> Self {
//         Self(D::origin())
//     }
//     /// Returns true if te NdVec is pointing to the origin; i.e. all components
//     /// of the NdVec are zero.
//     pub fn is_zero(self) -> bool {
//         self == Self::origin()
//     }
// }

// // Implement conversion from array.
// impl<D: Dim> From<D> for NdVec<D> {
//     fn from(dim: D) -> Self {
//         Self(dim)
//     }
// }

// // Implement indexing by usize.
// impl<D: Dim> Index<Axis> for NdVec<D> {
//     type Output = isize;
//     fn index(&self, axis: Axis) -> &isize {
//         self.0.get(axis)
//     }
// }
// impl<D: Dim> IndexMut<Axis> for NdVec<D> {
//     fn index_mut(&mut self, axis: Axis) -> &mut isize {
//         self.0.get_mut(axis)
//     }
// }

// /// Hard-coded access to X/Y.
// ///
// /// TODO replace this with easier indexing (with `use Axis::{X, Y}`)
// pub trait VecXY: Index<Axis, Output = isize> + IndexMut<Axis> {
//     /// Returns the X value of this vector.
//     fn x(&self) -> &isize {
//         // TODO maybe don't return a reference? isize is Copy.
//         &self[Axis::X]
//     }
//     /// Returns a mutable reference to the X value of this vector.
//     fn x_mut(&mut self) -> &mut isize {
//         &mut self[Axis::X]
//     }
//     /// Returns the Y value of this vector.
//     fn y(&self) -> &isize {
//         &self[Axis::Y]
//     }
//     /// Returns a mutable reference to the Y value of this vector.
//     fn y_mut(&mut self) -> &mut isize {
//         &mut self[Axis::Y]
//     }
// }
// impl VecXY for Vec2D {}

// #[cfg(test)]
// use proptest::prelude::*;

// #[cfg(test)]
// impl proptest::arbitrary::Arbitrary for Vec2D {
//     type Parameters = Option<isize>;
//     type Strategy = BoxedStrategy<Self>;
//     fn arbitrary_with(max: Option<isize>) -> Self::Strategy {
//         let max = max.unwrap_or(100);
//         prop::collection::vec(-max..=max, 2)
//             .prop_flat_map(|v| Just(NdVec([v[0], v[1]])))
//             .boxed()
//     }
// }

// #[cfg(test)]
// impl proptest::arbitrary::Arbitrary for Vec3D {
//     type Parameters = Option<isize>;
//     type Strategy = BoxedStrategy<Self>;
//     fn arbitrary_with(max: Option<isize>) -> Self::Strategy {
//         let max = max.unwrap_or(100);
//         prop::collection::vec(-max..=max, 3)
//             .prop_flat_map(|v| Just(NdVec([v[0], v[1], v[2]])))
//             .boxed()
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use proptest::proptest;

//     proptest! {
//         /// Tests various vector operations.
//         #[test]
//         fn test_ops(
//             pos1: Vec3D,
//             pos2: Vec3D,
//             scalar in -100..=100isize,
//             shift in 0..10isize,
//         ) {
//             for &ax in Dim3D::axes() {
//                 assert_eq!(-(pos1[ax]), (-pos1)[ax]);
//                 assert_eq!(pos1[ax] + pos2[ax],   (pos1 + pos2  )[ax]);
//                 assert_eq!(pos1[ax] - pos2[ax],   (pos1 - pos2  )[ax]);
//                 assert_eq!(pos1[ax] * pos2[ax],   (pos1 * pos2  )[ax]);
//                 assert_eq!(pos1[ax] + scalar,     (pos1 + scalar)[ax]);
//                 assert_eq!(pos1[ax] - scalar,     (pos1 - scalar)[ax]);
//                 assert_eq!(pos1[ax] * scalar,     (pos1 * scalar)[ax]);
//                 if scalar != 0 {
//                     assert_eq!(pos1[ax].div_euclid(scalar), (pos1.div_euclid(scalar))[ax]);
//                     assert_eq!(pos1[ax] % scalar, (pos1 % scalar)[ax]);
//                 }
//                 assert_eq!(pos1[ax] & scalar,     (pos1 & scalar)[ax]);
//                 assert_eq!(pos1[ax] | scalar,     (pos1 | scalar)[ax]);
//                 assert_eq!(pos1[ax] ^ scalar,     (pos1 ^ scalar)[ax]);
//                 assert_eq!(pos1[ax] << shift,     (pos1 << shift)[ax]);
//                 assert_eq!(pos1[ax] >> shift,     (pos1 >> shift)[ax]);
//             }
//             let mut result;
//             result = pos1; result += pos2;   assert_eq!(result, pos1 + pos2);
//             result = pos1; result -= pos2;   assert_eq!(result, pos1 - pos2);
//             result = pos1; result *= pos2;   assert_eq!(result, pos1 * pos2);
//             result = pos1; result += scalar; assert_eq!(result, pos1 + scalar);
//             result = pos1; result -= scalar; assert_eq!(result, pos1 - scalar);
//             result = pos1; result *= scalar; assert_eq!(result, pos1 * scalar);
//             if scalar != 0 {
//                 result = pos1; result /= scalar;  assert_eq!(result, pos1 / scalar);
//                 result = pos1; result %= scalar;  assert_eq!(result, pos1 % scalar);
//             }
//             result = pos1; result &= scalar; assert_eq!(result, pos1 & scalar);
//             result = pos1; result |= scalar; assert_eq!(result, pos1 | scalar);
//             result = pos1; result ^= scalar; assert_eq!(result, pos1 ^ scalar);
//             result = pos1; result <<= shift; assert_eq!(result, pos1 << shift);
//             result = pos1; result >>= shift; assert_eq!(result, pos1 >> shift);
//         }
//     }
// }
