//! N-dimensional vectors.
//!
//! Until generic associated types work (see rust-lang#44265), this module and
//! ndrect are kind of a mess. There's a ton of macros in order to implement all
//! the various operations on NdVecs without writing thousands of lines of
//! repetitive code.
//!
//! Note that we use noisy_float's R64 type here instead of f64 so that we don't
//! have to deal with infinities and NaN, which really should NEVER show up in
//! NdVecs.

use noisy_float::types::R64;
use num::{BigInt, Num};
use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::*;

mod aliases;
mod axis;
mod dim;
mod ops;

pub use aliases::*;
pub use axis::Axis::{U, V, W, X, Y, Z};
pub use axis::*;
pub use dim::*;

/// A "type alias" for types that can be used as coordinates in an NdVec.
pub trait NdVecNum: Num + Default + Clone + Eq + Ord {}
impl<T> NdVecNum for T where T: Num + Default + Clone + Eq + Ord {}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
/// A set of coordinates for a given dimensionality.
pub struct NdVec<D: DimFor<N>, N: NdVecNum>(D::Array);

// Implement Copy when coordinate type is Copy.
//
// Unfortunately, for a number of subtle reasons, this only works when the
// dimensionality is known, not when it is a type parameter. This is still
// useful enough that it's worth including.
impl<D: DimFor<N>, N: NdVecNum + Copy> Copy for NdVec<D, N> where D::Array: Copy {}

// Implement indexing using Axis.
impl<D: DimFor<N>, N: NdVecNum> Index<Axis> for NdVec<D, N> {
    type Output = N;
    fn index(&self, axis: Axis) -> &N {
        &self.0.as_ref()[axis as usize]
    }
}
impl<D: DimFor<N>, N: NdVecNum> IndexMut<Axis> for NdVec<D, N> {
    fn index_mut(&mut self, axis: Axis) -> &mut N {
        &mut self.0.as_mut()[axis as usize]
    }
}

impl<D: DimFor<N>, N: NdVecNum> NdVec<D, N> {
    pub fn origin() -> Self {
        Self::default()
    }
    pub fn is_zero(&self) -> bool {
        *self == Self::default()
    }

    fn from_fn<F: Fn(Axis) -> N>(generator: F) -> Self {
        let mut ret: Self = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = generator(ax);
        }
        ret
    }
    pub fn map_fn<F: Fn(Axis, &mut N)>(&mut self, f: F) {
        for &ax in D::Dim::axes() {
            f(ax, &mut self[ax]);
        }
    }

    pub fn min(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::min(&v1[ax], &v2[ax]).clone();
        }
        ret
    }
    pub fn max(v1: &Self, v2: &Self) -> Self {
        let mut ret = Self::default();
        for &ax in D::Dim::axes() {
            ret[ax] = std::cmp::max(&v1[ax], &v2[ax]).clone();
        }
        ret
    }
}

// make_ndvec_struct!(BigVec, BigInt, BigIntArray);
// make_ndvec_struct!(FVec, R64, R64Array, true);
// make_ndvec_struct!(IVec, isize, IsizeArray);
// make_ndvec_struct!(UVec, usize, UsizeArray);
// make_ndvec_struct!(ByteVec, u8, U8Array);

// impl<D: Dim> Copy for FVec<D> {}
// impl<D: Dim> Copy for IVec<D> {}
// impl<D: Dim> Copy for UVec<D> {}
// impl<D: Dim> Copy for ByteVec<D> {}

// macro_rules! impl_ndvec_conversion {
//     ($ndvec1:ident from $ndvec2:ident with $converter:expr) => {
//         impl<D: Dim> From<$ndvec2<D>> for $ndvec1<D> {
//             fn from(vec2: $ndvec2<D>) -> $ndvec1<D> {
//                 let mut ret = $ndvec1::default();
//                 for &ax in D::axes() {
//                     ret[ax] = ($converter)(vec2[ax]);
//                 }
//                 ret
//             }
//         }
//     };
//     ($ndvec1:ident from $ndvec2:ident) => {
//         impl_ndvec_conversion!($ndvec1 from $ndvec2 with std::convert::Into::into);
//     };
//     ($ndvec1:ident try_from $ndvec2:ident with $converter:expr) => {
//         impl<D: Dim> TryFrom<&$ndvec2<D>> for $ndvec1<D> {
//             type Error = ();
//             fn try_from(vec2: &$ndvec2<D>) -> Result<$ndvec1<D>, ()> {
//                 let mut ret = $ndvec1::default();
//                 for &ax in D::axes() {
//                     ret[ax] = ($converter)(&vec2[ax]).ok_or(())?
//                 }
//                 Ok(ret)
//             }
//         }
//     };
// }

// // Convert to BigVec.
// impl_ndvec_conversion!(BigVec try_from FVec with (|&x| FromPrimitive::from_f32(x)));
// impl_ndvec_conversion!(BigVec from IVec);
// impl_ndvec_conversion!(BigVec from ByteVec);
// impl_ndvec_conversion!(BigVec from UVec);

// // Convert to FVec.
// impl_ndvec_conversion!(FVec try_from BigVec with ToPrimitive::to_f32);
// impl_ndvec_conversion!(FVec from IVec with (|x| x as f32));
// impl_ndvec_conversion!(FVec from UVec with (|x| x as f32));
// impl_ndvec_conversion!(FVec from ByteVec);

// // Convert to IVec.
// impl_ndvec_conversion!(IVec try_from BigVec with ToPrimitive::to_isize);
// impl_ndvec_conversion!(IVec try_from FVec with ToPrimitive::to_isize);
// impl_ndvec_conversion!(IVec from ByteVec);
// impl_ndvec_conversion!(IVec try_from UVec with ToPrimitive::to_isize);

// // Convert to UVec.
// impl_ndvec_conversion!(UVec try_from BigVec with ToPrimitive::to_usize);
// impl_ndvec_conversion!(UVec try_from FVec with ToPrimitive::to_usize);
// impl_ndvec_conversion!(UVec try_from IVec with ToPrimitive::to_usize);
// impl_ndvec_conversion!(UVec from ByteVec);

// /// BigVec constructor using array notation; e.g. bigvec![1, -2, 3] or
// /// bigvec![-10; 2].
// #[macro_export]
// macro_rules! bigvec {
//     ($($t:tt)*) => {
//         BigVec([$($t)*])
//     };
// }
// ///  IVec constructor using array notation; e.g. ivec![1, -2, 3] or ivec![-10;
// /// 2].
// #[macro_export]
// macro_rules! ivec {
//     ($($t:tt)*) => {
//         IVec([$($t)*])
//     };
// }
// ///  FVec constructor using array notation; e.g. fvec![1.1, -2.3, 3.0] or
// /// fvec![-9.8; 2].
// #[macro_export]
// macro_rules! fvec {
//     ($($t:tt)*) => {
//         FVec([$($t)*])
//     };
// }
// /// ByteVec constructor using array notation; e.g. bytevec![1, 2, 255] or
// /// bytevec![30; 2].
// #[macro_export]
// macro_rules! bytevec {
//     ($($t:tt)*) => {
//         ByteVec([$($t)*])
//     };
// }
// /// UVec constructor using array notation; e.g. uvec![1, 2, 1000] or uvec![30;
// /// 2].
// #[macro_export]
// macro_rules! uvec {
//     ($($t:tt)*) => {
//         UVec([$($t)*])
//     };
// }

#[macro_export]
macro_rules! ndvec {
    ($($t:tt)*) => {NdVec([$($t)*])};
}

#[cfg(test)]
mod tests;
