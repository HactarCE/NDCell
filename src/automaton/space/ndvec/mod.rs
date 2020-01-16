use num::{BigInt, FromPrimitive, Num, ToPrimitive};
use std::cmp::Eq;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::*;

mod aliases;
mod axis;
mod dim;
mod ops_scalar;
mod ops_vector;

pub use aliases::*;
pub use axis::Axis::{U, V, W, X, Y, Z};
pub use axis::*;
pub use dim::*;

/// A set of coordinates for a given dimensionality.
pub trait NdVec<N: Num, D: Dim>:
    Sized + Default + PartialEq + Index<Axis, Output = N> + IndexMut<Axis>
{
    fn origin() -> Self {
        Self::default()
    }
    fn is_zero(&self) -> bool {
        *self == Self::origin()
    }
    fn map_fn<F: Fn(Axis, &mut N)>(&mut self, mutator: F) {
        for &ax in D::axes() {
            mutator(ax, &mut self[ax]);
        }
    }
    fn from_fn<F: Fn(Axis) -> N>(generator: F) -> Self {
        let mut ret: Self = Self::default();
        for &ax in D::axes() {
            ret[ax] = generator(ax);
        }
        ret
    }
}

macro_rules! make_ndvec_struct {
    ($vec_type_name: ident, $coord_type: ty, $dim_array_type_name: ident) => {
        /// A vector type using $coord_type coordinates.
        #[derive(Debug, Default, Clone, Eq, PartialEq, Hash)]
        pub struct $vec_type_name<D: Dim>(D::$dim_array_type_name);
        impl<D: Dim> NdVec<$coord_type, D> for $vec_type_name<D> {}

        impl<D: Dim> Index<Axis> for $vec_type_name<D> {
            type Output = $coord_type;
            fn index(&self, axis: Axis) -> &$coord_type {
                &self.0.as_ref()[axis as usize]
            }
        }
        impl<D: Dim> IndexMut<Axis> for $vec_type_name<D> {
            fn index_mut(&mut self, axis: Axis) -> &mut $coord_type {
                &mut self.0.as_mut()[axis as usize]
            }
        }
    };
}

make_ndvec_struct!(BigVec, BigInt, BigIntArray);
make_ndvec_struct!(FVec, f32, F32Array);
make_ndvec_struct!(IVec, isize, IsizeArray);
make_ndvec_struct!(ByteVec, u8, U8Array);
make_ndvec_struct!(UVec, usize, UsizeArray);

impl<D: Dim> Copy for FVec<D> {}
impl<D: Dim> Copy for IVec<D> {}
impl<D: Dim> Copy for ByteVec<D> {}
impl<D: Dim> Copy for UVec<D> {}

macro_rules! impl_ndvec_conversion {
    ($ndvec1:ident from $ndvec2:ident with $converter:expr) => {
        impl<D: Dim> From<$ndvec2<D>> for $ndvec1<D> {
            fn from(vec2: $ndvec2<D>) -> $ndvec1<D> {
                let mut ret = $ndvec1::default();
                for &ax in D::axes() {
                    ret[ax] = ($converter)(vec2[ax]);
                }
                ret
            }
        }
    };
    ($ndvec1:ident from $ndvec2:ident) => {
        impl_ndvec_conversion!($ndvec1 from $ndvec2 with std::convert::Into::into);
    };
    ($ndvec1:ident try_from $ndvec2:ident with $converter:expr) => {
        impl<D: Dim> TryFrom<&$ndvec2<D>> for $ndvec1<D> {
            type Error = ();
            fn try_from(vec2: &$ndvec2<D>) -> Result<$ndvec1<D>, ()> {
                let mut ret = $ndvec1::default();
                for &ax in D::axes() {
                    ret[ax] = ($converter)(&vec2[ax]).ok_or(())?
                }
                Ok(ret)
            }
        }
    };
}

// Convert to BigVec.
impl_ndvec_conversion!(BigVec try_from FVec with (|&x| FromPrimitive::from_f32(x)));
impl_ndvec_conversion!(BigVec from IVec);
impl_ndvec_conversion!(BigVec from ByteVec);
impl_ndvec_conversion!(BigVec from UVec);

// Convert to IVec.
impl_ndvec_conversion!(IVec try_from BigVec with ToPrimitive::to_isize);
impl_ndvec_conversion!(IVec try_from FVec with ToPrimitive::to_isize);
impl_ndvec_conversion!(IVec from ByteVec);
impl_ndvec_conversion!(IVec try_from UVec with ToPrimitive::to_isize);

// Convert to UVec.
impl_ndvec_conversion!(UVec try_from BigVec with ToPrimitive::to_usize);
impl_ndvec_conversion!(UVec try_from FVec with ToPrimitive::to_usize);
impl_ndvec_conversion!(UVec try_from IVec with ToPrimitive::to_usize);
impl_ndvec_conversion!(UVec from ByteVec);

// Convert to FVec.
impl_ndvec_conversion!(FVec try_from BigVec with ToPrimitive::to_f32);
impl_ndvec_conversion!(FVec from IVec with (|x| x as f32));
impl_ndvec_conversion!(FVec from ByteVec);
impl_ndvec_conversion!(FVec from UVec with (|x| x as f32));

/// BigVec constructor using array notation; e.g. bigvec![1, -2, 3] or
/// bigvec![-10; 2].
#[macro_export]
macro_rules! bigvec {
    ($($t:tt)*) => {
        BigVec([$($t)*])
    };
}
///  IVec constructor using array notation; e.g. ivec![1, -2, 3] or ivec![-10;
/// 2].
#[macro_export]
macro_rules! ivec {
    ($($t:tt)*) => {
        IVec([$($t)*])
    };
}
///  FVec constructor using array notation; e.g. fvec![1.1, -2.3, 3.0] or
/// fvec![-9.8; 2].
#[macro_export]
macro_rules! fvec {
    ($($t:tt)*) => {
        FVec([$($t)*])
    };
}
/// ByteVec constructor using array notation; e.g. bytevec![1, 2, 255] or
/// bytevec![30; 2].
#[macro_export]
macro_rules! bytevec {
    ($($t:tt)*) => {
        ByteVec([$($t)*])
    };
}
/// UVec constructor using array notation; e.g. uvec![1, 2, 1000] or uvec![30;
/// 2].
#[macro_export]
macro_rules! uvec {
    ($($t:tt)*) => {
        UVec([$($t)*])
    };
}

#[cfg(test)]
mod tests;
