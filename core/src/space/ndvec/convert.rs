//! Several traits for converting between different types of NdVecs.

use noisy_float::prelude::r64;
use num::{FromPrimitive, ToPrimitive};
use std::convert::TryInto;

use super::*;

// TODO: change these traits to simple method impls and rename `as` to `to`.

/// NdVecs that can be converted to UVecs but not using From/Into.
pub trait AsUVec<D: Dim> {
    /// Converts the NdVec to a UVec, panicking if it does not fit.
    fn as_uvec(&self) -> UVec<D>;
}
/// NdVecs that can be converted to IVecs but not using From/Into.
pub trait AsIVec<D: Dim> {
    /// Converts the NdVec to an IVec, panicking if it does not fit.
    fn as_ivec(&self) -> IVec<D>;
}
/// NdVecs that can be converted to FVecs but not using From/Into.
pub trait AsFVec<D: Dim> {
    /// Converts the NdVec to an FVec, panicking if it does not fit.
    fn as_fvec(&self) -> FVec<D>;
}
/// NdVecs that can be converted to BigVecs but not using From/Into.
pub trait AsBigVec<D: Dim> {
    /// Converts the NdVec to an BigVec, panicking if it does not fit.
    fn as_bigvec(&self) -> BigVec<D>;
}

impl<D: Dim> AsUVec<D> for IVec<D> {
    fn as_uvec(&self) -> UVec<D> {
        UVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this IVec into a UVec")
        })
    }
}
impl<D: Dim> AsIVec<D> for UVec<D> {
    fn as_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
}

impl<D: Dim> AsIVec<D> for BigVec<D> {
    fn as_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .to_isize()
                .expect("Cannot convert such a large BigVec into an IVec")
        })
    }
}
impl<D: Dim> AsIVec<D> for FVec<D> {
    fn as_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| self[ax].to_isize().unwrap())
    }
}

impl<D: Dim> AsFVec<D> for BigVec<D> {
    fn as_fvec(&self) -> FVec<D> {
        FVec::from_fn(|ax| {
            self[ax]
                .to_f64()
                .map(r64)
                .expect("Cannot convert such a large BigVec into an FVec")
        })
    }
}
impl<D: Dim> AsFVec<D> for IVec<D> {
    fn as_fvec(&self) -> FVec<D> {
        FVec::from_fn(|ax| {
            self[ax]
                .to_f64()
                .map(r64)
                .expect("Cannot convert such a large BigVec into an FVec")
        })
    }
}

impl<D: Dim> AsBigVec<D> for FVec<D> {
    fn as_bigvec(&self) -> BigVec<D> {
        BigVec::from_fn(|ax| BigInt::from_f64(self[ax].raw()).unwrap())
    }
}
