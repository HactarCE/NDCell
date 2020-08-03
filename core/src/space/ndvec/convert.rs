//! Methods for converting between different types of NdVecs.

use noisy_float::prelude::r64;
use num::{FromPrimitive, ToPrimitive};
use std::convert::TryInto;

use super::*;

impl<D: DimFor<N>, N: NdVecNum> NdVec<D, N> {
    /// Converts an NdVec from one number type to another using
    /// std::convert::Into.
    pub fn convert<N2: NdVecNum>(&self) -> NdVec<D, N2>
    where
        D: DimFor<N2>,
        N: Into<N2>,
    {
        NdVec::from_fn(|ax| N2::from(self[ax].clone().into()))
    }
}

impl<D: Dim> BigVec<D> {
    /// Converts this BigVec to an IVec.
    pub fn to_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .to_isize()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
    /// Converts this BigVec to an FVec, panicking if it does not fit.
    pub fn to_fvec(&self) -> FVec<D> {
        FVec::from_fn(|ax| {
            self[ax]
                .to_f64()
                .map(r64)
                .expect("Cannot convert this UVec into an IVec")
        })
    }
}

impl<D: Dim> FVec<D> {
    /// Converts this FVec to an IVec, panicking if it does not fit.
    pub fn to_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .to_isize()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
    /// Converts this FVec to a BigVec, panicking if it does not fit.
    pub fn to_bigvec(&self) -> BigVec<D> {
        BigVec::from_fn(|ax| BigInt::from_f64(self[ax].raw()).unwrap())
    }
}

impl<D: Dim> IVec<D> {
    /// Converts this IVec to a UVec, panicking if it does not fit.
    pub fn to_uvec(&self) -> UVec<D> {
        UVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this IVec into a UVec")
        })
    }
    /// Converts this IVec to an FVec, panicking if it does not fit.
    pub fn to_fvec(&self) -> FVec<D> {
        FVec::from_fn(|ax| {
            self[ax]
                .to_f64()
                .map(r64)
                .expect("Cannot convert this UVec into an IVec")
        })
    }
}

impl<D: Dim> UVec<D> {
    /// Converts this UVec to an IVec, panicking if it does not fit.
    pub fn to_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
}
