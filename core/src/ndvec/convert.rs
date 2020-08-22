//! Conversions between different types of `NdVec`s.

use std::convert::TryInto;

use super::{BigVec, FVec, IVec, NdVec, UVec};
use crate::dim::{Dim, DimFor};
use crate::num::{r64, BigInt, FromPrimitive, NdVecNum, ToPrimitive};

impl<D1: DimFor<N>, N: NdVecNum> NdVec<D1, N> {
    /// Converts an `NdVec` between arbitrary dimensionalities, as long as those
    /// dimensionalities are the same. This function is only really useful with
    /// generic dimensionalities.
    ///
    /// This function is safe because it performs "runtime" checking that the
    /// initial and final dimensionalities are the same, even though that
    /// "runtime" checking is trivially compile-time-optimized away.
    ///
    /// # Panics
    ///
    /// This method panics if the dimensionalities do not match.
    #[inline]
    pub fn transmute<D2: DimFor<N>>(ndvec: Self) -> NdVec<D2, N> {
        assert_eq!(
            D1::Dim::NDIM,
            D2::Dim::NDIM,
            "Cannot convert NdVec<_, Dim{}D> into NdVec<_, Dim{}D>",
            D1::Dim::NDIM,
            D2::Dim::NDIM,
        );
        unsafe { *std::mem::transmute::<Box<NdVec<D1, N>>, Box<NdVec<D2, N>>>(Box::new(ndvec)) }
    }
}

impl<D: Dim> BigVec<D> {
    /// Converts this `BigVec` to an `IVec`.
    #[inline]
    pub fn to_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .to_isize()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
    /// Converts this `BigVec` to a `UVec`.
    ///
    /// # Panics
    ///
    /// This method panics if any component does not fit in `usize`.
    #[inline]
    pub fn to_uvec(&self) -> UVec<D> {
        UVec::from_fn(|ax| {
            self[ax]
                .to_usize()
                .expect("Cannot convert this IVec into a UVec")
        })
    }
    /// Converts this `BigVec` to an `FVec`.
    ///
    /// # Panics
    ///
    /// This method panics if any component does not fit in `f64`.
    #[inline]
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
    /// Converts this `FVec` to an `IVec`.
    ///
    /// # Panics
    ///
    /// This method panics if any component does not fit in `isize`.
    #[inline]
    pub fn to_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .to_isize()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
    /// Converts this `FVec` to a `BigVec`.
    #[inline]
    pub fn to_bigvec(&self) -> BigVec<D> {
        // This should not panic, because `R64` only allows finite values (which
        // all have `BigInt` representations).
        BigVec::from_fn(|ax| BigInt::from_f64(self[ax].raw()).unwrap())
    }
}

impl<D: Dim> IVec<D> {
    /// Converts this `IVec` into a `BigVec`.
    #[inline]
    pub fn to_bigvec(&self) -> BigVec<D> {
        BigVec::from_fn(|ax| self[ax].into())
    }
    /// Converts this `IVec` to a `UVec`.
    ///
    /// # Panics
    ///
    /// This method panics if any component does not fit in `usize`.
    #[inline]
    pub fn to_uvec(&self) -> UVec<D> {
        UVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this IVec into a UVec")
        })
    }
    /// Converts this `IVec` to an `FVec`.
    ///
    /// # Panics
    ///
    /// This method panics if any component does not fit in `f64`.
    #[inline]
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
    /// Converts this `UVec` to a `BigVec`.
    #[inline]
    pub fn to_bigvec(&self) -> BigVec<D> {
        BigVec::from_fn(|ax| self[ax].into())
    }
    /// Converts this `UVec` to an `IVec`.
    ///
    /// # Panics
    ///
    /// This method panics if any component does not fit in `isize`.
    #[inline]
    pub fn to_ivec(&self) -> IVec<D> {
        IVec::from_fn(|ax| {
            self[ax]
                .try_into()
                .expect("Cannot convert this UVec into an IVec")
        })
    }
}
