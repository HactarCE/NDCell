//! Exotic numeric types.
//!
//! NDCell uses four non-`std` numeric types, from three different crates:
//! - `BigInt` and `BigUint` from `num` crate
//! - `R64` from `noisy_float` crate
//! - `FixedPoint` from `fixed` module below
//!
//! This module re-exports all of them for convenience, plus some traits from
//! the `num` crate.

// TODO: Only use `num-traits` and `num-bigint` instead of whole `num`.
// Re-exporting them should make that less awkward.

pub use noisy_float::prelude::{r64, R64};
pub use num::{
    BigInt, BigUint, Float, FromPrimitive, Integer, Num, One, Signed, ToPrimitive, Zero,
};
use std::fmt;
use std::hash::Hash;
use std::ops::{AddAssign, MulAssign};

mod fixed;
mod maybebig;

pub use fixed::FixedPoint;
pub use maybebig::MaybeBigUint;

/// "Trait alias" for number types that can be used as coordinates.
pub trait NdVecNum:
    fmt::Debug + Default + Clone + Eq + Hash + Ord + Send + Num + AddAssign + MulAssign
{
    /// Minimum size for an `NdRect` using this number type as coordinates.
    /// Integer `NdRect`s have an inclusive minimum and maximium, so this
    /// function returns 1 for integral types; for floating-point types, it
    /// function returns 0.
    fn min_rect_size() -> Self;
}
impl NdVecNum for BigInt {
    fn min_rect_size() -> Self {
        Self::one()
    }
}
impl NdVecNum for FixedPoint {
    fn min_rect_size() -> Self {
        Self::zero()
    }
}
impl NdVecNum for R64 {
    fn min_rect_size() -> Self {
        Self::zero()
    }
}
impl NdVecNum for isize {
    fn min_rect_size() -> Self {
        1
    }
}
impl NdVecNum for usize {
    fn min_rect_size() -> Self {
        1
    }
}
