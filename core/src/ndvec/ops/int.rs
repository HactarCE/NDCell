//! Operations between an `NdVec` and a scalar.

use std::ops::*;

use super::*;

impl_multi_ndvec_ops!(impl + for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl - for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl * for BigInt, FixedPoint, isize, usize);

impl_multi_ndvec_ops!(impl & for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl | for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl ^ for BigInt, FixedPoint, isize, usize);

impl_multi_ndvec_ops!(impl << usize for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl << isize for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl << u32 for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl << i32 for BigInt, FixedPoint, isize, usize);

impl_multi_ndvec_ops!(impl >> usize for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl >> isize for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl >> u32 for BigInt, FixedPoint, isize, usize);
impl_multi_ndvec_ops!(impl >> i32 for BigInt, FixedPoint, isize, usize);

impl_multi_ndvec_ops!(impl + f64 for FixedPoint);
impl_multi_ndvec_ops!(impl - f64 for FixedPoint);
impl_multi_ndvec_ops!(impl * f64 for FixedPoint);
