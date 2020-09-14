//! Fixed-point arithmetic using `BigInt`.
//!
//! None of the existing fixed-point libraries for Rust allowed
//! arbitrarily-large integral components, so I made my own.
//!
//! If you want to turn this into its own crate, be my guest! I have no interest
//! in implementing all the things that would be expected if this were its own
//! crate.

use noisy_float::prelude::R64;
use num::{BigInt, BigUint, FromPrimitive, Num, One, Signed, ToPrimitive, Zero};
use std::convert::TryFrom;
use std::fmt;
use std::ops::*;

#[macro_use]
mod macros;

/// Number of fractional bits.
const FRACTIONAL_BITS: usize = 32;
/// Lowest integral bit.
const INTEGRAL_ONE: u64 = 1 << FRACTIONAL_BITS as u64;
/// Mask of fractional bits.
const FRACTIONAL_MASK: u64 = INTEGRAL_ONE - 1;

/// Fixed-point number with an arbitrarily large integer component and a 32-bit
/// fractional component.
#[derive(Default, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FixedPoint(BigInt);

impl FixedPoint {
    /// Returns the integer part of the number.
    #[inline]
    pub fn trunc(&self) -> BigInt {
        let ret = self.0.abs() >> FRACTIONAL_BITS;
        if self.is_negative() {
            -ret
        } else {
            ret
        }
    }

    /// Returns the fractional part of the number.
    #[inline]
    pub fn fract(&self) -> f64 {
        let mask = BigInt::from(FRACTIONAL_MASK);
        let ret = (self.0.abs() & mask).to_u64().unwrap() as f64 / INTEGRAL_ONE as f64;
        assert!(0.0 <= ret && ret < 1.0);
        match self.0.sign() {
            num::bigint::Sign::Minus => -ret,
            num::bigint::Sign::NoSign => 0.0,
            num::bigint::Sign::Plus => ret,
        }
    }

    /// Returns the largest integer less than or equal to the number, along with
    /// the signed distance to that integer.
    #[inline]
    pub fn floor(&self) -> (BigInt, f64) {
        let mut int = self.trunc();
        let mut fract = self.fract();
        if fract < 0.0 {
            int -= 1;
            fract += 1.0;
        }
        (int, fract)
    }

    /// Returns the smallest integer greater than or equal to the number, along
    /// with the signed distance to that integer.
    #[inline]
    pub fn ceil(&self) -> (BigInt, f64) {
        let mut int = self.trunc();
        let mut fract = self.fract();
        if fract > 0.0 {
            int += 1;
            fract -= 1.0;
        }
        (int, fract)
    }

    /// Returns the smallest integer greater than or equal to the number, along
    /// with the signed distance to that integer.
    #[inline]
    pub fn round(&self) -> (BigInt, f64) {
        let (mut int, mut fract) = self.floor();
        if fract >= 0.5 {
            int += 1;
            fract -= 1.0;
        }
        (int, fract)
    }

    /// Returns the square root.
    #[inline]
    pub fn sqrt(&self) -> Self {
        // If the "real" number is `n`, then we are storing `n * INTEGRAL_ONE`
        // and we want to return `sqrt(n) * INTEGRAL_ONE`. `(sqrt(n) *
        // INTEGRAL_ONE)^2 = n * INTEGRAL_ONE^2`, so if we multiply by
        // `INTEGRAL_ONE` and then use integer square root we will get the
        // correct answer. Multiplying by `INTEGRAL_ONE` is equivalent to
        // bitshifting left by `FRACTIONAL_BITS`.
        Self((&self.0 << FRACTIONAL_BITS).sqrt())
    }
}

impl From<BigInt> for FixedPoint {
    #[inline]
    fn from(i: BigInt) -> Self {
        Self(i << FRACTIONAL_BITS)
    }
}

impl TryFrom<(BigInt, f64)> for FixedPoint {
    type Error = ();

    #[inline]
    fn try_from((mut i, f): (BigInt, f64)) -> Result<Self, ()> {
        i += BigInt::from_f64(f.trunc()).unwrap();
        let i_shifted = i << FRACTIONAL_BITS;
        let f_shifted = BigInt::from_f64(f.fract().abs() * INTEGRAL_ONE as f64).ok_or(())?;
        if f.is_sign_negative() {
            Ok(Self(i_shifted - f_shifted))
        } else {
            Ok(Self(i_shifted + f_shifted))
        }
    }
}
impl From<(BigInt, R64)> for FixedPoint {
    #[inline]
    fn from((i, f): (BigInt, R64)) -> Self {
        Self::try_from((i, f.raw())).unwrap()
    }
}

impl TryFrom<f64> for FixedPoint {
    type Error = ();

    #[inline]
    fn try_from(f: f64) -> Result<Self, ()> {
        Self::try_from((BigInt::zero(), f))
    }
}
impl From<R64> for FixedPoint {
    fn from(f: R64) -> Self {
        Self::try_from(f.raw()).unwrap()
    }
}

impl FromPrimitive for FixedPoint {
    #[inline]
    fn from_i64(n: i64) -> Option<Self> {
        Some(Self::from(BigInt::from(n)))
    }
    #[inline]
    fn from_u64(n: u64) -> Option<Self> {
        Some(Self::from(BigInt::from(n)))
    }
    #[inline]
    fn from_f64(n: f64) -> Option<Self> {
        Self::try_from(n).ok()
    }
}

impl ToPrimitive for FixedPoint {
    #[inline]
    fn to_i64(&self) -> Option<i64> {
        self.trunc().to_i64()
    }
    #[inline]
    fn to_u64(&self) -> Option<u64> {
        self.trunc().to_u64()
    }
    #[inline]
    fn to_f64(&self) -> Option<f64> {
        Some(self.trunc().to_f64()? + self.fract())
    }
}

impl fmt::Debug for FixedPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FixedPoint(")?;
        fmt::Debug::fmt(&self.0, f)?;
        write!(f, ")")?;
        Ok(())
    }
}
impl fmt::Display for FixedPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let int = self.trunc();
        if int.magnitude() >= &BigUint::from(10_usize) {
            fmt::Display::fmt(&(&int / 10_usize), f)?;
        }
        let lowest_digit: BigUint = int.magnitude() % 10_usize;
        fmt::Display::fmt(&(lowest_digit.to_f64().unwrap() + self.fract().abs()), f)?;
        Ok(())
    }
}
impl Num for FixedPoint {
    type FromStrRadixErr = ParseFixedPointError;
    fn from_str_radix(s: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        if let Some(dot) = s.find('.') {
            let int = if dot == 0 {
                BigInt::zero()
            } else {
                BigInt::from_str_radix(&s[..dot], radix)?
            };
            let fract = f64::from_str_radix(&s[dot..], radix)?;
            Ok(Self::try_from((int, fract)).unwrap())
        } else {
            let int = BigInt::from_str_radix(s, radix)?;
            Ok(Self::from(int))
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug)]
pub enum ParseFixedPointError {
    BigIntError(num::bigint::ParseBigIntError),
    FloatError(num::traits::ParseFloatError),
}
impl From<num::bigint::ParseBigIntError> for ParseFixedPointError {
    fn from(e: num::bigint::ParseBigIntError) -> Self {
        Self::BigIntError(e)
    }
}
impl From<num::traits::ParseFloatError> for ParseFixedPointError {
    fn from(e: num::traits::ParseFloatError) -> Self {
        Self::FloatError(e)
    }
}

impl One for FixedPoint {
    #[inline]
    fn one() -> Self {
        FixedPoint::from(BigInt::one())
    }
}
impl Zero for FixedPoint {
    #[inline]
    fn zero() -> Self {
        FixedPoint::from(BigInt::zero())
    }
    #[inline]
    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl Signed for FixedPoint {
    #[inline]
    fn abs(&self) -> Self {
        Self(self.0.abs())
    }
    #[inline]
    fn abs_sub(&self, other: &Self) -> Self {
        Self(self.0.abs_sub(&other.0))
    }
    #[inline]
    fn signum(&self) -> Self {
        Self::from(self.0.signum())
    }
    #[inline]
    fn is_positive(&self) -> bool {
        self.0.is_positive()
    }
    #[inline]
    fn is_negative(&self) -> bool {
        self.0.is_negative()
    }
}

macro_rules! forward_fixedpoint_ops {
    ($num:ty) => {
        forward_binop!(impl Add<$num> for FixedPoint, add);
        forward_binop!(impl Sub<$num> for FixedPoint, sub);
        forward_binop!(impl Mul<$num> for FixedPoint, mul, >> FRACTIONAL_BITS);
        forward_binop!(impl Div<$num> for FixedPoint (<< FRACTIONAL_BITS), div);
        forward_binop!(impl Rem<$num> for FixedPoint, rem);

        forward_assign!(impl AddAssign<$num> for FixedPoint, add_assign);
        forward_assign!(impl SubAssign<$num> for FixedPoint, sub_assign);
        forward_assign!(impl MulAssign<$num> for FixedPoint, mul_assign, >>= FRACTIONAL_BITS);
        forward_assign!(impl DivAssign<$num> for FixedPoint (<<= FRACTIONAL_BITS), div_assign);
        forward_assign!(impl RemAssign<$num> for FixedPoint, rem_assign);
    };
}

forward_fixedpoint_ops!(FixedPoint);
forward_fixedpoint_ops!(BigInt);
forward_fixedpoint_ops!(f64);

forward_binop!(impl BitAnd<FixedPoint> for FixedPoint, bitand);
forward_binop!(impl BitOr<FixedPoint> for FixedPoint, bitor);
forward_binop!(impl BitXor<FixedPoint> for FixedPoint, bitxor);

forward_assign!(impl BitAndAssign<FixedPoint> for FixedPoint, bitand_assign);
forward_assign!(impl BitOrAssign<FixedPoint> for FixedPoint, bitor_assign);
forward_assign!(impl BitXorAssign<FixedPoint> for FixedPoint, bitxor_assign);

impl Neg for FixedPoint {
    type Output = FixedPoint;

    #[inline]
    fn neg(self) -> FixedPoint {
        FixedPoint(-self.0)
    }
}
impl Neg for &FixedPoint {
    type Output = FixedPoint;

    #[inline]
    fn neg(self) -> FixedPoint {
        FixedPoint(-&self.0)
    }
}

impl<X> Shl<X> for FixedPoint
where
    BigInt: Shl<X, Output = BigInt>,
{
    type Output = Self;

    #[inline]
    fn shl(self, rhs: X) -> Self::Output {
        Self(self.0 << rhs)
    }
}
impl<X> Shr<X> for FixedPoint
where
    BigInt: Shr<X, Output = BigInt>,
{
    type Output = Self;

    #[inline]
    fn shr(self, rhs: X) -> Self::Output {
        Self(self.0 >> rhs)
    }
}
impl<X> ShlAssign<X> for FixedPoint
where
    BigInt: ShlAssign<X>,
{
    #[inline]
    fn shl_assign(&mut self, rhs: X) {
        self.0 <<= rhs
    }
}
impl<X> ShrAssign<X> for FixedPoint
where
    BigInt: ShrAssign<X>,
{
    #[inline]
    fn shr_assign(&mut self, rhs: X) {
        self.0 >>= rhs
    }
}

impl<'a, X> Shl<X> for &'a FixedPoint
where
    &'a BigInt: Shl<X, Output = BigInt>,
{
    type Output = FixedPoint;

    #[inline]
    fn shl(self, rhs: X) -> Self::Output {
        FixedPoint(&self.0 << rhs)
    }
}
impl<'a, X> Shr<X> for &'a FixedPoint
where
    &'a BigInt: Shr<X, Output = BigInt>,
{
    type Output = FixedPoint;

    #[inline]
    fn shr(self, rhs: X) -> Self::Output {
        FixedPoint(&self.0 >> rhs)
    }
}

#[cfg(test)]
mod tests;
