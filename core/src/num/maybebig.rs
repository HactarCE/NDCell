//! Enumeration of `BigUint` and `usize`, to save space.

use num::BigUint;
use std::fmt;
use std::ops::Add;

/// Either `usize` ("small") or `BigUint` ("big").
#[derive(Debug)]
pub enum MaybeBigUint<'a> {
    /// Number small enough to fit in `usize`.
    Small(usize),
    /// Number too large, requiring `BigUint`.
    Big(&'a BigUint),
}
impl fmt::Display for MaybeBigUint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MaybeBigUint::Small(x) => fmt::Display::fmt(x, f),
            MaybeBigUint::Big(x) => fmt::Display::fmt(x, f),
        }
    }
}
impl From<usize> for MaybeBigUint<'_> {
    fn from(x: usize) -> Self {
        Self::Small(x)
    }
}
impl<'a> From<&'a BigUint> for MaybeBigUint<'a> {
    fn from(x: &'a BigUint) -> Self {
        Self::Big(x)
    }
}
impl<'a> From<MaybeBigUint<'a>> for BigUint {
    fn from(n: MaybeBigUint<'a>) -> Self {
        match n {
            MaybeBigUint::Small(x) => x.into(),
            MaybeBigUint::Big(x) => x.clone(),
        }
    }
}
impl Add for MaybeBigUint<'_> {
    type Output = Result<usize, BigUint>;

    fn add(self, other: Self) -> Result<usize, BigUint> {
        match (self, other) {
            (Self::Small(a), Self::Small(b)) => {
                a.checked_add(b).ok_or_else(|| BigUint::from(a) + b)
            }
            (Self::Small(a), Self::Big(b)) => Err(b + a),
            (Self::Big(a), Self::Small(b)) => Err(a + b),
            (Self::Big(a), Self::Big(b)) => Err(a + b),
        }
    }
}
