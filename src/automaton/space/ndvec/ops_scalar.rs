//! Operations between an NdVec and a scalar.

use std::ops::*;

use super::*;

// Imlement addition between an NdVec and a scalar (i.e. add the scalar to each
// coordinate).
impl<D: Dim> Add<isize> for NdVec<D> {
    type Output = Self;
    fn add(mut self, other: isize) -> Self {
        self += other;
        self
    }
}
impl<D: Dim> AddAssign<isize> for NdVec<D> {
    fn add_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] += other;
        }
    }
}

// Implement subtraction between an NdVec and a scalar (i.e. subtract the scalar
// from each coordinate).
impl<D: Dim> Sub<isize> for NdVec<D> {
    type Output = Self;
    fn sub(mut self, other: isize) -> Self {
        self -= other;
        self
    }
}
impl<D: Dim> SubAssign<isize> for NdVec<D> {
    fn sub_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] -= other;
        }
    }
}

// Implement multiplication between an NdVec and a scalar (i.e. multiply each
// coordinate by the scalar).
impl<D: Dim> Mul<isize> for NdVec<D> {
    type Output = Self;
    fn mul(mut self, other: isize) -> Self {
        self *= other;
        self
    }
}
impl<D: Dim> MulAssign<isize> for NdVec<D> {
    fn mul_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] *= other;
        }
    }
}

// Implement _euclidean_ integer division between an NdVec and a scalar (i.e.
// divide each coordinate by the scalar). Note that this is not the same as
// Rust's normal division.
impl<D: Dim> Div<isize> for NdVec<D> {
    type Output = Self;
    fn div(mut self, other: isize) -> Self {
        self /= other;
        self
    }
}
impl<D: Dim> DivAssign<isize> for NdVec<D> {
    fn div_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] = self[ax].div_euclid(other);
        }
    }
}

// Implement integer division between an NdVec and a scalar (i.e. divide each
// coordinate by the scalar).
impl<D: Dim> Rem<isize> for NdVec<D> {
    type Output = Self;
    fn rem(mut self, other: isize) -> Self {
        self %= other;
        self
    }
}
impl<D: Dim> RemAssign<isize> for NdVec<D> {
    fn rem_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] %= other;
        }
    }
}

// Implement bitwise AND between an NdVec and a scalar (i.e. AND each coordinate
// with the scalar).
impl<D: Dim> BitAnd<isize> for NdVec<D> {
    type Output = Self;
    fn bitand(mut self, other: isize) -> Self {
        self &= other;
        self
    }
}
impl<D: Dim> BitAndAssign<isize> for NdVec<D> {
    fn bitand_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] &= other;
        }
    }
}

// Implement bitwise OR between an NdVec and a scalar (i.e. OR each coordinate
// with the scalar).
impl<D: Dim> BitOr<isize> for NdVec<D> {
    type Output = Self;
    fn bitor(mut self, other: isize) -> Self {
        self |= other;
        self
    }
}
impl<D: Dim> BitOrAssign<isize> for NdVec<D> {
    fn bitor_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] |= other;
        }
    }
}

// Implement bitwise XOR between an NdVec and a scalar (i.e. XOR each coordinate
// with the scalar).
impl<D: Dim> BitXor<isize> for NdVec<D> {
    type Output = Self;
    fn bitxor(mut self, other: isize) -> Self {
        self ^= other;
        self
    }
}
impl<D: Dim> BitXorAssign<isize> for NdVec<D> {
    fn bitxor_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] ^= other;
        }
    }
}

// Implement bitwise left-shift between an NdVec and a scalar (i.e. left-shift
// each coordinate by the scalar).
impl<D: Dim> Shl<isize> for NdVec<D> {
    type Output = Self;
    fn shl(mut self, other: isize) -> Self {
        self <<= other;
        self
    }
}
impl<D: Dim> ShlAssign<isize> for NdVec<D> {
    fn shl_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] <<= other;
        }
    }
}

// Implement bitwise right-shift between an NdVec and a scalar (i.e. right-shift
// each coordinate by the scalar).
impl<D: Dim> Shr<isize> for NdVec<D> {
    type Output = Self;
    fn shr(mut self, other: isize) -> Self {
        self >>= other;
        self
    }
}
impl<D: Dim> ShrAssign<isize> for NdVec<D> {
    fn shr_assign(&mut self, other: isize) {
        for &ax in D::axes() {
            self[ax] >>= other;
        }
    }
}

// Implement Euclidean division between an NdVec and a scalar (i.e. Euclidean
// divide each coordinate by the scalar).
impl<D: Dim> NdVec<D> {
    /// Performs Euclidean division between each coordinate of the vector and
    /// the given scalar.
    pub fn div_euclid(&self, other: isize) -> Self {
        let mut ret = *self;
        for &ax in D::axes() {
            ret[ax] = ret[ax].div_euclid(other);
        }
        ret
    }
}
