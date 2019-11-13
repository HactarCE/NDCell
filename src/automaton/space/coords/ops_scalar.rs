//! Operations between a set of coordinates and a scalar.

use std::ops::*;

use super::*;

// Imlement addition between a set of coordinates and a scalar (i.e. add the
// scalar to each coordinate).
impl<D: Dim> Add<isize> for Coords<D> {
    type Output = Self;
    fn add(self, other: isize) -> Self {
        let mut ret = self;
        ret += other;
        ret
    }
}
impl<D: Dim> AddAssign<isize> for Coords<D> {
    fn add_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) + other);
        }
    }
}

// Implement subtraction between a set of coordinates and a scalar (i.e.
// subtract the scalar from each coordinate).
impl<D: Dim> Sub<isize> for Coords<D> {
    type Output = Self;
    fn sub(self, other: isize) -> Self {
        let mut ret = self;
        ret -= other;
        ret
    }
}
impl<D: Dim> SubAssign<isize> for Coords<D> {
    fn sub_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) - other);
        }
    }
}

// Implement multiplication between a set of coordinates and a scalar (i.e.
// multiply each coordinate by the scalar).
impl<D: Dim> Mul<isize> for Coords<D> {
    type Output = Self;
    fn mul(self, other: isize) -> Self {
        let mut ret = self;
        ret *= other;
        ret
    }
}
impl<D: Dim> MulAssign<isize> for Coords<D> {
    fn mul_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) * other);
        }
    }
}

// Implement integer division between a set of coordinates and a scalar (i.e.
// divide each coordinate by the scalar).
impl<D: Dim> Div<isize> for Coords<D> {
    type Output = Self;
    fn div(self, other: isize) -> Self {
        let mut ret = self;
        ret /= other;
        ret
    }
}
impl<D: Dim> DivAssign<isize> for Coords<D> {
    fn div_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) / other);
        }
    }
}

// Implement integer division between a set of coordinates and a scalar (i.e.
// divide each coordinate by the scalar).
impl<D: Dim> Rem<isize> for Coords<D> {
    type Output = Self;
    fn rem(self, other: isize) -> Self {
        let mut ret = self;
        ret %= other;
        ret
    }
}
impl<D: Dim> RemAssign<isize> for Coords<D> {
    fn rem_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) % other);
        }
    }
}

// Implement bitwise AND between a set of coordinates and a scalar (i.e. AND
// each coordinate with the scalar).
impl<D: Dim> BitAnd<isize> for Coords<D> {
    type Output = Self;
    fn bitand(self, other: isize) -> Self {
        let mut ret = self;
        ret &= other;
        ret
    }
}
impl<D: Dim> BitAndAssign<isize> for Coords<D> {
    fn bitand_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) & other);
        }
    }
}

// Implement bitwise OR between a set of coordinates and a scalar (i.e. OR each
// coordinate with the scalar).
impl<D: Dim> BitOr<isize> for Coords<D> {
    type Output = Self;
    fn bitor(self, other: isize) -> Self {
        let mut ret = self;
        ret |= other;
        ret
    }
}
impl<D: Dim> BitOrAssign<isize> for Coords<D> {
    fn bitor_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) | other);
        }
    }
}

// Implement bitwise XOR between a set of coordinates and a scalar (i.e. XOR
// each coordinate with the scalar).
impl<D: Dim> BitXor<isize> for Coords<D> {
    type Output = Self;
    fn bitxor(self, other: isize) -> Self {
        let mut ret = self;
        ret ^= other;
        ret
    }
}
impl<D: Dim> BitXorAssign<isize> for Coords<D> {
    fn bitxor_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) ^ other);
        }
    }
}

// Implement bitwise left-shift between a set of coordinates and a scalar (i.e.
// left-shift each coordinate by the scalar).
impl<D: Dim> Shl<isize> for Coords<D> {
    type Output = Self;
    fn shl(self, other: isize) -> Self {
        let mut ret = self;
        ret <<= other;
        ret
    }
}
impl<D: Dim> ShlAssign<isize> for Coords<D> {
    fn shl_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) << other);
        }
    }
}

// Implement bitwise right-shift between a set of coordinates and a scalar (i.e.
// right-shift each coordinate by the scalar).
impl<D: Dim> Shr<isize> for Coords<D> {
    type Output = Self;
    fn shr(self, other: isize) -> Self {
        let mut ret = self;
        ret >>= other;
        ret
    }
}
impl<D: Dim> ShrAssign<isize> for Coords<D> {
    fn shr_assign(&mut self, other: isize) {
        for ax in D::axes() {
            self.0.set(ax, self.0.get(ax) >> other);
        }
    }
}
