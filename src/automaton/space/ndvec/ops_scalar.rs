//! Operations between an NdVec and a scalar.

use num::integer::Integer;
use std::ops::*;

use super::*;

macro_rules! impl_add_sub_mul {
    ($vec_type:ident with $coord_type:ident) => {
        // Implement addition between an NdVec and a scalar (i.e. add the scalar
        // to each coordinate).
        impl<D: Dim, N: Copy> Add<N> for $vec_type<D>
        where
            $coord_type: AddAssign<N>,
        {
            type Output = Self;
            fn add(self, other: N) -> Self {
                let mut ret = self;
                ret += other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> Add<N> for &'a $vec_type<D>
        where
            $coord_type: AddAssign<N>,
        {
            type Output = $vec_type<D>;
            fn add(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret += other;
                ret
            }
        }
        impl<D: Dim, N: Copy> AddAssign<N> for $vec_type<D>
        where
            $coord_type: AddAssign<N>,
        {
            fn add_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret += other);
            }
        }

        // Implement subtraction between an NdVec and a scalar (i.e. subtract
        // the scalar from each coordinate).
        impl<D: Dim, N: Copy> Sub<N> for $vec_type<D>
        where
            $coord_type: SubAssign<N>,
        {
            type Output = Self;
            fn sub(self, other: N) -> Self {
                let mut ret = self;
                ret -= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> Sub<N> for &'a $vec_type<D>
        where
            $coord_type: SubAssign<N>,
        {
            type Output = $vec_type<D>;
            fn sub(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret -= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> SubAssign<N> for $vec_type<D>
        where
            $coord_type: SubAssign<N>,
        {
            fn sub_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret -= other);
            }
        }

        // Implement multiplication between an NdVec and a scalar (i.e. multiply
        // each coordinate by the scalar).
        impl<D: Dim, N: Copy> Mul<N> for $vec_type<D>
        where
            $coord_type: MulAssign<N>,
        {
            type Output = Self;
            fn mul(self, other: N) -> Self {
                let mut ret = self;
                ret *= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> Mul<N> for &'a $vec_type<D>
        where
            $coord_type: MulAssign<N>,
        {
            type Output = $vec_type<D>;
            fn mul(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret *= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> MulAssign<N> for $vec_type<D>
        where
            $coord_type: MulAssign<N>,
        {
            fn mul_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret *= other);
            }
        }
    };
}

impl_add_sub_mul!(BigVec with BigInt);
impl_add_sub_mul!(FVec with f32);
impl_add_sub_mul!(IVec with isize);
impl_add_sub_mul!(ByteVec with u8);
impl_add_sub_mul!(UVec with usize);

macro_rules! impl_div_mod_floor {
    ($vec_type:ident with $coord_type:ident) => {
        // Implement floored euclidean division between an NdVec and a scalar
        // (i.e. divide each coordinate by the scalar). Note that this is not
        // the same as Rust's normal integer division operator.
        //
        // These are the only operations not implemented as traits.
        impl<D: Dim> $vec_type<D> {
            /// Floored integer division.
            pub fn div_floor(self, other: &$coord_type) -> Self {
                let mut ret = self;
                ret.map_fn(|_ax, ret| *ret = ret.div_floor(other));
                ret
            }
            /// Floored integer modulo.
            pub fn mod_floor(self, other: &$coord_type) -> Self {
                let mut ret = self;
                ret.map_fn(|_ax, ret| *ret = ret.mod_floor(other));
                ret
            }
        }
    };
}

impl_div_mod_floor!(BigVec with BigInt);
// impl_div_mod_floor!(FVec with f32);
impl_div_mod_floor!(IVec with isize);
impl_div_mod_floor!(ByteVec with u8);
impl_div_mod_floor!(UVec with usize);

impl<D: Dim, N: Copy> Div<N> for FVec<D>
where
    f32: DivAssign<N>,
{
    type Output = Self;
    fn div(self, other: N) -> Self {
        let mut ret = self;
        ret /= other;
        ret
    }
}
impl<'a, D: Dim, N: Copy> Div<N> for &'a FVec<D>
where
    f32: DivAssign<N>,
{
    type Output = FVec<D>;
    fn div(self, other: N) -> FVec<D> {
        let mut ret = self.clone();
        ret /= other;
        ret
    }
}
impl<D: Dim, N: Copy> DivAssign<N> for FVec<D>
where
    f32: DivAssign<N>,
{
    fn div_assign(&mut self, other: N) {
        self.map_fn(|_ax, ret| *ret /= other);
    }
}

macro_rules! impl_bit_ops {
    ($vec_type:ident with $coord_type:ident) => {
        // Implement bitwise AND between an NdVec and a scalar (i.e. AND each
        // coordinate with the scalar).
        impl<D: Dim, N: Copy> BitAnd<N> for $vec_type<D>
        where
            $coord_type: BitAndAssign<N>,
        {
            type Output = Self;
            fn bitand(self, other: N) -> Self {
                let mut ret = self;
                ret &= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> BitAnd<N> for &'a $vec_type<D>
        where
            $coord_type: BitAndAssign<N>,
        {
            type Output = $vec_type<D>;
            fn bitand(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret &= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> BitAndAssign<N> for $vec_type<D>
        where
            $coord_type: BitAndAssign<N>,
        {
            fn bitand_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret &= other);
            }
        }

        // Implement bitwise OR between an NdVec and a scalar (i.e. OR each
        // coordinate with the scalar).
        impl<D: Dim, N: Copy> BitOr<N> for $vec_type<D>
        where
            $coord_type: BitOrAssign<N>,
        {
            type Output = Self;
            fn bitor(self, other: N) -> Self {
                let mut ret = self;
                ret |= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> BitOr<N> for &'a $vec_type<D>
        where
            $coord_type: BitOrAssign<N>,
        {
            type Output = $vec_type<D>;
            fn bitor(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret |= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> BitOrAssign<N> for $vec_type<D>
        where
            $coord_type: BitOrAssign<N>,
        {
            fn bitor_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret |= other);
            }
        }

        // Implement bitwise XOR between an NdVec and a scalar (i.e. XOR each
        // coordinate with the scalar).
        impl<D: Dim, N: Copy> BitXor<N> for $vec_type<D>
        where
            $coord_type: BitXorAssign<N>,
        {
            type Output = Self;
            fn bitxor(self, other: N) -> Self {
                let mut ret = self;
                ret ^= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> BitXor<N> for &'a $vec_type<D>
        where
            $coord_type: BitXorAssign<N>,
        {
            type Output = $vec_type<D>;
            fn bitxor(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret ^= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> BitXorAssign<N> for $vec_type<D>
        where
            $coord_type: BitXorAssign<N>,
        {
            fn bitxor_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret ^= other);
            }
        }
        // Implement bitwise left-shift between an NdVec and a scalar (i.e.
        // left-shift each coordinate by the scalar).
        impl<D: Dim, N: Copy> Shl<N> for $vec_type<D>
        where
            $coord_type: ShlAssign<N>,
        {
            type Output = Self;
            fn shl(self, other: N) -> Self {
                let mut ret = self;
                ret <<= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> Shl<N> for &'a $vec_type<D>
        where
            $coord_type: ShlAssign<N>,
        {
            type Output = $vec_type<D>;
            fn shl(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret <<= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> ShlAssign<N> for $vec_type<D>
        where
            $coord_type: ShlAssign<N>,
        {
            fn shl_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret <<= other);
            }
        }

        // Implement bitwise right-shift between an NdVec and a scalar (i.e.
        // right-shift each coordinate by the scalar).
        impl<D: Dim, N: Copy> Shr<N> for $vec_type<D>
        where
            $coord_type: ShrAssign<N>,
        {
            type Output = Self;
            fn shr(self, other: N) -> Self {
                let mut ret = self;
                ret >>= other;
                ret
            }
        }
        impl<'a, D: Dim, N: Copy> Shr<N> for &'a $vec_type<D>
        where
            $coord_type: ShrAssign<N>,
        {
            type Output = $vec_type<D>;
            fn shr(self, other: N) -> Self::Output {
                let mut ret = self.clone();
                ret >>= other;
                ret
            }
        }
        impl<D: Dim, N: Copy> ShrAssign<N> for $vec_type<D>
        where
            $coord_type: ShrAssign<N>,
        {
            fn shr_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret >>= other);
            }
        }
    };
}

impl_bit_ops!(BigVec with BigInt);
impl_bit_ops!(FVec with f32);
impl_bit_ops!(IVec with isize);
impl_bit_ops!(ByteVec with u8);
impl_bit_ops!(UVec with usize);
