//! Operations between an `NdVec` and a scalar.

use std::ops::*;

use super::*;

macro_rules! forward_ndvec_op {
    (impl $imp:ident, $imp_assign:ident; fn $func:ident, $func_assign:ident; $op:tt, $op_assign:tt $(; $big_int:ident)?) => {
        impl<D: DimFor<N>, N: NdVecNum, X> $imp<X> for NdVec<D, N>
        where
            NdVec<D, N>: $imp_assign<X>,
        {
            type Output = Self;

            #[inline]
            fn $func(self, other: X) -> Self {
                let mut ret = self;
                ret $op_assign other;
                ret
            }
        }
        impl<'a, D: DimFor<N>, N: NdVecNum, X> $imp<X> for &'a NdVec<D, N>
        where
            NdVec<D, N>: $imp_assign<X>,
        {
            type Output = NdVec<D, N>;

            #[inline]
            fn $func(self, other: X) -> Self::Output {
                let mut ret = self.clone();
                ret $op_assign other;
                ret
            }
        }
        impl<D: DimFor<N>, N: NdVecNum> $imp_assign<N> for NdVec<D, N>
        where
            N: $imp_assign<N> + Copy,
        {
            #[inline]
            fn $func_assign(&mut self, other: N) {
                self.map_fn(|_ax, ret| *ret $op_assign other);
            }
        }
        $(
            impl<'a, D: Dim> $imp_assign<&'a $big_int> for BigVec<D> {
                #[inline]
                fn $func_assign(&mut self, other: &BigInt) {
                    self.map_fn(|_ax, ret| *ret $op_assign other);
                }
            }
        )?
    }
}

forward_ndvec_op!(
    impl Add, AddAssign;
    fn add, add_assign;
    +, +=;
    BigInt
);
forward_ndvec_op!(
    impl Sub, SubAssign;
    fn sub, sub_assign;
    -, -=;
    BigInt
);
forward_ndvec_op!(
    impl Mul, MulAssign;
    fn mul, mul_assign;
    *, *=;
    BigInt
);
forward_ndvec_op!(
    impl BitAnd, BitAndAssign;
    fn bitand, bitand_assign;
    &, &=;
    BigInt
);
forward_ndvec_op!(
    impl BitOr, BitOrAssign;
    fn bitor, bitor_assign;
    |, |=;
    BigInt
);
forward_ndvec_op!(
    impl BitXor, BitXorAssign;
    fn bitxor, bitxor_assign;
    ^, ^=;
    BigInt
);

// Implement bitwise left-shift between an NdVec and a scalar (i.e.
// left-shift each coordinate by the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> Shl<X> for NdVec<D, N>
where
    N: ShlAssign<X>,
{
    type Output = Self;

    #[inline]
    fn shl(self, other: X) -> Self {
        let mut ret = self;
        ret <<= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> Shl<X> for &'a NdVec<D, N>
where
    N: ShlAssign<X>,
{
    type Output = NdVec<D, N>;

    #[inline]
    fn shl(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret <<= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> ShlAssign<X> for NdVec<D, N>
where
    N: ShlAssign<X>,
{
    #[inline]
    fn shl_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret <<= other);
    }
}

// Implement bitwise right-shift between an NdVec and a scalar (i.e.
// right-shift each coordinate by the scalar).
impl<D: DimFor<N>, N: NdVecNum, X: Copy> Shr<X> for NdVec<D, N>
where
    N: ShrAssign<X>,
{
    type Output = Self;

    #[inline]
    fn shr(self, other: X) -> Self {
        let mut ret = self;
        ret >>= other;
        ret
    }
}
impl<'a, D: DimFor<N>, N: NdVecNum, X: Copy> Shr<X> for &'a NdVec<D, N>
where
    N: ShrAssign<X>,
{
    type Output = NdVec<D, N>;

    #[inline]
    fn shr(self, other: X) -> Self::Output {
        let mut ret = self.clone();
        ret >>= other;
        ret
    }
}
impl<D: DimFor<N>, N: NdVecNum, X: Copy> ShrAssign<X> for NdVec<D, N>
where
    N: ShrAssign<X>,
{
    #[inline]
    fn shr_assign(&mut self, other: X) {
        self.map_fn(|_ax, ret| *ret >>= other);
    }
}

// // Implement addition between an NdVec and a scalar (i.e. add the scalar
// // to each coordinate).
// impl<D: DimFor<N>, N: NdVecNum, X> Add<X> for NdVec<D, N>
// where
//     NdVec<D, N>: AddAssign<X>,
// {
//     type Output = Self;

//     #[inline]
//     fn add(self, other: X) -> Self {
//         let mut ret = self;
//         ret += other;
//         ret
//     }
// }
// impl<'a, D: DimFor<N>, N: NdVecNum, X> Add<X> for &'a NdVec<D, N>
// where
//     NdVec<D, N>: AddAssign<X>,
// {
//     type Output = NdVec<D, N>;

//     #[inline]
//     fn add(self, other: X) -> Self::Output {
//         let mut ret = self.clone();
//         ret += other;
//         ret
//     }
// }
// impl<D: DimFor<N>, N: NdVecNum> AddAssign<N> for NdVec<D, N>
// where
//     N: AddAssign<N> + Copy,
// {
//     #[inline]
//     fn add_assign(&mut self, other: N) {
//         self.map_fn(|_ax, ret| *ret += other);
//     }
// }
// impl<'a, D: Dim> AddAssign<&'a BigInt> for BigVec<D> {
//     #[inline]
//     fn add_assign(&mut self, other: &BigInt) {
//         self.map_fn(|_ax, ret| *ret += other);
//     }
// }

// // Implement subtraction between an NdVec and a scalar (i.e. subtract
// // the scalar from each coordinate).
// impl<D: DimFor<N>, N: NdVecNum, X> Sub<X> for NdVec<D, N>
// where
//     NdVec<D, N>: SubAssign<X>,
// {
//     type Output = Self;

//     #[inline]
//     fn sub(self, other: X) -> Self {
//         let mut ret = self;
//         ret -= other;
//         ret
//     }
// }
// impl<'a, D: DimFor<N>, N: NdVecNum, X> Sub<X> for &'a NdVec<D, N>
// where
//     NdVec<D, N>: SubAssign<X>,
// {
//     type Output = NdVec<D, N>;

//     #[inline]
//     fn sub(self, other: X) -> Self::Output {
//         let mut ret = self.clone();
//         ret -= other;
//         ret
//     }
// }
// impl<D: DimFor<N>, N: NdVecNum> SubAssign<N> for NdVec<D, N>
// where
//     N: SubAssign<N> + Copy,
// {
//     #[inline]
//     fn sub_assign(&mut self, other: N) {
//         self.map_fn(|_ax, ret| *ret -= other);
//     }
// }
// impl<'a, D: Dim> SubAssign<&'a BigInt> for BigVec<D> {
//     #[inline]
//     fn sub_assign(&mut self, other: &BigInt) {
//         self.map_fn(|_ax, ret| *ret -= other);
//     }
// }

// // Implement multiplication between an NdVec and a scalar (i.e. multiply
// // each coordinate by the scalar).
// impl<D: DimFor<N>, N: NdVecNum, X> Mul<X> for NdVec<D, N>
// where
//     NdVec<D, N>: MulAssign<X>,
// {
//     type Output = Self;

//     #[inline]
//     fn mul(self, other: X) -> Self {
//         let mut ret = self;
//         ret *= other;
//         ret
//     }
// }
// impl<'a, D: DimFor<N>, N: NdVecNum, X> Mul<X> for &'a NdVec<D, N>
// where
//     NdVec<D, N>: MulAssign<X>,
// {
//     type Output = NdVec<D, N>;

//     #[inline]
//     fn mul(self, other: X) -> Self::Output {
//         let mut ret = self.clone();
//         ret *= other;
//         ret
//     }
// }
// impl<D: DimFor<N>, N: NdVecNum> MulAssign<N> for NdVec<D, N>
// where
//     N: MulAssign<N> + Copy,
// {
//     #[inline]
//     fn mul_assign(&mut self, other: N) {
//         self.map_fn(|_ax, ret| *ret *= other);
//     }
// }
// impl<'a, D: Dim> MulAssign<&'a BigInt> for BigVec<D> {
//     #[inline]
//     fn mul_assign(&mut self, other: &BigInt) {
//         self.map_fn(|_ax, ret| *ret *= other);
//     }
// }
