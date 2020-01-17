//! Operations between two NdVecs (and unary negation operator).

use std::ops::*;

use super::*;

macro_rules! impl_neg {
    ($vec_type:ident with $coord_type:ident) => {
        // Implement negation of an NdVec (i.e. negate each coordinate).
        impl<D: Dim> Neg for $vec_type<D> {
            type Output = Self;
            fn neg(self) -> Self {
                let mut ret = self;
                for &ax in D::axes() {
                    ret[ax] = -&ret[ax];
                }
                ret
            }
        }
        impl<'a, D: Dim> Neg for &'a $vec_type<D> {
            type Output = $vec_type<D>;
            fn neg(self) -> Self::Output {
                let mut ret = self.clone();
                for &ax in D::axes() {
                    ret[ax] = -&ret[ax];
                }
                ret
            }
        }
    };
}

impl_neg!(BigVec with BigInt);
impl_neg!(FVec with R64);
impl_neg!(IVec with isize);
// impl_neg!(UVec with usize);
// impl_neg!(ByteVec with u8);

macro_rules! impl_add_sub_mul_copy {
    ($ndvec1:ident + $ndvec2:ident) => {
        // Implement elementwise addition between two NdVecs.
        impl<D: Dim> AddAssign<$ndvec2<D>> for $ndvec1<D> {
            fn add_assign(&mut self, other: $ndvec2<D>) {
                for &ax in D::axes() {
                    self[ax] += other[ax];
                }
            }
        }

        // Implement elementwise subtraction between two NdVecs.
        impl<D: Dim> SubAssign<$ndvec2<D>> for $ndvec1<D> {
            fn sub_assign(&mut self, other: $ndvec2<D>) {
                for &ax in D::axes() {
                    self[ax] -= other[ax];
                }
            }
        }

        // Implement elementwise multiplication between two NdVecs.
        impl<D: Dim> MulAssign<$ndvec2<D>> for $ndvec1<D> {
            fn mul_assign(&mut self, other: $ndvec2<D>) {
                for &ax in D::axes() {
                    self[ax] *= other[ax];
                }
            }
        }
    };
}

macro_rules! impl_add_sub_mul_ref {
    ($ndvec1:ident + $ndvec2:ident) => {
        // Implement elementwise addition between two NdVecs.
        impl<D: Dim> AddAssign<$ndvec2<D>> for $ndvec1<D> {
            fn add_assign(&mut self, other: $ndvec2<D>) {
                for &ax in D::axes() {
                    self[ax] += &other[ax];
                }
            }
        }

        // Implement elementwise subtraction between two NdVecs.
        impl<D: Dim> SubAssign<$ndvec2<D>> for $ndvec1<D> {
            fn sub_assign(&mut self, other: $ndvec2<D>) {
                for &ax in D::axes() {
                    self[ax] -= &other[ax];
                }
            }
        }

        // Implement elementwise multiplication between two NdVecs.
        impl<D: Dim> MulAssign<$ndvec2<D>> for $ndvec1<D> {
            fn mul_assign(&mut self, other: $ndvec2<D>) {
                for &ax in D::axes() {
                    self[ax] *= &other[ax];
                }
            }
        }
    };
}

impl_add_sub_mul_ref!(BigVec + BigVec);
// impl_add_sub_mul_copy!(BigVec + FVec);
impl_add_sub_mul_copy!(BigVec + IVec);
impl_add_sub_mul_copy!(BigVec + UVec);
impl_add_sub_mul_copy!(BigVec + ByteVec);

impl_add_sub_mul_copy!(FVec + FVec);

impl_add_sub_mul_copy!(IVec + IVec);

impl_add_sub_mul_copy!(UVec + UVec);

impl_add_sub_mul_copy!(ByteVec + ByteVec);
