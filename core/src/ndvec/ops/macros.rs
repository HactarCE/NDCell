macro_rules! do_assign_op {
    (($lhs:expr) + $rhs:expr) => {
        $lhs += $rhs;
    };
    (($lhs:expr) - $rhs:expr) => {
        $lhs -= $rhs;
    };
    (($lhs:expr) * $rhs:expr) => {
        $lhs *= $rhs;
    };
    (($lhs:expr) / $rhs:expr) => {
        $lhs /= $rhs;
    };
    (($lhs:expr) % $rhs:expr) => {
        $lhs %= $rhs;
    };
    (($lhs:expr) & $rhs:expr) => {
        $lhs &= $rhs;
    };
    (($lhs:expr) | $rhs:expr) => {
        $lhs |= $rhs;
    };
    (($lhs:expr) ^ $rhs:expr) => {
        $lhs ^= $rhs;
    };
    (($lhs:expr) << $rhs:expr) => {
        $lhs <<= $rhs;
    };
    (($lhs:expr) >> $rhs:expr) => {
        $lhs >>= $rhs;
    };
}

macro_rules! impl_op {
    (($($imp:tt)+) ($lhs:ty) + ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Add<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn add $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) - ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Sub<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn sub $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) * ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Mul<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn mul $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) / ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Div<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn div $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) % ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Rem<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn rem $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) & ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ BitAnd<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn bitand $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) | ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ BitOr<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn bitor $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) ^ ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ BitXor<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn bitxor $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) << ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Shl<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn shl $sig -> $out $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) >> ($rhs:ty) { $sig:tt -> $out:ty $body:block }) => {
        $($imp)+ Shr<$rhs> for $lhs {
            type Output = $out;

            #[inline]
            fn shr $sig -> $out $body
        }
    };
}

macro_rules! impl_assign_op {
    (($($imp:tt)+) ($lhs:ty) + ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ AddAssign<$rhs> for $lhs {
            #[inline]
            fn add_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) - ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ SubAssign<$rhs> for $lhs {
            #[inline]
            fn sub_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) * ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ MulAssign<$rhs> for $lhs {
            #[inline]
            fn mul_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) / ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ DivAssign<$rhs> for $lhs {
            #[inline]
            fn div_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) % ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ RemAssign<$rhs> for $lhs {
            #[inline]
            fn rem_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) & ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ BitAndAssign<$rhs> for $lhs {
            #[inline]
            fn bitand_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) | ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ BitOrAssign<$rhs> for $lhs {
            #[inline]
            fn bitor_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) ^ ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ BitXorAssign<$rhs> for $lhs {
            #[inline]
            fn bitxor_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) << ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ ShlAssign<$rhs> for $lhs {
            #[inline]
            fn shl_assign $sig $body
        }
    };

    (($($imp:tt)+) ($lhs:ty) >> ($rhs:ty) { $sig:tt $body:block }) => {
        $($imp)+ ShrAssign<$rhs> for $lhs {
            #[inline]
            fn shr_assign $sig $body
        }
    };
}

macro_rules! impl_vec_num_op {
    (
        impl ($($op_trait:tt)*) for $ndvec:ty;
        $lhs_num:ident $op:tt $rhs_num:ty; $($maybe_ref:tt)?
    ) => {
        impl_op!((impl<'a, D: DimFor<$lhs_num>>) ($ndvec) $op ($rhs_num) {
            (self, other: $rhs_num) -> NdVec<D, $lhs_num> {
                NdVec::from_fn(|ax| &self[ax] $op $($maybe_ref)? other)
            }
        });
    };
}
macro_rules! impl_vec_num_assign_op {
    (
        impl ($($op_trait:tt)*) for $ndvec:ty;
        $lhs_num:ident $op:tt $rhs_num:ty; $($maybe_ref:tt)?
    ) => {
        impl_assign_op!((impl<'a, D: DimFor<$lhs_num>>) ($ndvec) $op ($rhs_num) {
            (&mut self, other: $rhs_num) {
                for &ax in D::Dim::axes() {
                    do_assign_op!((self[ax]) $op $($maybe_ref)? Clone::clone(&other));
                }
            }
        });
    };
}

macro_rules! impl_vec_vec_op {
    (
        impl ($($op_trait:tt)*) for $ndvec_lhs:ty, $ndvec_rhs:ty;
        $lhs_num:ident $op:tt $rhs_num:ty;
    ) => {
        impl_op!((impl<'a, D: DimFor<$lhs_num> + DimFor<$rhs_num>>) ($ndvec_lhs) $op ($ndvec_rhs) {
            (self, other: $ndvec_rhs) -> NdVec<D, $lhs_num> {
                NdVec::from_fn(|ax| &self[ax] $op &other[ax])
            }
        });
    };
}
macro_rules! impl_vec_vec_assign_op {
    (
        impl ($($op_trait:tt)*) for $ndvec_lhs:ty, $ndvec_rhs:ty;
        $lhs_num:ident $op:tt $rhs_num:ty;
    ) => {
        impl_assign_op!((impl<'a, D: DimFor<$lhs_num> + DimFor<$rhs_num>>) ($ndvec_lhs) $op ($ndvec_rhs) {
            (&mut self, other: $ndvec_rhs) {
                for &ax in D::Dim::axes() {
                    do_assign_op!( (self[ax]) $op &other[ax]);
                }
            }
        });
    };
}

macro_rules! impl_vec_num_ops {
    ($lhs_num:ident $op:tt $rhs_num:ident) => {
        impl_vec_num_op!(
            impl (op_trait!($op)) for NdVec<D, $lhs_num>;
            $lhs_num $op $rhs_num; &
        );
        impl_vec_num_op!(
            impl (op_trait!($op)) for NdVec<D, $lhs_num>;
            $lhs_num $op &'a $rhs_num;
        );
        impl_vec_num_op!(
            impl (op_trait!($op)) for &'a NdVec<D, $lhs_num>;
            $lhs_num $op $rhs_num; &
        );
        impl_vec_num_op!(
            impl (op_trait!($op)) for &'a NdVec<D, $lhs_num>;
            $lhs_num $op &'a $rhs_num;
        );
        impl_vec_num_assign_op!(
            impl (op_assign_trait!($op)) for NdVec<D, $lhs_num>;
            $lhs_num $op $rhs_num;
        );
        impl_vec_num_assign_op!(
            impl (op_assign_trait!($op)) for NdVec<D, $lhs_num>;
            $lhs_num $op &'a $rhs_num;
        );
    };
}

macro_rules! impl_vec_vec_ops {
    ($lhs_num:ident $op:tt $rhs_num:ident) => {
        impl_vec_vec_op!(
            impl (op_trait!($op)) for NdVec<D, $lhs_num>, NdVec<D, $rhs_num>;
            $lhs_num $op $rhs_num;
        );
        impl_vec_vec_op!(
            impl (op_trait!($op)) for NdVec<D, $lhs_num>, &'a NdVec<D, $rhs_num>;
            $lhs_num $op $rhs_num;
        );
        impl_vec_vec_op!(
            impl (op_trait!($op)) for &'a NdVec<D, $lhs_num>, NdVec<D, $rhs_num>;
            $lhs_num $op $rhs_num;
        );
        impl_vec_vec_op!(
            impl (op_trait!($op)) for &'a NdVec<D, $lhs_num>, &'a NdVec<D, $rhs_num>;
            $lhs_num $op $rhs_num;
        );
        impl_vec_vec_assign_op!(
            impl (op_assign_trait!($op)) for NdVec<D, $lhs_num>, NdVec<D, $rhs_num>;
            $lhs_num $op $rhs_num;
        );
        impl_vec_vec_assign_op!(
            impl (op_assign_trait!($op)) for NdVec<D, $lhs_num>, &'a NdVec<D, $rhs_num>;
            $lhs_num $op $rhs_num;
        );
    };
}

macro_rules! impl_multi_ndvec_ops {
    (impl $op:tt $rhs:ident for $($num:ident),+) => {
        $(
            impl_vec_num_ops!($num $op $rhs);
        )+
    };
    (impl $op:tt for $($num:ident),+) => {
        $(
            impl_vec_num_ops!($num $op $num);
            impl_vec_vec_ops!($num $op $num);
        )+
    };
}
