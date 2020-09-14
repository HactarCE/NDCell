//! Macros for efficiently implementing traits on `FixedPoint`, based on the
//! ones in `num-bigint`.

macro_rules! forward_val_val_binop {
    (impl $imp:ident<$num:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<$num> for $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: $num) -> $res {
                let other = $res::try_from(other).unwrap();
                $res($imp::$method(self.0 $($($pre)+)?, other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_ref_val_binop {
    (impl $imp:ident<$num:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<$num> for &'a $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: $num) -> $res {
                let other = $res::try_from(other).unwrap();
                $res($imp::$method(&self.0 $($($pre)+)?, other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_val_ref_binop {
    (impl $imp:ident<$num:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<&'a $num> for $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: &$num) -> $res {
                let other = $res::try_from(other.clone()).unwrap();
                $res($imp::$method(self.0 $($($pre)+)?, &other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_ref_ref_binop {
    (impl $imp:ident<$num:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<&'a $num> for &'a $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: &$num) -> $res {
                let other = $res::try_from(other.clone()).unwrap();
                $res($imp::$method(&self.0 $($($pre)+)?, &other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_val_assign {
    (impl $imp:ident<$num:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl $imp<$num> for $res {
            #[inline]
            fn $method(&mut self, other: $num) {
                let other = $res::try_from(other).unwrap();
                $(self.0 $($pre)+;)?
                $imp::$method(&mut self.0, other.0);
                $(self.0 $($post)+;)?
            }
        }
    };
}

macro_rules! forward_ref_assign {
    (impl $imp:ident<$num:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<&$num> for $res {
            #[inline]
            fn $method(&mut self, other: &$num) {
                $(self.0 $($pre)+;)?
                $imp::$method(&mut self.0, &$res::try_from(other.clone()).unwrap().0);
                $(self.0 $($post)+;)?
            }
        }
    };
}

macro_rules! forward_binop {
    (impl $imp:ident<$num_type:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)? $(,)?) => {
        forward_val_val_binop!(impl $imp<$num_type> for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_ref_val_binop!(impl $imp<$num_type> for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_val_ref_binop!(impl $imp<$num_type> for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_ref_ref_binop!(impl $imp<$num_type> for $res $(($($pre)+))?, $method $(, $($post)+)?);
    };
}

macro_rules! forward_assign {
    (impl $imp:ident<$num_type:ty> for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)? $(,)?) => {
        forward_val_assign!(impl $imp<$num_type> for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_ref_assign!(impl $imp<$num_type> for $res $(($($pre)+))?, $method $(, $($post)+)?);
    };
}
