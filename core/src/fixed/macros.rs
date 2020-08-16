//! Macros for efficiently implementing traits on `FixedPoint`, based on the
//! ones in `num-bigint`.

macro_rules! forward_val_val_binop {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl $imp<$res> for $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: $res) -> $res {
                $res($imp::$method(self.0 $($($pre)+)?, other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_ref_val_binop {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<$res> for &'a $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: $res) -> $res {
                $res($imp::$method(&self.0 $($($pre)+)?, other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_val_ref_binop {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a> $imp<&'a $res> for $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: &$res) -> $res {
                $res($imp::$method(self.0 $($($pre)+)?, &other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_ref_ref_binop {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl<'a, 'b> $imp<&'b $res> for &'a $res {
            type Output = $res;

            #[inline]
            fn $method(self, other: &$res) -> $res {
                $res($imp::$method(&self.0 $($($pre)+)?, &other.0) $($($post)+)?)
            }
        }
    };
}

macro_rules! forward_val_assign {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl $imp<$res> for $res {
            #[inline]
            fn $method(&mut self, other: $res) {
                $(self.0 $($pre)+;)?
                $imp::$method(&mut self.0, other.0);
                $(self.0 $($post)+;)?
            }
        }
    };
}

macro_rules! forward_ref_assign {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)?) => {
        impl $imp<&$res> for $res {
            #[inline]
            fn $method(&mut self, other: &$res) {
                $(self.0 $($pre)+;)?
                $imp::$method(&mut self.0, &other.0);
                $(self.0 $($post)+;)?
            }
        }
    };
}

macro_rules! forward_binop {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)? $(,)?) => {
        forward_val_val_binop!(impl $imp for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_ref_val_binop!(impl $imp for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_val_ref_binop!(impl $imp for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_ref_ref_binop!(impl $imp for $res $(($($pre)+))?, $method $(, $($post)+)?);
    };
}

macro_rules! forward_assign {
    (impl $imp:ident for $res:ident $(($($pre:tt)+))?, $method:ident $(, $($post:tt)+)? $(,)?) => {
        forward_val_assign!(impl $imp for $res $(($($pre)+))?, $method $(, $($post)+)?);
        forward_ref_assign!(impl $imp for $res $(($($pre)+))?, $method $(, $($post)+)?);
    };
}
