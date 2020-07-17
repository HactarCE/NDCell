/// Returns `true` if the next token of the given token feeder matches the given
/// pattern.
macro_rules! next_token_matches {
    ( $tf:expr, $( $pattern:pat )|+ $( if $guard: expr )? $(,)? ) => {
        matches!(
            $tf.peek_next_class(),
            $( Some($pattern) )|+ $( if $guard )?
        )
    };
}

/// "Feeds" the first matching "token eater" using the given token feeder, or
/// returns an error listing all of the token eaters if none match.
macro_rules! feed_one_of {
    ( $tf:expr, [ $( @ $varname:ident, )* $first_eater:expr $(, $eater:expr )* $(,)? ] $(,)? ) => {
        {
            let this_var_is_unique = $first_eater;
            feed_one_of!(
                $tf,
                [
                    $( @ $varname, )*
                    @ this_var_is_unique,
                    $( $eater, )*
                ],
            )
        }
    };
    ( $tf:expr, [ $( @ $varname:ident, )+ ], ) => {
        None
            $( .or_else(|| $tf.try_feed(&$varname)) )+
            .unwrap_or_else(|| $tf.expected(
                crate::utils::join_with_conjunction("or", &[
                    $( $varname.to_string(), )+
                ])
            ))
    };
}

/// Implements `std::format::Display` for the given type using the given
/// arguments to `write!()`.
macro_rules! impl_display {
    ( $typename:ty, $( $fmt_arg:expr ),+ $(,)? ) => {
        impl std::fmt::Display for $typename {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, $( $fmt_arg ),+ )
            }
        }
    };
}

/// Implements the `TokenEater` trait for the given type `T`, as long as
/// `TokenClass: From<T>`.
///
/// TODO: once #![feature(specialization)] stabalizes, try using that instead.
macro_rules! impl_tokeneater_for_tokenclass {
    ( $typename:ty ) => {
        impl TokenEater for $typename {
            type Output = ();
            fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
                crate::lexer::TokenClass::from(*self).might_match(tf)
            }
            fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
                crate::lexer::TokenClass::from(*self).eat(tf)
            }
        }
    };
}
