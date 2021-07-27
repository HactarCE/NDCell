/// Parses the first matching syntax rule using `try_parse()` or returns an
/// error listing all of them if none match.
macro_rules! parse_one_of {
    ( $p:expr, $ast:expr, [ $( @ $varname:ident, )* $first_rule:expr $(, $rule:expr )* $(,)? ] $(,)? ) => {
        {
            let this_var_is_unique = $first_rule;
            parse_one_of!(
                $p,
                $ast,
                [
                    $( @ $varname, )*
                    @ this_var_is_unique,
                    $( $rule, )*
                ],
            )
        }
    };
    ( $p:expr, $ast:expr, [ $( @ $varname:ident, )+ ], ) => {
        None
            $( .or_else(|| $p.try_parse($ast, &$varname)) )+
            .unwrap_or_else(|| $p.expected(
                crate::utils::join_with_conjunction("or", &[
                    $( $varname.to_string(), )+
                ])
            ))
    };
}
