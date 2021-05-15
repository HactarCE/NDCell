use itertools::Itertools;
use std::convert::TryFrom;
use std::fmt;

/// Prefixes a string with 'a' or 'an' according to its first character.
pub fn a(s: impl fmt::Display) -> String {
    let s = s.to_string();
    match s.chars().next() {
        Some(ch) if "aeiouAEIOU".contains(ch) => format!("an {}", s),
        _ => format!("a {}", s),
    }
}

/// Returns a human-friendly list of things, joined at the end by the given
/// conjuction.
pub fn join_with_conjunction(conjunction: &str, items: &[impl fmt::Display]) -> String {
    match items {
        [] => format!("(none)"),
        [a] => format!("{}", a),
        [a, b] => format!("{} {} {}", a, conjunction, b),
        [all_but_last @ .., z] => {
            let mut ret = all_but_last.iter().map(|x| format!("{}, ", x)).join("");
            ret.push_str(conjunction);
            ret.push_str(&format!(" {}", z));
            ret
        }
    }
}

/// Formats a list surrounded by square brackets, but using `fmt::Display`.
pub fn display_bracketed_list(items: &[impl fmt::Display]) -> String {
    match items {
        [] => format!("[]"),
        [a] => format!("[{}]", a),
        [a, etc @ ..] => {
            let mut ret = format!("[{}", a);
            for item in etc {
                ret.push_str(&format!(", {}", item))
            }
            ret.push(']');
            ret
        }
    }
}

/// Wrapper around `i64::checked_pow()` that takes an `i64` exponent instead
/// (and correctly handles all cases where there is no overflow.)
pub fn checked_pow_i64(base: i64, exp: i64) -> Option<i64> {
    // Handle special cases where `u32::try_from()` fails even though there's no
    // overflow.
    match (base, exp) {
        _ if exp < 0 => None,
        (_, 0) => Some(1),
        (_, 1) => Some(base),
        (0, _) => Some(0),
        (1, _) => Some(1),
        (-1, _) => Some(if exp & 1 == 0 { 1 } else { -1 }),
        (_, _) => base.checked_pow(u32::try_from(exp).ok()?),
    }
}

#[test]
fn test_join_with_conjunction() {
    let strs = &["X", "Y", "Z"];
    assert_eq!(join_with_conjunction("and", &strs[..0]), "(none)");
    assert_eq!(join_with_conjunction("and", &strs[..1]), "X");
    assert_eq!(join_with_conjunction("and", &strs[..2]), "X and Y");
    // Oxford comma!
    assert_eq!(join_with_conjunction("and", &strs[..3]), "X, Y, and Z");
}
