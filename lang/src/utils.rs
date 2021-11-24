use itertools::Itertools;
use std::convert::TryFrom;
use std::fmt;

use ndcell_core::prelude::IRect6D;

/// Implements `std::format::Display` for a type using arguments to `write!()`.
macro_rules! impl_display {
    ( for $typename:ty, $( $fmt_arg:expr ),+ $(,)? ) => {
        impl std::fmt::Display for $typename {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, $( $fmt_arg ),+ )
            }
        }
    };
}

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
    format!("[{}]", items.iter().map(|x| x.to_string()).join(", "))
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

pub fn ndarray_strides(ndim: usize, bounds: IRect6D) -> Vec<i64> {
    // TODO: document this function & move it into ndcell_core

    // The strides array is the cumulative product of the size vector of the
    // bounding rectangle.
    bounds.size().0[..ndim]
        .iter()
        .scan(1, |prod, &i| {
            *prod *= i;
            Some((*prod / i) as i64)
        })
        .collect()
}
pub fn ndarray_base_idx(bounds: IRect6D, strides: &[i64]) -> i64 {
    strides
        .iter()
        .zip(&bounds.min().0)
        .map(|(&stride, &i)| stride * i as i64)
        .sum()
}

/// Applies `map` to the inside of two `Option`s, then merges the result using
/// `merge` if both are `Some`.
pub fn map_and_merge_options<T, U>(
    a: Option<T>,
    b: Option<T>,
    mut map: impl FnMut(T) -> U,
    merge: impl FnOnce(U, U) -> U,
) -> Option<U> {
    let a = a.map(&mut map);
    let b = b.map(&mut map);
    match (a, b) {
        (Some(x), Some(y)) => Some(merge(x, y)),
        (x, y) => x.or(y),
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

/// Returns a nonsense `Span` for use in tests.
#[cfg(test)]
pub fn dummy_span() -> codemap::Span {
    codemap::CodeMap::new()
        .add_file("_".to_string(), "".to_string())
        .span
}
