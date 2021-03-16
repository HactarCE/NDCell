use itertools::Itertools;
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

#[test]
fn test_join_with_conjunction() {
    let strs = &["X", "Y", "Z"];
    assert_eq!(join_with_conjunction("and", &strs[..0]), "(none)");
    assert_eq!(join_with_conjunction("and", &strs[..1]), "X");
    assert_eq!(join_with_conjunction("and", &strs[..2]), "X and Y");
    // Oxford comma!
    assert_eq!(join_with_conjunction("and", &strs[..3]), "X, Y, and Z");
}

/// Automatically implements std::fmt::Display and std::str::FromStr on an enum
/// using custom-specified string representations.
macro_rules! enum_with_str_repr {
    (
        // For each enum ...
        $(
            // Get enum attributes (including #[derive] and docs).
            $(#[$enum_attr:meta])*
            // Get enum definition line.
            $vis:vis enum $enum_name:ident {
                // For each variant ...
                $(
                    // Get variant attributes (including docs).
                    $(#[$variant_attr:meta])*
                    // Get the variant name and its string representation.
                    $variant_name:ident = $variant_str:expr
                ),*
                $(,)?
            }
        )*
    ) => {
        // For each enum ...
        $(
            $(#[$enum_attr])*
            $vis enum $enum_name {
                $(
                    $(#[$variant_attr])*
                    $variant_name,
                )*
            }
            impl std::fmt::Display for $enum_name {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    match self {
                        $(
                            <$enum_name>::$variant_name => write!(f, "{}", $variant_str),
                        )*
                    }
                }
            }
            impl std::str::FromStr for $enum_name {
                type Err = ();
                fn from_str(s: &str) -> Result<Self, ()> {
                    match s {
                        $(
                            $variant_str => Ok(<$enum_name>::$variant_name),
                        )*
                        _ => Err(()),
                    }
                }
            }
        )*
    }
}
