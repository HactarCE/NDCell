use itertools::Itertools;

/// Returns a human-friendly list of things, joined at the end by the given
/// conjuction.
pub fn join_with_conjunction(conjunction: &str, items: &[impl std::fmt::Display]) -> String {
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

/// Divides one number by another, rounding up.
pub fn div_ceil(n: usize, d: usize) -> usize {
    (n + d - 1) / d
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

#[test]
fn test_div_ceil() {
    assert_eq!(div_ceil(0, 5), 0);
    assert_eq!(div_ceil(1, 5), 1);
    assert_eq!(div_ceil(2, 5), 1);
    assert_eq!(div_ceil(3, 5), 1);
    assert_eq!(div_ceil(4, 5), 1);
    assert_eq!(div_ceil(5, 5), 1);
    assert_eq!(div_ceil(6, 5), 2);
    assert_eq!(div_ceil(7, 5), 2);
    assert_eq!(div_ceil(8, 5), 2);
    assert_eq!(div_ceil(9, 5), 2);
    assert_eq!(div_ceil(10, 5), 2);
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
