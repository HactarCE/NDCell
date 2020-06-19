use itertools::Itertools;

/// Returns a human-friendly list of things, joined at the end by the given
///
/// ```
/// assert_eq!(join_with_conjunction("and", &[]), "(none)");
/// assert_eq!(join_with_conjunction("and", &["X"]), "X");
/// assert_eq!(join_with_conjunction("and", &["X", "Y"]), "X and Y");
/// // Oxford comma included
/// assert_eq!(join_with_conjunction("and", &["X", "Y", "Z"]), "X, Y, and Z");
/// ```
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
