use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

/// Wrapper for `regex::Regex` with `impl`s for various common traits that
/// delegate to the original source strings.
///
/// See https://github.com/rust-lang/regex/issues/364.
pub struct Regex(pub regex::Regex);
impl fmt::Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{:?}", self.0.as_str())
    }
}
impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}
impl Eq for Regex {}
impl PartialOrd for Regex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.as_str().partial_cmp(other.0.as_str())
    }
}
impl Ord for Regex {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_str().cmp(other.0.as_str())
    }
}
impl Hash for Regex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state)
    }
}
impl Deref for Regex {
    type Target = regex::Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
