use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::{Borrow, BorrowMut};

lazy_static! {
    /// A regex pattern that matches linebreaks.
    ///
    /// Note that `^` or `$` only match '\n' in Rust, so that is insufficient.
    static ref NEWLINE_PATTERN: Regex = Regex::new("\r\n?|\n").unwrap();
}

/// The 1-indexed line and column number of a given character index in a string.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TextPoint(pub usize, pub usize);
impl TextPoint {
    /// Finds the line and column number of the given character index in the
    /// given string.
    pub fn from_idx(s: &str, idx: usize) -> Self {
        let (line_number, line_start_idx) = NEWLINE_PATTERN
            // Find all linebreaks.
            .find_iter(s)
            // Give them numbers, where the nth linebreak separates line (n+1) from
            // line (n+2) (with the first line of the string being line 1).
            .enumerate()
            // Find the indices of all line beginnings, and increment line numbers
            // to indicate the number of the line that they start.
            .map(|(line_num, regex_match)| (line_num + 2, regex_match.end()))
            // Ignore any lines after the desired index.
            .take_while(|&(_, line_start_idx)| line_start_idx <= idx)
            // Find the last line beginning that occurs before the given index.
            .last()
            // If the index occurs before any linebreaks, then this is the first
            // line and the index of the start of this line is 0.
            .unwrap_or((1, 0));
        // The first column is numbered 1.
        let column_number = idx - line_start_idx + 1;
        Self(line_number, column_number)
    }
    // Returns the 1-indexed line number of this text point.
    pub fn line(self) -> usize {
        self.0
    }
    // Returns the 1-indexed column number of this text point.
    pub fn column(self) -> usize {
        self.1
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl Span {
    pub fn textpoints(self, string: &str) -> (TextPoint, TextPoint) {
        (
            TextPoint::from_idx(string, self.start),
            TextPoint::from_idx(string, self.end),
        )
    }
    pub fn merge<T: Into<Span>, U: Into<Span>>(span1: T, span2: U) -> Self {
        let span1: Span = span1.into();
        let span2: Span = span2.into();
        Self {
            start: std::cmp::min(span1.start, span2.start),
            end: std::cmp::max(span1.end, span2.end),
        }
    }
}
impl<T> From<Spanned<T>> for Span {
    fn from(spanned: Spanned<T>) -> Self {
        spanned.span
    }
}
impl<T> From<&Spanned<T>> for Span {
    fn from(spanned: &Spanned<T>) -> Self {
        spanned.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}
impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T {
        &self.inner
    }
}
impl<T> BorrowMut<T> for Spanned<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}
impl<T> Spanned<T> {
    pub fn new(start: usize, end: usize, inner: T) -> Self {
        Self {
            span: Span { start, end },
            inner,
        }
    }
    pub fn replace<U>(&self, new_inner: U) -> Spanned<U> {
        Spanned {
            span: self.span,
            inner: new_inner,
        }
    }
    pub fn default(inner: T) -> Self {
        let span = Span::default();
        Self { span, inner }
    }
    pub fn into_tuple(self) -> (Span, T) {
        (self.span, self.inner)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_textpoint() {
        let s = "abc\ndef\r\nghi\rjkl";
        let expected_textpoints = vec![
            TextPoint(1, 1),
            TextPoint(1, 2),
            TextPoint(1, 3),
            TextPoint(1, 4),
            TextPoint(2, 1),
            TextPoint(2, 2),
            TextPoint(2, 3),
            TextPoint(2, 4),
            TextPoint(2, 5),
            TextPoint(3, 1),
            TextPoint(3, 2),
            TextPoint(3, 3),
            TextPoint(3, 4),
            TextPoint(4, 1),
            TextPoint(4, 2),
            TextPoint(4, 3),
        ];
        let actual_textpoints: Vec<_> = (0..s.len())
            .map(|idx| TextPoint::from_idx(&s, idx))
            .collect();
        assert_eq!(expected_textpoints, actual_textpoints);
    }
}
