use std::fmt;

use super::*;
use crate::lexer::*;
use crate::{Span, Spanned};

/// Wrapper that consumes the same tokens but attaches span information to the
/// result.
#[derive(Debug, Copy, Clone)]
pub struct TokenSpanner<T>(pub T);
impl<T: fmt::Display> fmt::Display for TokenSpanner<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl<T: SyntaxConstruct> SyntaxConstruct for TokenSpanner<T> {
    type Output = Spanned<T::Output>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        self.0.might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        tf.next();
        let start = tf.span();
        tf.prev();
        let ret = self.0.build(tf);
        let end = tf.span();
        ret.map(|t| Spanned {
            span: Span::merge(start, end),
            inner: t,
        })
    }
}

/// Wrapper that consumes the same tokens but applies some function to the
/// result.
#[derive(Debug, Copy, Clone)]
pub struct TokenMapper<T, F> {
    /// Inner token eater.
    pub eater: T,
    /// Function to apply to the result.
    pub f: F,
}
impl<T: fmt::Display, F> fmt::Display for TokenMapper<T, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.eater.fmt(f)
    }
}
impl<B, T: SyntaxConstruct, F: Fn(T::Output) -> B> SyntaxConstruct for TokenMapper<T, F> {
    type Output = B;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        self.eater.might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        self.eater.build(tf).map(&self.f)
    }
}

/// Consumes another syntax structure surrounded by certain symbols, such as
/// parentheses or brackets.
#[derive(Debug, Copy, Clone)]
pub struct Surround<T> {
    inner: T,
    start: PunctuationToken,
    end: PunctuationToken,
}
impl<T> Surround<T> {
    /// Wraps the given syntax structure in parentheses.
    pub fn paren(inner: T) -> Self {
        use PunctuationToken::*;
        Self {
            inner,
            start: LParen,
            end: RParen,
        }
    }
    /// Wraps the given syntax structure in brackets.
    pub fn bracket(inner: T) -> Self {
        use PunctuationToken::*;
        Self {
            inner,
            start: LBracket,
            end: RBracket,
        }
    }
}
impl<T: fmt::Display> fmt::Display for Surround<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} surrounded by '{}' and '{}'",
            self.inner, self.start, self.end,
        )
    }
}
impl<T: Copy + SyntaxConstruct> SyntaxConstruct for Surround<T> {
    type Output = T::Output;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        self.start.might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        tf.parse(self.start)?;
        let ret = tf.parse(self.inner)?;
        tf.parse(self.end)?;
        Ok(ret)
    }
}

/// Consumes a list of syntax structures surrounded by a symbol pair, such as a
/// comma-separated list enclosed in parentheses.
#[derive(Debug, Copy, Clone)]
pub struct List<T> {
    /// Token eater for each element of the list.
    inner: T,
    /// Separator (e.g. comma).
    sep: PunctuationToken,
    /// Symbol at start (e.g. left paren).
    start: PunctuationToken,
    /// Symbol at end (e.g. right paren).
    end: PunctuationToken,
}
impl<T> List<T> {
    /// Produces a comma-separated list of the given syntax structure enclosed in parentheses.
    pub fn paren_comma_sep(inner: T) -> Self {
        use PunctuationToken::*;
        Self {
            inner,
            sep: Comma,
            start: LParen,
            end: RParen,
        }
    }
    /// Produces a comma-separated list of the given syntax structure enclosed in brackets.
    pub fn bracket_comma_sep(inner: T) -> Self {
        use PunctuationToken::*;
        Self {
            inner,
            sep: Comma,
            start: LBracket,
            end: RBracket,
        }
    }
}
impl<T: fmt::Display> fmt::Display for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}-separated list of {} surrounded by '{}' and '{}'",
            self.sep.name(),
            self.inner,
            self.start,
            self.end,
        )
    }
}
impl<T: Copy + SyntaxConstruct> SyntaxConstruct for List<T> {
    type Output = Vec<T::Output>;
    fn might_match(&self, tf: Parser<'_>) -> bool {
        self.start.might_match(tf)
    }
    fn eat(&self, tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        let mut items = vec![];
        tf.parse(self.start)?;
        loop {
            // End the list or consume an item.
            if let Some(item) = feed_one_of!(tf, [self.inner.map(Some), self.end.map(|_| None)])? {
                // There is an item.
                items.push(item);
            } else {
                // End of list; empty list, or trailing separator.
                break;
            }
            // End the list or consume a separator.
            if let Some(_) = feed_one_of!(tf, [self.sep.map(Some), self.end.map(|_| None)])? {
                // There is a separator.
                continue;
            } else {
                // End of list, no trailing separator.
                break;
            }
        }
        Ok(items)
    }
}

/// Consumes no tokens; always succeeds (useful as a fallback when matching
/// multiple possible token eaters).
pub struct Epsilon;
impl_display!(Epsilon, "nothing");
impl SyntaxConstruct for Epsilon {
    type Output = ();
    fn might_match(&self, _tf: Parser<'_>) -> bool {
        true
    }
    fn eat(&self, _tf: &mut Parser<'_>) -> LangResult<Self::Output> {
        Ok(())
    }
}
