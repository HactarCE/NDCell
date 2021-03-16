use codemap::Spanned;
use std::convert::TryFrom;
use std::fmt;
use std::marker::PhantomData;

use super::{Ctx, Parser, SyntaxRule};
use crate::errors::Result;
use crate::lexer::Token;

/// Rule that matches the same tokens but applies some function to the
/// result.
#[derive(Debug, Copy, Clone)]
pub struct TokenMapper<R, F> {
    /// Inner syntax rule.
    pub inner: R,
    /// Function to apply to the result.
    pub f: F,
}
impl<R: fmt::Display, F> fmt::Display for TokenMapper<R, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}
impl<B, R: SyntaxRule, F: Fn(R::Output) -> B> SyntaxRule for TokenMapper<R, F> {
    type Output = B;

    fn might_match(&self, p: Parser<'_>) -> bool {
        self.inner.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        self.inner.consume_match(p, ctx).map(&self.f)
    }
}

/// Rule that matches tokens by certain symbols, such as
/// parentheses or brackets.
#[derive(Debug, Copy, Clone)]
pub struct Surround<R> {
    /// Inner syntax rule.
    inner: R,

    /// Symbol at start (e.g. left paren).
    start: Token,
    /// Symbol at end (e.g. right paren).
    end: Token,
}
impl<R> Surround<R> {
    /// Wraps the rule in parentheses.
    pub fn paren(inner: R) -> Self {
        Self {
            inner,

            start: Token::LParen,
            end: Token::RParen,
        }
    }
    /// Wraps the rule in brackets.
    pub fn bracket(inner: R) -> Self {
        Self {
            inner,

            start: Token::LBracket,
            end: Token::RBracket,
        }
    }
}
impl<R: fmt::Display> fmt::Display for Surround<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}, surrounded by {} and {}",
            self.inner, self.start, self.end,
        )
    }
}
impl<R: Copy + SyntaxRule> SyntaxRule for Surround<R> {
    type Output = Spanned<R::Output>;

    fn might_match(&self, p: Parser<'_>) -> bool {
        self.start.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        let span1 = p.peek_next_span();

        p.parse(ctx, self.start)?;
        let ret = p.parse(ctx, self.inner)?;
        p.parse(ctx, self.end)?;

        let span2 = p.span();
        let span = span1.merge(span2);
        Ok(Spanned { span, node: ret })
    }
}

/// Rule that matches a list of things surrounded by a symbol pair,
/// such as a comma-separated list enclosed in parentheses.
#[derive(Debug, Copy, Clone)]
pub struct List<R> {
    /// Syntax rule for each element of the list.
    inner: R,

    /// Separator (e.g. comma).
    sep: Token,
    /// Symbol at start (e.g. left paren).
    start: Token,
    /// Symbol at end (e.g. right paren).
    end: Token,

    /// User-friendly name for separator.
    sep_name: &'static str,
}
impl<R> List<R> {
    /// Returns a rule for a comma-separated list surrounded by parentheses.
    pub fn paren_comma_sep(inner: R) -> Self {
        Self {
            inner,

            sep: Token::Comma,
            start: Token::LParen,
            end: Token::RParen,

            sep_name: "comma",
        }
    }
    /// Returns a rule for a comma-separated list surrounded by brackets.
    pub fn bracket_comma_sep(inner: R) -> Self {
        Self {
            inner,

            sep: Token::Comma,
            start: Token::LBracket,
            end: Token::RBracket,

            sep_name: "comma",
        }
    }
}
impl<R: fmt::Display> fmt::Display for List<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}-separated list of {}, surrounded by {} and {}",
            self.sep_name, self.inner, self.start, self.end,
        )
    }
}
impl<R: Copy + SyntaxRule> SyntaxRule for List<R> {
    type Output = Spanned<Vec<R::Output>>;

    fn might_match(&self, p: Parser<'_>) -> bool {
        self.start.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        let span1 = p.peek_next_span();

        let mut items = vec![];
        p.parse(ctx, self.start)?;
        loop {
            // End the list or consume an item.
            if let Some(item) =
                parse_one_of!(p, ctx, [self.inner.map(Some), self.end.map(|_| None)])?
            {
                items.push(item); // There is an item.
            } else {
                break; // End of list; empty list, or trailing separator.
            }
            // End the list or consume a separator.
            if let Some(_) = parse_one_of!(p, ctx, [self.sep.map(Some), self.end.map(|_| None)])? {
                continue; // There is a separator.
            } else {
                break; // End of list, no trailing separator.
            }
        }

        let span2 = p.span();
        let span = span1.merge(span2);
        Ok(Spanned { node: items, span })
    }
}

/// Rule that matches a list of things surrounded by a symbol pair,
/// such as a comma-separated list enclosed in parentheses.
#[derive(Debug, Copy, Clone)]
pub struct TryFromToken<T>(PhantomData<T>, &'static str);
impl<T> TryFromToken<T> {
    pub const fn with_display(msg: &'static str) -> Self {
        Self(PhantomData, msg)
    }
}
impl<T> fmt::Display for TryFromToken<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}
impl<T: TryFrom<Token>> SyntaxRule for TryFromToken<T> {
    type Output = Spanned<T>;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next().and_then(|t| T::try_from(t).ok()).is_some()
    }
    fn consume_match(&self, p: &mut Parser<'_>, _ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.next()
            .and_then(|t| T::try_from(t).ok())
            .map(|t| Spanned {
                span: p.span(),
                node: t,
            })
            .ok_or_else(|| p.expected_err(self))
    }
}

/// Rule that matches no tokens; always succeeds (useful as a fallback when
/// matching multiple possible rules).
pub struct Epsilon;
impl_display!(for Epsilon, "nothing");
impl SyntaxRule for Epsilon {
    type Output = ();

    fn might_match(&self, _p: Parser<'_>) -> bool {
        true
    }
    fn consume_match(&self, _p: &mut Parser<'_>, _ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        Ok(())
    }
}

pub struct EndOfFile;
impl_display!(for EndOfFile, "end of file");
impl SyntaxRule for EndOfFile {
    type Output = ();

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next().is_none()
    }
    fn consume_match(&self, p: &mut Parser<'_>, _ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        match p.next() {
            Some(_) => Ok(()),
            None => p.expected(self),
        }
    }
}
