use super::eaters::TokenEater;

use crate::errors::*;
use crate::lexer::*;
use crate::Span;
use LangErrorMsg::Expected;

/// Specialized iterator over tokens used to produce an untyped AST.
#[derive(Debug, Copy, Clone)]
pub struct TokenFeeder<'a> {
    /// Tokens to feed.
    tokens: &'a [Token<'a>],
    /// Index of the "current" token (None = before start).
    cursor: Option<usize>,
}
impl<'a> From<&'a [Token<'a>]> for TokenFeeder<'a> {
    fn from(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: None,
        }
    }
}
impl<'a> TokenFeeder<'a> {
    /// Moves the cursor forward and then returns the token at the cursor.
    pub fn next(&mut self) -> Option<Token<'a>> {
        // Add 1 or set to zero.
        self.cursor = Some(self.cursor.map(|idx| idx + 1).unwrap_or(0));
        self.current()
    }
    /// Moves the cursor back and then returns the token at the cursor.
    pub fn prev(&mut self) -> Option<Token<'a>> {
        // Subtract 1 if possible.
        self.cursor = self.cursor.and_then(|idx| idx.checked_sub(1));
        self.current()
    }
    /// Returns the token at the cursor.
    pub fn current(self) -> Option<Token<'a>> {
        self.cursor.and_then(|idx| self.tokens.get(idx).copied())
    }
    /// Returns the token after the one at the cursor, without mutably moving
    /// the cursor.
    pub fn peek_next(self) -> Option<Token<'a>> {
        let mut tmp = self;
        tmp.next()
    }

    /// Moves the cursor forward and then returns the TokenClass of the token at
    /// the cursor.
    pub fn next_class(&mut self) -> Option<TokenClass<'a>> {
        self.next().map(|t| t.class)
    }
    /// Returns the TokenClass of the token after the one at the cursor, without
    /// mutably moving the cursor.
    pub fn peek_next_class(self) -> Option<TokenClass<'a>> {
        self.peek_next().map(|t| t.class)
    }

    /// Returns the span of the current token. If there is no current token,
    /// returns an empty span at the begining or end of the input appropriately.
    pub fn span(&self) -> Span {
        if self.cursor.is_none() {
            // This is the beginning of the token stream; return an empty span
            // at the beginning of the file.
            Span::empty(0)
        } else {
            match self.current() {
                // This is the middle of the token stream.
                Some(t) => t.span,
                // This is the end of the token stream; return an empty span at
                // the end of the file.
                None => self
                    .tokens
                    .last()
                    .map(|t| Span::empty(t.span.end))
                    .unwrap_or(Span::empty(0)),
            }
        }
    }

    /// Attempts to "feed" tokens to the given "token eater" to produce an AST
    /// node. This is the same as `.try_feed()` except that it returns
    /// `Err(Expected(_))` instead of `None`, so it should be used when this
    /// token eater represents the only possible parse.
    pub fn feed<E: TokenEater>(&mut self, eater: E) -> LangResult<E::Output> {
        self.try_feed(&eater)
            .unwrap_or_else(|| self.expected(eater))
    }
    /// Attepmts to "feed" tokens to the given "token eater" to produce an AST
    /// node.
    ///
    /// Returns `None` if there is no way the token eater will match (i.e. its
    /// `.might_match()` method returned `false`) or `Some(_)` containing the
    /// result of the token eater's `.eat()` method.
    pub fn try_feed<E: TokenEater>(&mut self, eater: E) -> Option<LangResult<E::Output>> {
        if eater.might_match(*self) {
            let old_state = *self;
            let ret = eater.eat(self);
            if ret.is_err() {
                // Restore previous state on failure.
                *self = old_state;
            }
            Some(ret)
        } else {
            None
        }
    }

    /// Returns an `Err(Expected(_))` containing a string describing the given
    /// value.
    pub fn expected<T>(mut self, expected: impl ToString) -> LangResult<T> {
        // TODO: when #[feature(never_type)] stabalizes, use that here and
        // return LangResult<!>.
        self.next();
        self.err(Expected(expected.to_string()))
    }
    /// Attaches the span of the current token to the given error message and
    /// returns it as a `LangResult`.
    pub fn err<T>(self, msg: LangErrorMsg) -> LangResult<T> {
        // TODO: when #[feature(never_type)] stabalizes, use that here and
        // return LangResult<!>.
        Err(self.error(msg))
    }
    /// Attaches the span of the current token to the given error message.
    pub fn error(self, msg: LangErrorMsg) -> LangError {
        msg.with_span(self.span())
    }
}
