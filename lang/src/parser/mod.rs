//! Parser that turns tokens into an AST.
//!
//! Rather than generate a parse tree and then turn that into an AST, we
//! generate an AST directly from tokens. The only problem is that directives we
//! need to know all of the directives in the file before parsing the contents
//! of any of them. In order to resolve variables, we need to know their types,
//! and the types of many variables may depend on other directives. For example,
//! the type of the `nbhd` variable depends on the `@ndim` and `@neighborhood`
//! directives, *which may be absent*. If these directives are absent, we need
//! to assume their default values; if they are not we need to parse them before
//! parsing the transition function.
//!
//! The compromise I've implemented is this: initially, we split the list of
//! tokens before each directive, immediately parse enough of the directive to
//! know what might depend on it, and then lazily parse its body.

use codemap::{File, Span, Spanned};
use itertools::Itertools;

#[macro_use]
mod macros;
mod context;
pub mod rules;

use crate::ast;
use crate::errors::{Error, Result};
use crate::lexer::{self, Token};
pub use context::Ctx;
use rules::SyntaxRule;

/// Tokenize a file and split the token list by directive.
pub fn tokenize_and_split_directives(file: &File) -> Result<Vec<Vec<Spanned<Token>>>> {
    // Tokenize the file.
    let tokens = lexer::tokenize(file).filter(|t| !t.is_skip()).collect_vec();

    // Return an error if there is an invalid token.
    for token in &tokens {
        match token.node {
            Token::UnterminatedBlockComment => {
                return Err(Error::unterminated(token.span, "block comment"));
            }
            Token::UnterminatedStringLiteral => {
                return Err(Error::unterminated(token.span, "string literal"));
            }
            Token::Unknown => {
                return Err(Error::invalid_symbol(token.span));
            }
            _ => (),
        }
    }

    // Split the tokens list by directive.
    let mut directive_token_slices = vec![];
    let mut i = 0;
    let mut j = 0;
    while i < tokens.len() {
        // Find the beginning of the next directive. Directives are the only
        // tokens that start with `@`.
        while j < tokens.len() && !file.source_slice(tokens[j].span).starts_with('@') {
            j += 1;
        }
        // If this is not the empty span before the first `@`, save the
        // token slice.
        if j > 0 {
            directive_token_slices.push(tokens[i..j].to_vec());
        }
        i = j;
        j += 1;
    }

    Ok(directive_token_slices)
}

/// Token parser used to assemble an AST.
#[derive(Debug, Copy, Clone)]
pub struct Parser<'a> {
    /// Source file.
    file: &'a File,
    /// Tokens to feed.
    tokens: &'a [Spanned<Token>],
    /// Index of the "current" token (None = before start).
    cursor: Option<usize>,
}
impl<'a> Parser<'a> {
    /// Constructs a parser for a file.
    ///
    /// `tokens` must be from `file` and must be nonempty.
    pub fn new(file: &'a File, tokens: &'a [Spanned<Token>]) -> Result<Self> {
        if tokens.is_empty() {
            internal_error!("empty tokens for parser");
        }
        Ok(Self {
            file,
            tokens,
            cursor: None,
        })
    }

    /// Returns the span from the first token to the last token.
    pub fn total_span(self) -> Span {
        let span1 = self.tokens.first().unwrap().span;
        let span2 = self.tokens.last().unwrap().span;
        span1.merge(span2)
    }

    /// Returns the token at the cursor.
    pub fn current(self) -> Option<Token> {
        Some(self.tokens.get(self.cursor?)?.node)
    }
    /// Returns the span of the current token. If there is no current token,
    /// returns an empty span at the begining or end of the input appropriately.
    pub fn span(&self) -> Span {
        if let Some(idx) = self.cursor {
            if let Some(token) = self.tokens.get(idx) {
                // This is a token in the middle of the region.
                token.span
            } else {
                // This is the end of the region; return an empty span at the
                // end of the region.
                let len = self.total_span().len();
                self.file.span.subspan(len, len)
            }
        } else {
            // This is the beginning of the region; return an empty span at the
            // beginning of the region.
            self.file.span.subspan(0, 0)
        }
    }
    /// Returns the source string of the curent token. If there is no current
    /// tokens, returns an empty string.
    pub fn string(&self) -> &'a str {
        if self.current().is_some() {
            self.file.source_slice(self.span())
        } else {
            ""
        }
    }

    /// Moves the cursor forward without skipping whitespace/comments and then
    /// returns the token at the cursor.
    pub fn next_noskip(&mut self) -> Option<Token> {
        // Add 1 or set to zero.
        self.cursor = Some(self.cursor.map(|idx| idx + 1).unwrap_or(0));
        self.current()
    }
    /// Moves the cursor back without skipping whitespace/comments and then
    /// returns the token at the cursor.
    pub fn prev_noskip(&mut self) -> Option<Token> {
        // Subtract 1 if possible.
        self.cursor = self.cursor.and_then(|idx| idx.checked_sub(1));
        self.current()
    }
    /// Returns whether the current token would normally be skipped.
    pub fn is_skip(self) -> bool {
        if let Some(t) = self.current() {
            t.is_skip()
        } else {
            false
        }
    }

    /// Moves the cursor forward and then returns the token at the cursor.
    pub fn next(&mut self) -> Option<Token> {
        loop {
            self.next_noskip();
            if self.is_skip() {
                return self.current();
            }
        }
    }
    /// Moves the cursor back and then returns the token at the cursor.
    pub fn prev(&mut self) -> Option<Token> {
        loop {
            self.prev_noskip();
            if self.is_skip() {
                return self.current();
            }
        }
    }

    /// Returns the token after the one at the cursor, without mutably moving
    /// the cursor.
    pub fn peek_next(self) -> Option<Token> {
        let mut tmp = self;
        tmp.next()
    }
    // /// Returns the token before the one at the cursor, without mutably moving
    // /// the cursor.
    // pub fn peek_prev(self) -> Option<Token> {
    //     let mut tmp = self;
    //     tmp.prev()
    // }

    /// Returns the span of the token after the one at the cursor, without
    /// mutably moving the cursor.
    pub fn peek_next_span(self) -> Span {
        let mut tmp = self;
        tmp.next();
        tmp.span()
    }

    /// Attempts to apply a syntax rule starting at the cursor, returning an
    /// error if it fails. This should only be used when this syntax rule
    /// represents the only valid parse; if there are other options,
    /// `try_parse()` is preferred.
    pub fn parse<R: SyntaxRule>(&mut self, ctx: &mut Ctx<'_>, rule: R) -> Result<R::Output> {
        self.try_parse(ctx, &rule)
            .unwrap_or_else(|| self.expected(rule))
    }
    /// Applies a syntax rule starting at the cursor, returning `None` if the
    /// syntax rule definitely doesn't match (i.e. its `might_match()`
    /// implementation returned false).
    pub fn try_parse<R: SyntaxRule>(
        &mut self,
        ctx: &mut Ctx<'_>,
        rule: R,
    ) -> Option<Result<R::Output>> {
        rule.might_match(*self).then(|| {
            let old_state = *self; // Save state.
            let ret = rule.consume_match(self, ctx);
            if ret.is_err() {
                // Restore prior state on failure.
                *self = old_state;
            }
            ret
        })
    }

    pub fn parse_and_add_ast_node<'b, D>(
        &'b mut self,
        ctx: &'b mut Ctx<'_>,
        f: impl 'b + FnOnce(&mut Self, &mut Ctx<'_>) -> Result<D>,
    ) -> Result<ast::NodeId<ast::Node<D>>>
    where
        ast::Node<D>: ast::NodeTrait,
    {
        let span1 = self.peek_next_span();
        let node_data = f(self, ctx)?;
        let span2 = self.span();
        Ok(ctx.add_node(span1.merge(span2), node_data))
    }

    /// Returns an error describing that `expected` was expected.
    pub fn expected<T>(mut self, expected: impl ToString) -> Result<T> {
        // TODO: when #[feature(never_type)] stabalizes, use that here and
        // return Result<!>.
        Err(self.expected_err(expected))
    }

    /// Returns an error describing that `expected` was expected.
    pub fn expected_err(mut self, expected: impl ToString) -> Error {
        self.next();
        Error::expected(self.span(), expected.to_string())
    }
}
