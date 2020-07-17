use itertools::Itertools;
use std::convert::TryInto;
use std::sync::Arc;

use super::*;
use crate::lexer::*;
use crate::parser::tree;
use crate::types::StencilCell;
use LangErrorMsg::{CellStateOutOfRange, ReservedWord};

/// Consumes a stencil literal.
#[derive(Debug, Copy, Clone)]
pub struct StencilLiteral;
impl_display!(StencilLiteral, "stencil literal");
impl TokenEater for StencilLiteral {
    type Output = tree::Expr;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Punctuation(PunctuationToken::Backtick))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        tf.feed(PunctuationToken::Backtick)?;
        let mut cells = vec![];
        let mut bindings = vec![];
        enum Either {
            Cells(Vec<StencilCell>),
            Bindings(Vec<tree::StencilBinding>),
            Done,
        }
        loop {
            match feed_one_of!(
                tf,
                [
                    StencilCells.map(Either::Cells),
                    StencilBindings.map(Either::Bindings),
                    PunctuationToken::Backtick.map(|_| Either::Done),
                ]
            )? {
                Either::Cells(c) => cells.extend_from_slice(&c),
                Either::Bindings(b) => bindings.extend_from_slice(&b),
                Either::Done => return Ok(tree::Expr::Stencil { cells, bindings }),
            }
        }
    }
}

/// Consumes one or more cells in a stencil literal.
#[derive(Debug, Copy, Clone)]
struct StencilCells;
impl_display!(
    StencilCells,
    "identifier, number, or symbol representing a cell state filter"
);
impl TokenEater for StencilCells {
    type Output = Vec<StencilCell>;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        match tf.peek_next_class() {
            None => false,
            Some(TokenClass::Keyword(_)) => false,
            Some(TokenClass::String { .. }) => false,
            Some(_) => true,
        }
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            None => tf.expected(self),
            Some(TokenClass::Keyword(_)) => tf.err(ReservedWord),
            Some(TokenClass::String { .. }) => tf.expected(self),
            Some(TokenClass::Ident(s)) => Ok(vec![StencilCell::Ident(Arc::new(s.to_owned()))]),
            Some(TokenClass::Integer(i)) => Ok(vec![StencilCell::Number(
                i.try_into().map_err(|_| tf.error(CellStateOutOfRange))?,
            )]),
            Some(_) => Ok(tf
                .current()
                .unwrap()
                .string
                .chars()
                .map(StencilCell::Other)
                .collect()),
        }
    }
}

/// Consumes exactly one cell in a stencil literal.
#[derive(Debug, Copy, Clone)]
struct QuotedStencilCell;
impl_display!(
    QuotedStencilCell,
    "identifier or symbol representing a cell state filter (symbols must be quoted with single quotes)"
);
impl TokenEater for QuotedStencilCell {
    type Output = StencilCell;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Ident(_) | TokenClass::String { .. })
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            None => tf.expected(self),
            Some(TokenClass::Ident(s)) => Ok(StencilCell::Ident(Arc::new(s.to_owned()))),
            Some(TokenClass::String {
                prefix: None,
                quote: '\'',
                contents,
            }) => {
                match contents.chars().exactly_one() {
                    Err(_) => tf.expected("exactly one character in the string"),
                    // These would be valid identifiers, so they should not
                    // be quoted.
                    Ok('A'..='Z') | Ok('a'..='z') | Ok('_') => tf.expected(self),
                    // These would be valid integers, so they should not be
                    // quoted (and cannot be overridden anyway).
                    Ok('0'..='9') => tf.expected(self),
                    Ok(other) => Ok(StencilCell::Other(other)),
                }
            }
            _ => tf.expected(self),
        }
    }
}

/// Consumes a "where" clause in a stencil literal.
#[derive(Debug, Copy, Clone)]
struct StencilBindings;
impl_display!(StencilBindings, "'{}' clause", KeywordToken::Where);
impl TokenEater for StencilBindings {
    type Output = Vec<tree::StencilBinding>;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Keyword(KeywordToken::Where))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        tf.feed(KeywordToken::Where)?;
        let mut ret = vec![];
        loop {
            let cell = tf.feed(QuotedStencilCell.spanned())?;
            tf.feed(KeywordToken::Is)?;
            let same = tf.try_feed(KeywordToken::Same).transpose()?.is_some();
            let expr = Box::new(tf.feed(ExprWithPrecedence(OpPrecedence::Is.next()))?);
            ret.push(tree::StencilBinding { cell, same, expr });
            if tf.try_feed(KeywordToken::And).transpose()?.is_none() {
                return Ok(ret);
            }
        }
    }
}
