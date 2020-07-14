use super::*;
use crate::lexer::*;
use crate::parser::tree;
use crate::types::StencilCell;

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
            Cell(StencilCell),
            Bindings(Vec<tree::StencilBinding>),
            Done,
        }
        loop {
            match feed_one_of!(
                tf,
                [
                    StencilChar.map(Either::Cell),
                    StencilBindings.map(Either::Bindings),
                    PunctuationToken::Backtick.map(|_| Either::Done),
                ]
            )? {
                Either::Cell(c) => cells.push(c),
                Either::Bindings(b) => bindings.extend_from_slice(&b),
                Either::Done => return Ok(tree::Expr::Stencil { cells, bindings }),
            }
        }
    }
}

/// Consumes a cell character in a stencil literal.
#[derive(Debug, Copy, Clone)]
struct StencilChar;
impl_display!(StencilChar, "character representing a cell state filter");
impl TokenEater for StencilChar {
    type Output = StencilCell;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(
            tf,
            TokenClass::Operator(OperatorToken::Tag)
            | TokenClass::Punctuation(PunctuationToken::Period)
            | TokenClass::Ident(_)
        )
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        feed_one_of!(
            tf,
            [
                OperatorToken::Tag.map(|_| StencilCell::Hashtag),
                PunctuationToken::Period.map(|_| StencilCell::Period),
                Identifier.map(StencilCell::Ident),
            ]
        )
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
            let cell = tf.feed(StencilChar.spanned())?;
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
