use codemap::Spanned;
use std::sync::Arc;

use super::{Expression, List, Parser, SyntaxRule};
use crate::ast;
use crate::data::LangInt;
use crate::errors::{Error, Result};
use crate::lexer::Token;

/// Consumes an identifier.
#[derive(Debug, Copy, Clone)]
pub struct Identifier;
impl_display!(for Identifier, "identifier, such as a variable or function name");
impl SyntaxRule for Identifier {
    type Output = Spanned<Arc<String>>;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        matches!(p.next(), Some(Token::Ident) | Some(Token::Keyword(_)))
    }
    fn consume_match(
        &self,
        p: &mut Parser<'_>,
        _ast: &'_ mut ast::Program,
    ) -> Result<Self::Output> {
        match p.next() {
            Some(Token::Ident) => Ok(Spanned {
                span: p.span(),
                node: Arc::new(p.string().to_owned()),
            }),

            Some(Token::Keyword(_)) => Err(Error::reserved_word(p.span())),

            _ => p.expected(self),
        }
    }
}

/// Consumes a string literal.
#[derive(Debug, Copy, Clone)]
pub struct StringLiteral;
impl_display!(for StringLiteral, "string literal");
impl SyntaxRule for StringLiteral {
    type Output = Spanned<Arc<String>>;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::StringLiteral)
    }
    fn consume_match(
        &self,
        p: &mut Parser<'_>,
        _ast: &'_ mut ast::Program,
    ) -> Result<Self::Output> {
        if p.next() != Some(Token::StringLiteral) {
            return p.expected(self);
        }
        // Use IIFE for error handling.
        || -> Option<Self::Output> {
            let mut string_contents = String::new();
            let mut chars = p.string().chars().peekable();
            if matches!(chars.peek()?, 'a'..='z') {
                chars.next()?; // Skip prefix character.
            }
            let quote = chars.next()?;
            // Read characters.
            loop {
                match chars.next()? {
                    '\\' => string_contents.push(chars.next()?),
                    c if c == quote => break,
                    c => string_contents.push(c),
                }
            }

            if chars.next().is_none() {
                // End of token, as expected.
                Some(Spanned {
                    span: p.span(),
                    node: Arc::new(string_contents),
                })
            } else {
                // Why is there more after the closing quote?
                None
            }
        }()
        .ok_or_else(|| internal_error_value!("error in string literal parsing"))
    }
}

/// Consumes an integer literal.
#[derive(Debug, Copy, Clone)]
pub struct IntegerLiteral;
impl_display!(for IntegerLiteral, "integer literal, such as '42'");
impl SyntaxRule for IntegerLiteral {
    type Output = LangInt;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::IntegerLiteral)
    }
    fn consume_match(
        &self,
        p: &mut Parser<'_>,
        _ast: &'_ mut ast::Program,
    ) -> Result<Self::Output> {
        match p.next() {
            Some(Token::IntegerLiteral) => parse_int::parse::<LangInt>(p.string())
                .map_err(|e| Error::invalid_integer_literal(p.span(), e.to_string())),
            _ => p.expected(self),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct VectorLiteral;
impl_display!(for VectorLiteral, "vector literal ({})", List::bracket_comma_sep(Expression));
impl SyntaxRule for VectorLiteral {
    type Output = Spanned<Vec<ast::ExprId>>;

    fn might_match(&self, p: Parser<'_>) -> bool {
        List::bracket_comma_sep(Expression).might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, List::bracket_comma_sep(Expression))
    }
}
