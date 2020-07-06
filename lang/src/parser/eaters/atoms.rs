use super::*;
use crate::lexer::*;
use crate::parser::tree;
use crate::types::LangInt;

// Implement the TokenEater trait for some TokenClass categories. TODO: once
// #![feature(specialization)] stabalizes, try using that here instead.
impl_tokeneater_for_tokenclass!(KeywordToken);
impl_tokeneater_for_tokenclass!(OperatorToken);
impl_tokeneater_for_tokenclass!(PunctuationToken);

/// Consumes a type name.
#[derive(Debug, Copy, Clone)]
pub struct TypeName;
impl_display!(
    TypeName,
    "type name (e.g. Int, Cell, Vec, Vec1..256 -- capitalization matters)",
);
impl TokenEater for TypeName {
    type Output = TypeToken;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Type(_))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::Type(ty)) => Ok(ty),
            _ => tf.expected(self),
        }
    }
}

/// Consumes an identifier
#[derive(Debug, Copy, Clone)]
pub struct Identifier;
impl_display!(Identifier, "identifier (variable or function name)");
impl TokenEater for Identifier {
    type Output = String;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Ident(_))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::Ident(s)) => Ok(s.to_owned()),
            _ => tf.expected(self),
        }
    }
}

/// Consumes a tag.
#[derive(Debug, Copy, Clone)]
pub struct Tag;
impl_display!(Tag, "tag");
impl TokenEater for Tag {
    type Output = String;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Tag(_))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::Tag(s)) => Ok(s.to_owned()),
            _ => tf.expected(self),
        }
    }
}

/// Consumes an assignment symbol.
#[derive(Debug, Copy, Clone)]
pub struct AssignOp;
impl_display!(AssignOp, "assignment symbol (e.g. '=' or '+=')");
impl TokenEater for AssignOp {
    type Output = AssignmentToken;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Assignment(_))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::Assignment(assign_op)) => Ok(assign_op),
            _ => tf.expected(self),
        }
    }
}

/// Consumes a string literal.
#[derive(Debug, Copy, Clone)]
pub struct StringLiteral;
impl_display!(StringLiteral, "string literal");
impl TokenEater for StringLiteral {
    type Output = tree::StringLiteral;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::String{..})
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::String {
                prefix,
                quote,
                contents,
            }) => Ok(tree::StringLiteral {
                prefix,
                quote,
                contents: contents.to_owned(),
            }),
            _ => tf.expected(self),
        }
    }
}

/// Consumes an integer literal.
#[derive(Debug, Copy, Clone)]
pub struct IntLiteral;
impl_display!(IntLiteral, "integer literal");
impl TokenEater for IntLiteral {
    type Output = LangInt;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Integer(_))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::Integer(i)) => Ok(i),
            _ => tf.expected(self),
        }
    }
}
