use std::convert::TryFrom;
use std::sync::Arc;

use super::*;
use crate::lexer::*;
use crate::parser::tree;
use crate::Spanned;
use ErrorKind::InvalidDirectiveName;

/// Consumes directives until the end of the input.
#[derive(Debug, Copy, Clone)]
pub struct Directives;
impl_display!(Directives, "directives");
impl TokenEater for Directives {
    type Output = Vec<Spanned<(tree::Directive, tree::DirectiveContents)>>;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        Directive.might_match(tf)
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        std::iter::from_fn(|| {
            if tf.peek_next().is_some() {
                Some(tf.feed(Directive.spanned()))
            } else {
                None
            }
        })
        .collect()
    }
}

/// Consumes a directive.
#[derive(Debug, Copy, Clone)]
pub struct Directive;
impl_display!(Directive, "directive");
impl TokenEater for Directive {
    type Output = (tree::Directive, tree::DirectiveContents);
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Directive(_))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        match tf.next_class() {
            Some(TokenClass::Directive(directive_name)) => {
                let directive = tree::Directive::try_from(directive_name)
                    .map_err(|_| tf.error(InvalidDirectiveName))?;
                let contents = match directive {
                    tree::Directive::Transition => tf.feed(StatementBlock.spanned())?.into(),
                    tree::Directive::States => tf.feed(Expr)?.into(),
                    tree::Directive::Dimensions => tf.feed(Expr)?.into(),
                    tree::Directive::Function => tf.feed(FunctionDefinition)?.into(),
                };
                Ok((directive, contents))
            }
            _ => tf.expected(self),
        }
    }
}

/// Consumes a function definition.
#[derive(Debug, Copy, Clone)]
pub struct FunctionDefinition;
impl_display!(FunctionDefinition, "function definition");
impl TokenEater for FunctionDefinition {
    type Output = tree::HelperFunc;
    fn might_match(&self, _tf: TokenFeeder<'_>) -> bool {
        // It's better to give an error saying "expected type name" with
        // examples of valid type names than just "expected function definition"
        // if the user gives a bad return type.
        true
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        Ok(tree::HelperFunc {
            return_type: tf.feed(TypeName.spanned())?,
            name: tf.feed(Identifier.spanned())?,
            params: tf.feed(List::paren_comma_sep(FunctionParameter.spanned()))?,
            body: tf.feed(StatementBlock.spanned())?,
        })
    }
}

/// Consumes a function parameter (type and identifier).
#[derive(Debug, Copy, Clone)]
pub struct FunctionParameter;
impl_display!(
    FunctionParameter,
    "function parameter (type followed by parameter name)",
);
impl TokenEater for FunctionParameter {
    type Output = (Spanned<TypeToken>, Spanned<Arc<String>>);
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        TypeName.might_match(tf)
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        Ok((tf.feed(TypeName.spanned())?, tf.feed(Identifier.spanned())?))
    }
}
