use codemap::Spanned;
use std::sync::Arc;

use super::{Expression, Identifier, List, Parser, StatementBlock, SyntaxRule};
use crate::ast;
use crate::errors::{Error, Result};
use crate::lexer::Token;
use crate::LangMode;

/// Matches a directive.
#[derive(Debug, Copy, Clone)]
pub struct Directive;
impl_display!(for Directive, "directive, such as '@ndim 2'");
impl SyntaxRule for Directive {
    type Output = ast::DirectiveId;

    fn prefix_matches(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Ident) && p.string().starts_with('@')
    }

    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            if !(p.next() == Some(Token::Ident) && p.string().starts_with('@')) {
                return p.expected(self);
            }
            match p.string() {
                "@init" => Ok(ast::DirectiveData::Init {
                    mode: p.mode,
                    body: p.parse(ast, StatementBlock)?,
                }),

                "@func" => {
                    let name = p.parse(ast, Identifier).or_else(|_| {
                        p.expected(
                            "function definition, such as 'add(a: Integer, b: Integer) -> Integer",
                        )
                    })?;
                    let params = p.parse(ast, List::paren_comma_sep(FunctionParam))?.node;
                    p.parse(ast, Token::Arrow)?;
                    let ret_type = p.parse(ast, Expression)?;
                    let body = p.parse(ast, StatementBlock)?;

                    Ok(ast::DirectiveData::Function {
                        mode: p.mode,
                        name,
                        params,
                        ret_type,
                        body,
                    })
                }
                "@transition" => Ok(ast::DirectiveData::Transition(
                    p.parse(ast, StatementBlock)?,
                )),
                "@compile" if p.mode == LangMode::Internal => Ok(ast::DirectiveData::Compile {
                    param_types: p.parse(ast, List::bracket_comma_sep(Expression))?,
                    body: p.parse(ast, StatementBlock)?,
                }),

                // "@rule" => Ok(ast::DirectiveData::Rule(p.parse(ast, StringLiteral)?)),
                "@rule" => Err(Error::unimplemented(p.span())),
                "@ndim" => Ok(ast::DirectiveData::Ndim(p.parse(ast, Expression)?)),
                "@states" => Ok(ast::DirectiveData::States(p.parse(ast, Expression)?)),

                s => Err(match s.to_ascii_lowercase().as_str() {
                    "@init" | "@initialize" | "@initialization" | "@pre" | "@setup" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@init")
                    }
                    "@con" | "@const" | "@constant" | "@define" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@const")
                    }
                    "@def" | "@fn" | "@fun" | "@func" | "@function" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@func")
                    }

                    "@name" | "@rule" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@rule")
                    }
                    "@dim" | "@dimension" | "@dimensions" | "@ndim" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@ndim")
                    }
                    "@state" | "@states" | "@nstates" | "@n_states" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@states")
                    }
                    "@table" | "@tran" | "@trans" | "@transition" | "@transitions" => {
                        Error::invalid_directive_name_with_suggestion(p.span(), "@transition")
                    }

                    _ => Error::invalid_directive_name(p.span()),
                }),
            }
        })
    }
}

/// Matches a function parameter (name followed by type annotation).
#[derive(Debug, Copy, Clone)]
pub struct FunctionParam;
impl_display!(
    for FunctionParam,
    "function parameter, such as 'arg_name: Integer'",
);
impl SyntaxRule for FunctionParam {
    type Output = (Spanned<Arc<String>>, ast::ExprId);

    fn prefix_matches(&self, p: Parser<'_>) -> bool {
        Identifier.prefix_matches(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        let name = p.parse(ast, Identifier)?;
        if p.next() != Some(Token::Colon) {
            return p.expected("type annotation, such as ': Integer'")?;
        }
        let type_expr = p.parse(ast, Expression)?;
        Ok((name, type_expr))
    }
}
