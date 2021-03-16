use super::{Ctx, Expression, Identifier, List, Parser, StatementBlock, SyntaxRule};
use crate::ast;
use crate::errors::{Error, Result};
use crate::lexer::Token;

/// Matches a directive, but does not parse its contents.
#[derive(Debug, Copy, Clone)]
pub struct DirectiveKey;
impl_display!(for DirectiveKey, "{}", Directive);
impl SyntaxRule for DirectiveKey {
    type Output = ast::DirectiveKey;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Ident) && p.string().starts_with('@')
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        if !(p.next() == Some(Token::Ident) && p.string().starts_with('@')) {
            return p.expected(self);
        }
        match p.string() {
            "@const" => Ok(ast::DirectiveKey::Const {
                name: p.parse(ctx, Identifier)?.node,
            }),
            "@func" => Ok(ast::DirectiveKey::Func {
                name: p.parse(ctx, Identifier)?.node,
            }),

            "@name" => Ok(ast::DirectiveKey::Name),
            "@ndim" => Ok(ast::DirectiveKey::Ndim),
            "@states" => Ok(ast::DirectiveKey::States),
            "@transition" => Ok(ast::DirectiveKey::Transition),

            s => Err(match s.to_ascii_lowercase().as_str() {
                "@con" | "@const" | "@constant" | "@define" => {
                    Error::invalid_directive_name_with_suggestion(p.span(), "@const")
                }
                "@def" | "@fn" | "@fun" | "@func" | "@function" => {
                    Error::invalid_directive_name_with_suggestion(p.span(), "@func")
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
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Directive;
impl_display!(for Directive, "directive, such as '@ndim 2'");
impl SyntaxRule for Directive {
    type Output = (ast::DirectiveKey, ast::DirectiveContents);

    fn might_match(&self, p: Parser<'_>) -> bool {
        DirectiveKey.might_match(p)
    }

    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        let key = p.parse(ctx, DirectiveKey)?;
        let contents = match key {
            ast::DirectiveKey::Name
            | ast::DirectiveKey::Const { .. }
            | ast::DirectiveKey::Ndim
            | ast::DirectiveKey::States => {
                let expr_id = p.parse(ctx, Expression)?;
                ctx.get_node(expr_id).const_eval().into()
            }

            ast::DirectiveKey::Func { name } => {
                todo!("stuff function definition into Value enum");
                // ast::DirectiveContents::Value(p.parse(ctx, FunctionDefinition)?)
            }
            ast::DirectiveKey::Transition => p.parse(ctx, StatementBlock)?.into(),
        };
        Ok((key, contents))
    }
}

/// Matches a function definition.
#[derive(Debug, Copy, Clone)]
pub struct FunctionDefinition;
impl_display!(
    for FunctionDefinition,
    "function definition, such as 'add(a: Integer, b: Integer) -> Integer'",
);
impl SyntaxRule for FunctionDefinition {
    type Output = ast::FuncId;

    fn might_match(&self, _tf: Parser<'_>) -> bool {
        // It's better to give an error saying "expected identifier" with
        // examples of valid type names than just "expected function definition"
        // if the user gives a bad return type.
        true
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            let name = p
                .parse(ctx, Identifier)
                .or_else(|_| p.expected("function name"))?;
            ctx.in_new_scope(|mut ctx| {
                let params = p.parse(ctx, List::paren_comma_sep(FunctionParameter))?.node;
                p.parse(ctx, Token::Arrow)?;
                let ret_type_expr = p.parse(ctx, Expression)?;
                let ret_type = todo!("const eval type expr");
                let body = p.parse(ctx, StatementBlock)?;

                Ok(ast::FuncData {
                    name,
                    params,
                    ret_type,
                    body,
                })
            })
        })
    }
}

/// Matches a function parameter (name followed by type annotation).
#[derive(Debug, Copy, Clone)]
pub struct FunctionParameter;
impl_display!(
    for FunctionParameter,
    "function parameter, such as 'arg_name: Integer'",
);
impl SyntaxRule for FunctionParameter {
    type Output = ast::VarId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Identifier.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        let name = p.parse(ctx, Identifier)?;
        if p.next() != Some(Token::Colon) {
            return p.expected("type annotation, such as ': Integer'")?;
        }
        let type_expr = p.parse(ctx, TypeExpression)?;
        Ok(ctx.add_scoped_var(name, Some(type_expr), None, None))
    }
}

/// Matches an expression that evaluates to a type.
#[derive(Debug, Copy, Clone)]
pub struct TypeExpression;
impl_display!(for TypeExpression, "type, such as 'Integer' or 'Vector[3]'",);
impl SyntaxRule for TypeExpression {
    type Output = ast::ExprId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Expression.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        Expression.consume_match(p, ctx)
    }
}
