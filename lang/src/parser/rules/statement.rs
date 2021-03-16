use super::{Ctx, Expression, Identifier, Parser, StringLiteral, SyntaxRule, TryFromToken};
use crate::ast;
use crate::errors::{Error, Result};
use crate::lexer::Token;

/// Consumes a block of statements, surrounded by curly braces.
#[derive(Debug, Copy, Clone)]
pub struct StatementBlock;
impl_display!(for StatementBlock, "block of code surrounded by '{{' and '}}'");
impl SyntaxRule for StatementBlock {
    type Output = ast::StmtId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Statement.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            ctx.in_new_scope(|ctx| {
                if p.next() != Some(Token::LBrace) {
                    return p.expected(self);
                }
                let statements = std::iter::from_fn(|| {
                    parse_one_of!(p, ctx, [Statement.map(Some), Token::RBracket.map(|_| None)])
                        .transpose() // Result<Option<T>> -> Option<Result<T>>
                })
                .collect::<Result<Vec<ast::StmtId>>>()?;
                Ok(ast::StmtData::Block(statements))
            })
        })
    }
}

/// Consumes a statement.
#[derive(Debug, Copy, Clone)]
pub struct Statement;
impl_display!(
    for Statement,
    "statement starting with {}",
    crate::utils::join_with_conjunction("or", Token::STATEMENT_KEYWORDS),
);
impl SyntaxRule for Statement {
    type Output = ast::StmtId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        AssignStatement.might_match(p)
            || p.next()
                .filter(|t| Token::STATEMENT_KEYWORDS.contains(t))
                .is_some()
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        if let Some(result) = p.try_parse(ctx, AssignStatement) {
            return result;
        }

        match p.next() {
            // Loops
            Some(Token::KeywordBreak) => {
                p.parse_and_add_ast_node(ctx, |_, _| Ok(ast::StmtData::Break))
            }
            Some(Token::KeywordContinue) => {
                p.parse_and_add_ast_node(ctx, |_, _| Ok(ast::StmtData::Continue))
            }
            Some(Token::KeywordFor) => p.parse(ctx, ForLoop),

            // Returns
            Some(Token::KeywordBecome) => {
                if ctx.dir_key() == Some(&ast::DirectiveKey::Transition) {
                    p.parse_and_add_ast_node(ctx, |p, ctx| {
                        Ok(ast::StmtData::Return(p.parse(ctx, Expression)?))
                    })
                } else {
                    Err(Error::become_or_remain_in_helper_fn(p.span()))
                }
            }
            Some(Token::KeywordRemain) => {
                if ctx.dir_key() == Some(&ast::DirectiveKey::Transition) {
                    Err(Error::unimplemented(p.span()))
                } else {
                    Err(Error::become_or_remain_in_helper_fn(p.span()))
                }
            }
            Some(Token::KeywordReturn) => {
                if ctx.dir_key() != Some(&ast::DirectiveKey::Transition) {
                    p.parse_and_add_ast_node(ctx, |p, ctx| {
                        Ok(ast::StmtData::Return(p.parse(ctx, Expression)?))
                    })
                } else {
                    Err(Error::become_or_remain_in_helper_fn(p.span()))
                }
            }

            // Branching
            Some(Token::KeywordIf) => p.parse(ctx, IfStatement),
            Some(Token::KeywordElse) => Err(Error::else_without_if(p.span())),
            Some(Token::KeywordUnless) => p.parse_and_add_ast_node(ctx, |p, ctx| {
                Ok(ast::StmtData::IfElse(
                    p.parse(ctx, Expression)?,
                    None,
                    Some(p.parse(ctx, StatementBlock)?),
                ))
            }),

            Some(Token::KeywordCase) => Err(Error::unimplemented(p.span())),
            Some(Token::KeywordMatch) => Err(Error::unimplemented(p.span())),

            // Debugging
            Some(Token::KeywordAssert) => p.parse_and_add_ast_node(ctx, |p, ctx| {
                Ok(ast::StmtData::Assert(
                    p.parse(ctx, Expression)?,
                    if p.peek_next() == Some(Token::Comma) {
                        p.next();
                        Some(p.parse(ctx, StringLiteral)?)
                    } else {
                        None
                    },
                ))
            }),
            Some(Token::KeywordError) => p.parse_and_add_ast_node(ctx, |p, ctx| {
                Ok(ast::StmtData::Error(
                    if p.peek_next() == Some(Token::Comma) {
                        p.next();
                        Some(p.parse(ctx, StringLiteral)?)
                    } else {
                        None
                    },
                ))
            }),

            _ => p.expected(self),
        }
    }
}

/// Consumes a `for` loop.
#[derive(Debug, Copy, Clone)]
struct ForLoop;
impl_display!(for ForLoop, "{} loop", Token::KeywordFor);
impl SyntaxRule for ForLoop {
    type Output = ast::StmtId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::KeywordFor)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            p.parse(ctx, Token::KeywordFor)?;

            let var_name = p.parse(ctx, Identifier)?;
            p.parse(ctx, Token::KeywordIn)?;
            let iter_expr = p.parse(ctx, Expression)?;
            let block = p.parse(ctx, StatementBlock)?;
            let var = ctx.add_scoped_var(var_name, None, None, Some(iter_expr));

            Ok(ast::StmtData::ForLoop(var, iter_expr, block))
        })
    }
}

/// Consumes an `if` statement.
#[derive(Debug, Copy, Clone)]
struct IfStatement;
impl_display!(for IfStatement, "{} statement", Token::KeywordIf);
impl SyntaxRule for IfStatement {
    type Output = ast::StmtId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::KeywordIf)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            Ok(ast::StmtData::IfElse(
                p.parse(ctx, Expression)?,
                Some(p.parse(ctx, StatementBlock)?),
                p.try_parse(ctx, ElseStatement).transpose()?,
            ))
        })
    }
}

/// Consumes an `else` block or `else if` statement.
#[derive(Debug, Copy, Clone)]
struct ElseStatement;
impl_display!(for ElseStatement, "{} statement", Token::KeywordElse);
impl SyntaxRule for ElseStatement {
    type Output = ast::StmtId;

    fn might_match(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::KeywordElse)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse(ctx, Token::KeywordElse)?;
        parse_one_of!(p, ctx, [IfStatement, StatementBlock])
    }
}

/// Consumes a variable assignment statement.
#[derive(Debug, Copy, Clone)]
struct AssignStatement;
impl_display!(for AssignStatement, "assignment statement");
impl SyntaxRule for AssignStatement {
    type Output = ast::StmtId;

    fn might_match(&self, p: Parser<'_>) -> bool {
        Expression.might_match(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ctx, |p, ctx| {
            Ok(ast::StmtData::Assign(
                p.parse(ctx, Expression)?,
                p.parse(
                    ctx,
                    TryFromToken::<ast::AssignOp>::with_display(
                        "assignment symbol, such as '=' or '+='",
                    ),
                )?,
                p.parse(ctx, Expression)?,
            ))
        })
    }
}
