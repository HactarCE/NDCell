use itertools::Itertools;

use super::{Expression, Identifier, Parser, StringLiteral, SyntaxRule, TryFromToken};
use crate::ast;
use crate::errors::{Error, Result};
use crate::lexer::{Keyword, Token};

/// Matches a block of statements, surrounded by curly braces.
#[derive(Debug, Copy, Clone)]
pub struct StatementBlock;
impl_display!(for StatementBlock, "block of code surrounded by '{{' and '}}'");
impl SyntaxRule for StatementBlock {
    type Output = ast::StmtId;

    fn prefix_matches(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::LBrace)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            if p.next() != Some(Token::LBrace) {
                return p.expected(self);
            }
            let statements = std::iter::from_fn(|| {
                parse_one_of!(p, ast, [Statement.map(Some), Token::RBrace.map(|_| None)])
                    .transpose() // Result<Option<T>> -> Option<Result<T>>
            })
            .collect::<Result<Vec<ast::StmtId>>>()?;
            Ok(ast::StmtData::Block(statements))
        })
    }
}

/// Matches a statement.
#[derive(Debug, Copy, Clone)]
pub struct Statement;
impl_display!(
    for Statement,
    "statement starting with {} or assignment such as 'x = y'",
    crate::utils::join_with_conjunction("or", Keyword::STATEMENT_STARTERS),
);
impl SyntaxRule for Statement {
    type Output = ast::StmtId;

    fn prefix_matches(&self, mut p: Parser<'_>) -> bool {
        StatementBlock.prefix_matches(p)
            || AssignStatement.prefix_matches(p)
            || matches!(p.next(), Some(Token::Keyword(kw)) if kw.starts_statement())
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        None.or_else(|| p.try_parse(ast, StatementBlock))
            .or_else(|| p.try_parse(ast, AssignStatement))
            .unwrap_or_else(|| {
                if let Some(Token::Keyword(kw)) = p.next() {
                    match kw {
                        // Loops
                        Keyword::Break => {
                            p.parse_and_add_ast_node(ast, |_, _| Ok(ast::StmtData::Break))
                        }
                        Keyword::Continue => {
                            p.parse_and_add_ast_node(ast, |_, _| Ok(ast::StmtData::Continue))
                        }
                        Keyword::For => {
                            p.prev();
                            p.parse(ast, ForLoop)
                        }

                        // Returns
                        Keyword::Become => p.parse_and_add_ast_node(ast, |p, ast| {
                            Ok(ast::StmtData::Become(p.parse(ast, Expression)?))
                        }),
                        Keyword::Remain => {
                            p.parse_and_add_ast_node(ast, |_, _| Ok(ast::StmtData::Remain))
                        }
                        Keyword::Return => p.parse_and_add_ast_node(ast, |p, ast| {
                            Ok(ast::StmtData::Return(
                                p.try_parse(ast, Expression).transpose()?,
                            ))
                        }),

                        // Branching
                        Keyword::If => {
                            p.prev();
                            p.parse(ast, IfStatement)
                        }
                        Keyword::Else => Err(Error::else_without_if(p.span())),
                        Keyword::Unless => p.parse_and_add_ast_node(ast, |p, ast| {
                            Ok(ast::StmtData::IfElse {
                                condition: p.parse(ast, Expression)?,
                                if_true: None,
                                if_false: Some(p.parse(ast, StatementBlock)?),
                            })
                        }),

                        Keyword::Case => Err(Error::unimplemented(p.span())),
                        Keyword::Match => Err(Error::unimplemented(p.span())),

                        // Debugging
                        Keyword::Assert => p.parse_and_add_ast_node(ast, |p, ast| {
                            Ok(ast::StmtData::Assert {
                                condition: p.parse(ast, Expression)?,
                                msg: if p.peek_next() == Some(Token::Comma) {
                                    p.next();
                                    Some(p.parse(ast, StringLiteral)?)
                                } else {
                                    None
                                },
                            })
                        }),
                        Keyword::Error => p.parse_and_add_ast_node(ast, |p, ast| {
                            Ok(ast::StmtData::Error {
                                msg: if p.peek_next() == Some(Token::Comma) {
                                    p.next();
                                    Some(p.parse(ast, StringLiteral)?)
                                } else {
                                    None
                                },
                            })
                        }),

                        _ => p.expected(self),
                    }
                } else {
                    p.expected(self)
                }
            })
    }
}

/// Matches a `for` loop.
#[derive(Debug, Copy, Clone)]
struct ForLoop;
impl_display!(for ForLoop, "{} loop", Token::Keyword(Keyword::For));
impl SyntaxRule for ForLoop {
    type Output = ast::StmtId;

    fn prefix_matches(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Keyword(Keyword::For))
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, Token::Keyword(Keyword::For))?;

        let iter_var = p.parse(ast, Identifier)?;
        p.parse(ast, Token::Keyword(Keyword::In))?;
        let iter_expr = p.parse(ast, Expression)?;
        let block = p.parse(ast, StatementBlock)?;

        p.parse_and_add_ast_node(ast, |_p, _ast| {
            Ok(ast::StmtData::ForLoop {
                iter_var,
                iter_expr,
                block,
            })
        })
    }
}

/// Matches an `if` statement.
#[derive(Debug, Copy, Clone)]
struct IfStatement;
impl_display!(for IfStatement, "{} statement", Token::Keyword(Keyword::If));
impl SyntaxRule for IfStatement {
    type Output = ast::StmtId;

    fn prefix_matches(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Keyword(Keyword::If))
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, Token::Keyword(Keyword::If))?;
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::StmtData::IfElse {
                condition: p.parse(ast, Expression)?,
                if_true: Some(p.parse(ast, StatementBlock)?),
                if_false: p.try_parse(ast, ElseStatement).transpose()?,
            })
        })
    }
}

/// Matches an `else` block or `else if` statement.
#[derive(Debug, Copy, Clone)]
struct ElseStatement;
impl_display!(for ElseStatement, "{} statement", Token::Keyword(Keyword::Else));
impl SyntaxRule for ElseStatement {
    type Output = ast::StmtId;

    fn prefix_matches(&self, mut p: Parser<'_>) -> bool {
        p.next() == Some(Token::Keyword(Keyword::Else))
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, Token::Keyword(Keyword::Else))?;
        parse_one_of!(p, ast, [IfStatement, StatementBlock])
    }
}

/// Matches a variable assignment statement.
#[derive(Debug, Copy, Clone)]
struct AssignStatement;
impl_display!(for AssignStatement, "assignment statement");
impl SyntaxRule for AssignStatement {
    type Output = ast::StmtId;

    fn prefix_matches(&self, p: Parser<'_>) -> bool {
        Expression.prefix_matches(p)
    }
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        let old_p = *p;
        p.parse_and_add_ast_node(ast, |p, ast| {
            let lhs = p.parse(ast, Expression)?;

            let op = p
                .try_parse(
                    ast,
                    TryFromToken::<Option<ast::AssignOp>>::with_display(
                        "assignment symbol, such as '=' or '+='",
                    ),
                )
                .unwrap_or_else(|| {
                    // There's no assignment symbol, so the user probably
                    // just wrote an expression on a line. We should tell
                    // them that we expect a statement.
                    old_p.expected(Statement)
                })?;

            let mut rhs = p.parse(ast, Expression)?;

            // Desugar assignment operator.
            if let Some(op) = op {
                let lhs_span = ast.get_node(lhs).span();
                let rhs_span = ast.get_node(rhs).span();
                rhs = ast.add_node(
                    lhs_span.merge(rhs_span),
                    ast::ExprData::BinaryOp(lhs, op, rhs),
                );
            }

            Ok(ast::StmtData::Assign { lhs, rhs })
        })
    }
}
