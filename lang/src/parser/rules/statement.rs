use super::{Expression, Identifier, Parser, StringLiteral, SyntaxRule, TryFromToken};
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            if p.next() != Some(Token::LBrace) {
                return p.expected(self);
            }
            let statements = std::iter::from_fn(|| {
                parse_one_of!(p, ast, [Statement.map(Some), Token::RBracket.map(|_| None)])
                    .transpose() // Result<Option<T>> -> Option<Result<T>>
            })
            .collect::<Result<Vec<ast::StmtId>>>()?;
            Ok(ast::StmtData::Block(statements))
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        if let Some(result) = p.try_parse(ast, AssignStatement) {
            return result;
        }

        match p.next() {
            // Loops
            Some(Token::KeywordBreak) => {
                p.parse_and_add_ast_node(ast, |_, _| Ok(ast::StmtData::Break))
            }
            Some(Token::KeywordContinue) => {
                p.parse_and_add_ast_node(ast, |_, _| Ok(ast::StmtData::Continue))
            }
            Some(Token::KeywordFor) => {
                p.prev();
                p.parse(ast, ForLoop)
            }

            // Returns
            Some(Token::KeywordBecome) => p.parse_and_add_ast_node(ast, |p, ast| {
                Ok(ast::StmtData::Become(p.parse(ast, Expression)?))
            }),
            Some(Token::KeywordRemain) => {
                p.parse_and_add_ast_node(ast, |_, _| Ok(ast::StmtData::Remain))
            }
            Some(Token::KeywordReturn) => p.parse_and_add_ast_node(ast, |p, ast| {
                Ok(ast::StmtData::Return(
                    p.try_parse(ast, Expression).transpose()?,
                ))
            }),

            // Branching
            Some(Token::KeywordIf) => p.parse(ast, IfStatement),
            Some(Token::KeywordElse) => Err(Error::else_without_if(p.span())),
            Some(Token::KeywordUnless) => p.parse_and_add_ast_node(ast, |p, ast| {
                Ok(ast::StmtData::IfElse {
                    condition: p.parse(ast, Expression)?,
                    if_true: None,
                    if_false: Some(p.parse(ast, StatementBlock)?),
                })
            }),

            Some(Token::KeywordCase) => Err(Error::unimplemented(p.span())),
            Some(Token::KeywordMatch) => Err(Error::unimplemented(p.span())),

            // Debugging
            Some(Token::KeywordAssert) => p.parse_and_add_ast_node(ast, |p, ast| {
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
            Some(Token::KeywordError) => p.parse_and_add_ast_node(ast, |p, ast| {
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, Token::KeywordFor)?;

        let iter_var = p.parse(ast, Identifier)?;
        p.parse(ast, Token::KeywordIn)?;
        let iter_expr = p.parse(ast, Expression)?;
        let block = p.parse(ast, StatementBlock)?;

        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::StmtData::ForLoop {
                iter_var,
                iter_expr,
                block,
            })
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::StmtData::IfElse {
                condition: p.parse(ast, Expression)?,
                if_true: Some(p.parse(ast, StatementBlock)?),
                if_false: p.try_parse(ast, ElseStatement).transpose()?,
            })
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse(ast, Token::KeywordElse)?;
        parse_one_of!(p, ast, [IfStatement, StatementBlock])
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
    fn consume_match(&self, p: &mut Parser<'_>, ast: &'_ mut ast::Program) -> Result<Self::Output> {
        p.parse_and_add_ast_node(ast, |p, ast| {
            Ok(ast::StmtData::Assign {
                lhs: p.parse(ast, Expression)?,
                op: p.parse(
                    ast,
                    TryFromToken::<ast::AssignOp>::with_display(
                        "assignment symbol, such as '=' or '+='",
                    ),
                )?,
                rhs: p.parse(ast, Expression)?,
            })
        })
    }
}
