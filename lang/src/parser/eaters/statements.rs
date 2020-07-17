use itertools::Itertools;
use std::fmt;

use super::*;
use crate::lexer::*;
use crate::parser::tree;
use LangErrorMsg::{ElseWithoutIf, Unimplemented};

/// Consumes a block of statements, surrounded by curly braces.
#[derive(Debug, Copy, Clone)]
pub struct StatementBlock;
impl_display!(StatementBlock, "code block");
impl TokenEater for StatementBlock {
    type Output = tree::StatementBlock;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Punctuation(PunctuationToken::LBrace))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        tf.feed(PunctuationToken::LBrace)?;
        std::iter::from_fn(|| {
            feed_one_of!(
                tf,
                [
                    PunctuationToken::RBrace.map(|_| None),
                    Statement.spanned().map(Some),
                ],
            )
            .transpose()
        })
        .collect()
    }
}

/// Consumes a statement.
#[derive(Debug, Copy, Clone)]
pub struct Statement;
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut keywords = KeywordToken::STATEMENT_STARTERS
            .iter()
            .map(|kw| kw.to_string())
            .collect_vec();
        keywords.sort();
        write!(
            f,
            "statement starting with {}",
            crate::utils::join_with_conjunction("or", &keywords)
        )
    }
}
impl TokenEater for Statement {
    type Output = tree::Statement;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Keyword(kw) if kw.starts_statement())
            || VarAssignStatement.might_match(tf)
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        use KeywordToken::*;
        match tf.next_class() {
            Some(TokenClass::Keyword(kw)) if kw.starts_statement() => match kw {
                // Loops
                For => Ok(tree::Statement::ForLoop {
                    var_expr: tf.feed(Expr)?,
                    iter_expr: {
                        tf.feed(KeywordToken::In)?;
                        tf.feed(Expr)?
                    },
                    block: tf.feed(StatementBlock)?,
                }),
                While => tf.err(Unimplemented),

                // Loop control
                Break => Ok(tree::Statement::Break),
                Continue => Ok(tree::Statement::Continue),

                // Returning values
                Become => Ok(tree::Statement::Become(tf.feed(Expr)?)),
                Remain => tf.err(Unimplemented),
                Return => Ok(tree::Statement::Return(tf.feed(Expr)?)),

                // Branching
                Case => tf.err(Unimplemented),
                Else => tf.err(ElseWithoutIf),
                If => {
                    tf.prev();
                    tf.feed(IfStatement)
                }
                Match => tf.err(Unimplemented),
                Unless => Ok(tree::Statement::If {
                    cond_expr: tf.feed(Expr)?,
                    if_true: vec![],
                    if_false: tf.feed(StatementBlock)?,
                }),

                // Debugging
                Assert => {
                    tf.prev();
                    tf.feed(AssertStatement)
                }
                Error => Ok(tree::Statement::Error {
                    msg: tf.try_feed(StringLiteral.spanned()).transpose()?,
                }),

                _ => internal_error!(
                    "Lexer says {:?} starts a statement, but parser doesn't know how to handle it",
                    kw
                ),
            },
            _ => {
                tf.prev();
                tf.try_feed(VarAssignStatement)
                    .unwrap_or_else(|| tf.expected(self))
            }
        }
    }
}

/// Consumes an `if` statement.
#[derive(Debug, Copy, Clone)]
struct IfStatement;
impl_display!(IfStatement, "if statement");
impl TokenEater for IfStatement {
    type Output = tree::Statement;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Keyword(KeywordToken::If))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        tf.feed(KeywordToken::If)?;
        Ok(tree::Statement::If {
            cond_expr: tf.feed(Expr)?,
            if_true: tf.feed(StatementBlock)?,
            if_false: feed_one_of!(tf, [ElseBlock, Epsilon.map(|_| vec![])])?,
        })
    }
}

/// Consumes an `else` block or `else if` statement.
#[derive(Debug, Copy, Clone)]
struct ElseBlock;
impl_display!(ElseBlock, "else block");
impl TokenEater for ElseBlock {
    type Output = tree::StatementBlock;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Keyword(KeywordToken::Else))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        tf.feed(KeywordToken::Else)?;
        feed_one_of!(tf, [StatementBlock, IfStatement.spanned().map(|x| vec![x])])
    }
}

/// Consumes an `assert` statement.
#[derive(Debug, Copy, Clone)]
struct AssertStatement;
impl_display!(AssertStatement, "assert statement");
impl TokenEater for AssertStatement {
    type Output = tree::Statement;
    fn might_match(&self, tf: TokenFeeder<'_>) -> bool {
        next_token_matches!(tf, TokenClass::Keyword(KeywordToken::Assert))
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        tf.feed(KeywordToken::Assert)?;
        Ok(tree::Statement::Assert {
            expr: tf.feed(Expr)?,
            msg: {
                if let Some(()) = tf.try_feed(PunctuationToken::Comma).transpose()? {
                    Some(tf.feed(StringLiteral.spanned())?)
                } else {
                    None
                }
            },
        })
    }
}

/// Consumes a variable assignment statement.
#[derive(Debug, Copy, Clone)]
struct VarAssignStatement;
impl_display!(VarAssignStatement, "variable assignment");
impl TokenEater for VarAssignStatement {
    type Output = tree::Statement;
    fn might_match(&self, mut tf: TokenFeeder<'_>) -> bool {
        tf.feed(Expr).is_ok() && AssignOp.might_match(tf)
    }
    fn eat(&self, tf: &mut TokenFeeder<'_>) -> LangResult<Self::Output> {
        Ok(tree::Statement::SetVar {
            var_expr: tf.feed(Expr)?,
            assign_op: tf.feed(AssignOp)?,
            value_expr: tf.feed(Expr)?,
        })
    }
}
