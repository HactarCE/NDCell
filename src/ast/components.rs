use std::convert::TryFrom;

use super::super::errors::*;
use super::Spanned;

pub type StatementBlock = Vec<Spanned<Statement>>;

pub struct Program {
    pub transition_fn: StatementBlock,
}
impl TryFrom<Vec<Spanned<Directive>>> for Program {
    type Error = LangError;
    fn try_from(directives: Vec<Spanned<Directive>>) -> LangResult<Self> {
        let mut transition_fn = None;
        for directive in directives {
            match directive.inner {
                Directive::Transition(block) => {
                    if transition_fn.is_none() {
                        transition_fn = Some(block);
                    } else {
                        return spanned_lang_err(directive.span, "Multiple transition functions");
                    }
                }
            }
        }
        let transition_fn = transition_fn.ok_or(lang_error("Missing transition function"))?;
        Ok(Self { transition_fn })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    // SetVar(Spanned<Var>, Spanned<Expr>),
    // If(
    //     // If
    //     Spanned<Expr>,
    //     StatementBlock,
    //     // Elseif
    //     Vec<(Spanned<Expr>, StatementBlock)>,
    //     // Else
    //     Option<StatementBlock>,
    // ),
    // ForLoop(Spanned<Var>, Spanned<Expr>, StatementBlock),
    // WhileLoop(Spanned<Expr>, StatementBlock),
    // DoWhileLoop(StatementBlock, Spanned<Expr>),
    // Break,
    // Continue,

    // /// Returns the center cell state from the transition function.
    // Remain,
    /// Returns a value from a transition function.
    Become(Spanned<Expr>),
    /// Returns a value from a helper function.
    Return(Spanned<Expr>),

    /// Jump directly to a given instruction index (used by the interpreter).
    Goto(usize),
    /// End of program (used by the interpreter).
    End,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Transition(StatementBlock),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Tag(Box<Spanned<Expr>>),
    Neg(Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Var(String),
}
