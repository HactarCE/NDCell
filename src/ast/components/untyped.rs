use std::convert::{TryFrom, TryInto};
use std::fmt;

use super::super::super::errors::*;
use super::super::Spanned;
use super::{common::*, typed};
use LangErrorMsg::{MissingTransitionFunction, MultipleTransitionFunctions};

pub type StatementBlock = Vec<Spanned<Statement>>;

#[derive(Debug, Clone)]
pub struct Program {
    pub transition_fn: StatementBlock,
}
impl Program {
    pub fn check_types(self) -> LangResult<typed::Program> {
        self.try_into()
    }
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
                        return Err(MultipleTransitionFunctions.with_span(directive.span));
                    }
                }
            }
        }
        let transition_fn = transition_fn.ok_or(MissingTransitionFunction)?;
        Ok(Self { transition_fn })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Transition(StatementBlock),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Sets a variable value.
    SetVar {
        /// Variable to set.
        var_expr: Spanned<Expr>,
        /// Value to store in the variable.
        value_expr: Spanned<Expr>,
    },
    /// Branches conditionally.
    If {
        /// Condition.
        cond_expr: Spanned<Expr>,
        /// Statements to execute if condition is truthy.
        if_true: StatementBlock,
        /// Statements to execute if condition is falsey.
        if_false: StatementBlock,
    },
    // ForLoop(Spanned<Expr>, Spanned<Expr>, StatementBlock),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Tag(Box<Spanned<Expr>>),
    Neg(Box<Spanned<Expr>>),
    Op {
        lhs: Box<Spanned<Expr>>,
        op: Op,
        rhs: Box<Spanned<Expr>>,
    },
    Cmp(Box<Spanned<Expr>>, Vec<(Cmp, Spanned<Expr>)>),
    Var(String),
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Program {{")?;
        writeln!(f, "    @TRANSITION {:#?}", self.transition_fn)?;
        write!(f, "}}")?;
        Ok(())
    }
}
