//! Untyped AST components.

use std::convert::TryInto;
use std::fmt;

use super::super::super::errors::*;
use super::super::super::meta::RuleMeta;
use super::super::Spanned;
use super::{common::*, typed};
use LangErrorMsg::{MissingTransitionFunction, MultipleTransitionFunctions};

pub type StatementBlock = Vec<Spanned<Statement>>;

/// A complete rule containing a transition function, pre-typecheck.
#[derive(Debug, Clone)]
pub struct Rule {
    pub source_code: String,
    pub meta: RuleMeta,
    pub transition_fn: StatementBlock,
}
impl Rule {
    /// Check types in this rule, returning a typed::Rule.
    pub fn check_types(self) -> LangResult<typed::Rule> {
        self.try_into()
    }
}
impl Rule {
    pub fn from_ast(source_code: String, directives: Vec<Spanned<Directive>>) -> LangResult<Self> {
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
        let meta = RuleMeta::new();
        let transition_fn = transition_fn.ok_or(MissingTransitionFunction)?;
        Ok(Self {
            source_code,
            meta,
            transition_fn,
        })
    }
}

/// A single '@' directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    /// The transition function.
    Transition(StatementBlock),
}

/// A single statement, pre-typecheck.
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

/// An expression, pre-typecheck.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// Variable access.
    Var(String),
    /// Constant integer.
    Int(i64),
    /// Cell state constructed from another value.
    Tag(Box<Spanned<Expr>>),
    /// Negation of an integer.
    Neg(Box<Spanned<Expr>>),
    /// Operation on two values.
    Op {
        /// Left-hand-side operand.
        lhs: Box<Spanned<Expr>>,
        /// Operator.
        op: Op,
        /// Right-hand-side operand.
        rhs: Box<Spanned<Expr>>,
    },
    /// Comparison between two values.
    Cmp(Box<Spanned<Expr>>, Vec<(Cmp, Spanned<Expr>)>),
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Rule {{")?;
        writeln!(f, "    @TRANSITION {:#?}", self.transition_fn)?;
        write!(f, "}}")?;
        Ok(())
    }
}
