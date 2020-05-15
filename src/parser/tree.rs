//! Parse tree.

use std::collections::HashMap;
use std::convert::TryFrom;

use super::super::errors::*;
use super::super::lexer::{AssignmentToken, ComparisonToken, OperatorToken, PunctuationToken};
use super::super::Spanned;
use LangErrorMsg::RepeatDirective;

#[derive(Debug, Clone)]
pub struct ParseTree {
    pub source_code: String,
    pub directives: HashMap<Directive, Vec<DirectiveContents>>,
}
impl ParseTree {
    pub fn get_single_directive(
        &self,
        directive: Directive,
    ) -> LangResult<Option<&DirectiveContents>> {
        match self.directives.get(&directive).map(Vec::as_slice) {
            None => Ok(None),
            Some([]) => Ok(None),
            Some([x]) => Ok(Some(x)),
            Some([_, _, ..]) => Err(RepeatDirective(directive.name()).without_span()),
        }
    }
}

pub type StatementBlock = Vec<Spanned<Statement>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Directive {
    /// Transition function.
    Transition,
    /// State definitions.
    States,
    /// Number of dimensions.
    Dimensions,
}
impl Directive {
    pub fn name(self) -> &'static str {
        match self {
            Self::Transition => "transition",
            Self::States => "states",
            Self::Dimensions => "dimensions",
        }
    }
}
impl TryFrom<&str> for Directive {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, ()> {
        match s.to_ascii_lowercase().as_ref() {
            "transition" => Ok(Self::Transition),
            "states" => Ok(Self::States),
            "dim" | "dimen" | "dimensions" | "ndim" => Ok(Self::Dimensions),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DirectiveContents {
    Block(Spanned<StatementBlock>),
    Expr(Spanned<Expr>),
}
impl From<Spanned<StatementBlock>> for DirectiveContents {
    fn from(block: Spanned<StatementBlock>) -> Self {
        Self::Block(block)
    }
}
impl From<Spanned<Expr>> for DirectiveContents {
    fn from(expr: Spanned<Expr>) -> Self {
        Self::Expr(expr)
    }
}

/// A single statement, pre-typecheck.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Sets a variable value.
    SetVar {
        /// Variable to set.
        var_expr: Spanned<Expr>,
        /// Assignment operator.
        assign_op: AssignmentToken,
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
    /// Integer literal.
    Int(i64),
    /// Identifier.
    Ident(String),
    /// Parethetical or bracketed group.
    Group {
        /// Punctuation token at the beginning of the group.
        start_token: PunctuationToken,
        /// List of comma-separated members.
        inner: Box<Spanned<Expr>>,
    },
    /// Comma-separated list.
    List(Vec<Box<Spanned<Expr>>>),
    /// Operation on one value.
    UnaryOp {
        /// Operator.
        op: OperatorToken,
        /// Operand.
        operand: Box<Spanned<Expr>>,
    },
    /// Operation on two values.
    BinaryOp {
        /// Left-hand-side operand.
        lhs: Box<Spanned<Expr>>,
        /// Operator.
        op: OperatorToken,
        /// Right-hand-side operand.
        rhs: Box<Spanned<Expr>>,
    },
    /// Comparison between two values.
    Cmp {
        exprs: Vec<Spanned<Expr>>,
        cmps: Vec<ComparisonToken>,
    },
}
