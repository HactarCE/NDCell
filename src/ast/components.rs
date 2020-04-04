use std::convert::TryFrom;
use std::fmt;

use super::super::errors::*;
use super::tokens::ComparisonToken;
use super::Spanned;
use LangErrorMsg::{MissingTransitionFunction, MultipleTransitionFunctions};

pub type StatementBlock = Vec<Spanned<Statement>>;

const DISPLAY_INDENT: usize = 2;

#[derive(Debug, Clone)]
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
pub enum Statement {
    // SetVar(Spanned<Var>, Spanned<Expr>),
    If(
        // If
        Spanned<Expr>,
        StatementBlock,
        // // Elseif
        // Vec<(Spanned<Expr>, StatementBlock)>,
        // // Else
        // Option<StatementBlock>,
    ),
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
    /// A series of chained comparisons (a la Python). For example, `x < y == z`
    /// would be represented (roughly) as: `Expr::Comparison(x, [(LessThan, y),
    /// (Equal, z)])`.
    Comparison(Box<Spanned<Expr>>, Vec<(Comparison, Spanned<Expr>)>),
    Var(String),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}
impl Comparison {
    pub fn get_symbol(self) -> &'static str {
        match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThanOrEqual => ">=",
        }
    }
}
impl From<ComparisonToken> for Comparison {
    fn from(token_class: ComparisonToken) -> Self {
        use ComparisonToken::*;
        match token_class {
            Equal => Self::Equal,
            NotEqual => Self::NotEqual,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessThanOrEqual => Self::LessThanOrEqual,
            GreaterThanOrEqual => Self::GreaterThanOrEqual,
        }
    }
}

fn write_indent(f: &mut fmt::Formatter, spaces: usize) -> fmt::Result {
    write!(f, "{:spaces$}", "", spaces = spaces)
}

fn write_statement_block_indented(
    f: &mut fmt::Formatter,
    statements: &StatementBlock,
    indent: usize,
) -> fmt::Result {
    writeln!(f, "{{")?;
    for statement in statements {
        statement.inner.fmt_indented(f, indent + DISPLAY_INDENT)?;
        writeln!(f)?;
    }
    write_indent(f, indent)?;
    write!(f, "}}")?;
    Ok(())
}

impl Statement {
    fn name(&self) -> &'static str {
        match self {
            Self::If(_, _) => "If",
            Self::Become(_) => "Become",
            Self::Return(_) => "Return",
            Self::Goto(_) => "Goto",
            Self::End => "End",
        }
    }
    fn fmt_indented(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        write!(f, "{}", self.name())?;
        let next_indent = indent + DISPLAY_INDENT;
        match self {
            Self::If(expr, if_true) => {
                writeln!(f, " (")?;
                expr.inner.fmt_indented(f, next_indent)?;
                writeln!(f)?;
                write_indent(f, indent)?;
                write!(f, ") Then ")?;
                write_statement_block_indented(f, &if_true, indent)?;
            }
            Self::Become(expr) | Self::Return(expr) => {
                writeln!(f, " (")?;
                expr.inner.fmt_indented(f, next_indent)?;
                writeln!(f)?;
                write_indent(f, indent)?;
                write!(f, ")")?;
            }
            Self::Goto(index) => write!(f, " {}", index)?,
            Self::End => (),
        }
        Ok(())
    }
}

impl Expr {
    fn name(&self) -> &'static str {
        match self {
            Self::Int(_) => "Int",
            Self::Tag(_) => "Tag",
            Self::Neg(_) => "Neg",
            Self::Add(_, _) => "Add",
            Self::Sub(_, _) => "Sub",
            Self::Comparison(_, _) => "Comparison",
            Self::Var(_) => "Var",
        }
    }
    fn fmt_indented(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        write!(f, "{}", self.name())?;
        let next_indent = indent + DISPLAY_INDENT;
        match self {
            Self::Int(n) => write!(f, " {}", n)?,
            Self::Tag(expr) | Self::Neg(expr) => {
                writeln!(f)?;
                expr.inner.fmt_indented(f, next_indent)?;
            }
            Self::Add(expr1, expr2) | Self::Sub(expr1, expr2) => {
                writeln!(f)?;
                expr1.inner.fmt_indented(f, next_indent)?;
                writeln!(f)?;
                expr2.inner.fmt_indented(f, next_indent)?;
            }
            Self::Comparison(expr1, comparisons) => {
                writeln!(f)?;
                let next_next_indent = next_indent + DISPLAY_INDENT;
                expr1.inner.fmt_indented(f, next_next_indent)?;
                for (comparison_type, expr2) in comparisons {
                    writeln!(f)?;
                    write_indent(f, next_indent)?;
                    writeln!(f, "{:?}", comparison_type)?;
                    expr2.inner.fmt_indented(f, next_next_indent)?;
                }
            }
            Self::Var(name) => write!(f, " '{}'", name)?,
        }
        Ok(())
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Program {{")?;
        {
            write_indent(f, DISPLAY_INDENT)?;
            write!(f, "@TRANSITION ")?;
            write_statement_block_indented(f, &self.transition_fn, DISPLAY_INDENT)?;
            writeln!(f)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}
