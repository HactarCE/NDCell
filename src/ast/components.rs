use super::Spanned;

pub type Block = Vec<Spanned<Statement>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    // SetVar(Spanned<Var>, Spanned<Expr>),
    // If(
    //     // If
    //     Spanned<Expr>,
    //     Statements,
    //     // Elseif
    //     Vec<(Spanned<Expr>, Statements)>,
    //     // Else
    //     Option<Statements>,
    // ),
    // ForLoop(Spanned<Var>, Spanned<Expr>, Statements),
    // WhileLoop(Spanned<Expr>, Statements),
    // DoWhileLoop(Statements, Spanned<Expr>),
    // Break,
    // Continue,
    // Remain,
    Become(Spanned<Expr>),
    Return(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Transition(Vec<Spanned<Statement>>),
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
