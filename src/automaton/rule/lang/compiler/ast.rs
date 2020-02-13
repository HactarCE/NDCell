use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block(Vec<Spanned<Statement>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    SetVar(Spanned<Var>, Spanned<Expr>),
    // If(
    //     // If
    //     Spanned<Expr>,
    //     Spanned<Block>,
    //     // Elseif
    //     Vec<(Spanned<Expr>, Spanned<Block>)>,
    //     // Else
    //     Option<Spanned<Block>>,
    // ),
    // ForLoop(Spanned<Var>, Spanned<Expr>, Spanned<Block>),
    // WhileLoop(Spanned<Expr>, Spanned<Block>),
    // DoWhileLoop(Spanned<Block>, Spanned<Expr>),
    // Break,
    // Continue,
    // Remain,
    // Become(Spanned<Expr>),
    // Return(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Int(i32),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    id: usize,
}
