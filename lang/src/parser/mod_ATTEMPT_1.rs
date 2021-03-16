use codemap::{Span, Spanned};

use crate::data::LangInt;
use crate::lexer::Token;

pub struct SyntaxNode {
    kind: SyntaxKind,
    span: Span,
    children: Vec<SyntaxNode>,
}

enum SyntaxKind {
    Directive,
    Statement(StatementKind),
    Expr(ExprKind),
    Token(Token),
}

pub enum Statement {
    /// Executes multiple statements in sequence.
    Block,

    /// Sets a variable value.
    Assign,

    /// Breaks out of a loop.
    Break,
    /// Continues to the next iteration of a loop.
    Continue,
    /// Iterates over a set of values.
    ForLoop,

    /// Returns a value from a transition function.
    Become,
    /// Returns the original cell state from the transition function.
    Remain,
    /// Returns a value from a helper function.
    Return,

    /// Branches if a condition is true.
    If,
    /// Branches based on a value.
    Match { expr: Expr, cases: Vec<MatchCase> },
    /// Branches if a condition is false.
    Unless,

    /// Aborts the program if a condition is false.
    Assert {
        expr: Spanned<Expr>,
        msg: Option<Spanned<StringLiteral>>,
    },
    /// Aborts the program unconditionally.
    Error,
}

pub struct MatchCase {
    possibities: Vec<Spanned<Expr>>,
    block: Spanned<Statement>,
}

pub struct ParenList {}

pub enum Expr {
    /// Single token.
    Token(Token),

    /// Vector literal.
    VectorLiteral,

    /// Parenthetical group containing single expression.
    ParenGroup(Spanned<Expr>),

    /// Operation on one value.
    UnaryOp {
        op: Spanned<Token>,
        rhs: Box<Spanned<Expr>>,
    },
    /// Operation on two values.
    BinaryOp {
        lhs: Box<Spanned<Expr>>,
        op: Spanned<Token>,
        rhs: Box<Spanned<Expr>>,
    },
    /// Comparison between at least two values.
    Comparison {
        /// Expressions to compare (at least two).
        exprs: Vec<Spanned<Expr>>,
        /// Comparison operations (one less than the number of expressions).
        cmps: Vec<Spanned<Token>>,
    },

    /// Free function call.
    FunctionCall {
        func: Box<Spanned<Expr>>,
        args: Option<ParenList>,
    },
    /// Attribute access / method call.
    MethodCall,

    /// Indexing access / type parameter expression.
    Index,
}
