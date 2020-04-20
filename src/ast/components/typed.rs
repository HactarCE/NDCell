//! Typed AST components.

use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

use super::super::super::errors::*;
use super::super::super::span::{Span, Spanned};
use super::super::super::types::Type;
use super::super::ResolveTypes;
use super::{common::*, untyped};
use LangErrorMsg::{CmpError, TypeError};

pub type StatementBlock = Vec<Spanned<Statement>>;

/// A complete program containing a transition function.
#[derive(Debug, Clone)]
pub struct Program {
    pub transition_fn: Function,
}
impl TryFrom<untyped::Program> for Program {
    type Error = LangError;
    fn try_from(untyped_program: untyped::Program) -> LangResult<Self> {
        Ok(Self {
            transition_fn: Function::new(
                "transition".to_owned(),
                untyped_program.transition_fn,
                FunctionType::Transition,
            )?,
        })
    }
}

/// A pure function.
#[derive(Debug, Clone)]
pub struct Function {
    /// The function name.
    pub name: String,
    /// The function type (e.g. transition vs. helper function and return type).
    pub fn_type: FunctionType,
    /// Variable names and types.
    pub vars: HashMap<String, Type>,
    /// Statements in the function.
    pub statements: StatementBlock,
}
impl Function {
    /// Constructs a new function and resolve types. Returns an error if type
    /// checking fails.
    pub fn new(
        name: String,
        untyped_statements: untyped::StatementBlock,
        mut fn_type: FunctionType,
    ) -> LangResult<Self> {
        let mut vars = HashMap::new();
        let statements = untyped_statements.resolve_types(&mut FunctionMeta {
            fn_type: &mut fn_type,
            vars: &mut vars,
        })?;
        Ok(Self {
            name,
            fn_type,
            vars,
            statements,
        })
    }
}

/// The type of function (i.e. transition vs. helper function and return type).
#[derive(Debug, Copy, Clone)]
pub enum FunctionType {
    /// The transition function (always return Type::CellState).
    Transition,
    /// A helper function (return type varies; None means not yet determined).
    Helper(Option<Type>),
}

/// Metadata about a function's type and variable.
#[derive(Debug)]
pub struct FunctionMeta<'a> {
    /// The type of function.
    pub fn_type: &'a mut FunctionType,
    /// Variable names and types.
    pub vars: &'a mut HashMap<String, Type>,
}

/// A single statement.
#[derive(Debug, Clone)]
pub enum Statement {
    /// Sets a variable value.
    SetVar {
        /// Variable to set.
        var_name: Spanned<String>,
        /// Value to store in the variable.
        value_expr: Expr,
    },
    /// Sets a variable value.
    If {
        /// Condition.
        cond_expr: Spanned<IntExpr>,
        /// Statements to execute if condition is truthy.
        if_true: StatementBlock,
        /// Statements to execute if condition is falsey.
        if_false: StatementBlock,
    },

    /// Returns a value from a function.
    Return(Expr),

    /// Jumps directly to a given instruction index (used by the interpreter).
    ///
    /// Note that the index given here is actually the index _before_ the
    /// instruction that will be executed next, since the interpreter still
    /// increments the instruction pointer.
    Goto(usize),
    /// End of program (used by the interpreter).
    End,
}

/// An expression of any type.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer expression.
    Int(Spanned<IntExpr>),
    /// Cell state expression.
    CellState(Spanned<CellStateExpr>),
}
impl Expr {
    /// Returns the IntExpr inside if this is Expr::Int; otherwise a TypeError.
    pub fn into_int_expr(self) -> LangResult<Spanned<IntExpr>> {
        match self {
            Self::Int(e) => Ok(e),
            _ => self.type_err(Type::Int),
        }
    }
    /// Returns a reference to the IntExpr inside if this is Expr::Int;
    /// otherwise a TypeError.
    pub fn as_int_expr(&self) -> LangResult<&Spanned<IntExpr>> {
        match self {
            Self::Int(e) => Ok(e),
            _ => self.type_err(Type::Int),
        }
    }
    /// Returns the CellStateExpr inside if this is Expr::CellState; otherwise a
    /// TypeError.
    pub fn into_cell_state_expr(self) -> LangResult<Spanned<CellStateExpr>> {
        match self {
            Self::CellState(e) => Ok(e),
            _ => self.type_err(Type::CellState),
        }
    }
    /// Returns a reference to the CellStateExpr inside if this is
    /// Expr::CellState; otherwise a TypeError.
    pub fn as_cell_state_expr(&self) -> LangResult<&Spanned<CellStateExpr>> {
        match self {
            Self::CellState(e) => Ok(e),
            _ => self.type_err(Type::CellState),
        }
    }
    /// Returns the type that this expression evaluates to.
    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
        }
    }
    /// Returns the Span of this expression.
    pub fn get_span(&self) -> Span {
        *match self {
            Self::Int(Spanned { span, .. }) => span,
            Self::CellState(Spanned { span, .. }) => span,
        }
    }
    /// Constructs a TypeError at this expression.
    fn type_err<T>(&self, expected: Type) -> LangResult<T> {
        Err(TypeError {
            expected,
            got: self.get_type(),
        }
        .with_span(self.get_span()))
    }
}
impl Into<Span> for &Expr {
    fn into(self) -> Span {
        self.get_span()
    }
}

/// An expression that evaluates to an integer.
#[derive(Debug, Clone)]
pub enum IntExpr {
    /// Function call.
    FnCall(FnCall),
    /// Variable access.
    Var(String),
    /// Constant integer.
    Literal(i64),
    /// Operation on two other integers.
    Op {
        /// Left-hand-side operand.
        lhs: Box<Spanned<IntExpr>>,
        /// Operator.
        op: Op,
        /// Right-hand-side operand.
        rhs: Box<Spanned<IntExpr>>,
    },
    /// Negation of an integer.
    Neg(Box<Spanned<IntExpr>>),
    /// Comparison between two integers.
    CmpInt(CmpExpr<IntExpr, Cmp>),
    /// Comparison between two cell states (equality only).
    CmpCellState(CmpExpr<CellStateExpr, EqCmp>),
}
impl IntExpr {
    /// Wraps this IntExpr in an Expr::Int with the given Span.
    pub fn as_generic(self, span: Span) -> Expr {
        Expr::Int(Spanned { span, inner: self })
    }
}
impl TryFrom<Expr> for Spanned<IntExpr> {
    type Error = LangError;
    fn try_from(expr: Expr) -> LangResult<Self> {
        expr.into_int_expr()
    }
}

/// An expression that evaluates to a cell state.
#[derive(Debug, Clone)]
pub enum CellStateExpr {
    /// Function call.
    FnCall(FnCall),
    /// Variable access.
    Var(String),
    /// Cell state constructed from an integer ID.
    FromId(Box<Spanned<IntExpr>>),
}
impl CellStateExpr {
    /// Wraps this CellStateExpr in an Expr::CellState with the given Span.
    pub fn as_generic(self, span: Span) -> Expr {
        Expr::CellState(Spanned { span, inner: self })
    }
}
impl TryFrom<Expr> for Spanned<CellStateExpr> {
    type Error = LangError;
    fn try_from(expr: Expr) -> LangResult<Self> {
        expr.into_cell_state_expr()
    }
}

/// A function call.
#[derive(Debug, Clone)]
pub struct FnCall {
    /// The name of the function to call.
    name: String,
    /// The arguments to pass to it.
    args: Vec<Expr>,
    /// Whether the function is a method of the first argument.
    is_method: bool,
}

/// A series of chained comparisons, a la Python. For example, `x < y == z`
/// would be represented (roughly) as:
/// ```
/// CmpExpr {
///     initial: x,
///     comparisons: vec![
///         (Cmp::LessThan, y),
///         (Cmp::Equal, z),
///     ],
/// }
/// ```
#[derive(Debug, Clone)]
pub struct CmpExpr<ExprType, CmpType: Copy> {
    /// The leftmost expression to compare.
    pub initial: Box<Spanned<ExprType>>,
    /// Subsequent comparison operators followed by expressions to compare.
    pub comparisons: Vec<(CmpType, Spanned<ExprType>)>,
}
impl<E, C: Copy> CmpExpr<E, C> {
    /// Returns the Span of this entire comparison expression, from the leftmost
    /// sub-expression to the rightmost sub-expression.
    pub fn get_span(&self) -> Span {
        let lhs = self.initial.span;
        let rhs = match self.comparisons.last() {
            Some((_, expr)) => expr.span,
            None => lhs,
        };
        Span::merge(lhs, rhs)
    }
}
impl<E> CmpExpr<E, Cmp> {
    /// Constructs a new CmpExpr.
    pub fn new(
        initial: impl TryInto<Spanned<E>, Error = LangError>,
        comparisons: impl Iterator<
            Item = LangResult<(Cmp, impl TryInto<Spanned<E>, Error = LangError>)>,
        >,
    ) -> LangResult<Self> {
        Ok(Self {
            initial: Box::new(initial.try_into()?),
            comparisons: comparisons
                .map(|result| result.and_then(|(cmp, expr)| Ok((cmp, expr.try_into()?))))
                .collect::<LangResult<Vec<_>>>()?,
        })
    }
    /// Converts this CmpExpr<E, Cmp> into a CmpExp<E, EqCmp>, returning a
    /// CmpError if an invalid Cmp variant is found.
    pub fn eq_only(self, ty: Type) -> LangResult<CmpExpr<E, EqCmp>> {
        let mut new_comparisons = Vec::with_capacity(self.comparisons.len());
        let mut lhs_span = self.initial.span;
        for (cmp, expr) in self.comparisons {
            let rhs_span = expr.span;
            new_comparisons.push((
                cmp.try_into().map_err(|_| {
                    CmpError {
                        cmp,
                        lhs: ty,
                        rhs: ty,
                    }
                    .with_span(Span::merge(lhs_span, rhs_span))
                })?,
                expr,
            ));
            lhs_span = rhs_span;
        }
        Ok(CmpExpr {
            initial: self.initial,
            comparisons: new_comparisons,
        })
    }
}
