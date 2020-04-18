use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

use super::super::super::errors::*;
use super::super::super::span::{Span, Spanned};
use super::super::super::types::Type;
use super::super::ResolveTypes;
use super::{common::*, untyped};
use LangErrorMsg::{CmpError, TypeError};

pub type StatementBlock = Vec<Spanned<Statement>>;

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

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub fn_type: FunctionType,
    pub vars: HashMap<String, Type>,
    pub statements: StatementBlock,
}
impl Function {
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

#[derive(Debug, Copy, Clone)]
pub enum FunctionType {
    Transition,
    Helper(Option<Type>),
}

#[derive(Debug)]
pub struct FunctionMeta<'a> {
    pub fn_type: &'a mut FunctionType,
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

    /// Jump directly to a given instruction index (used by the interpreter).
    Goto(usize),
    /// End of program (used by the interpreter).
    End,
}

/// An expression of any type.
#[derive(Debug, Clone)]
pub enum Expr {
    Int(Spanned<IntExpr>),
    CellState(Spanned<CellStateExpr>),
}
impl Expr {
    pub fn int(self) -> LangResult<Spanned<IntExpr>> {
        match self {
            Self::Int(e) => Ok(e),
            _ => self.type_err(Type::Int),
        }
    }
    pub fn int_ref(&self) -> LangResult<&Spanned<IntExpr>> {
        match self {
            Self::Int(e) => Ok(e),
            _ => self.type_err(Type::Int),
        }
    }
    pub fn cell_state(self) -> LangResult<Spanned<CellStateExpr>> {
        match self {
            Self::CellState(e) => Ok(e),
            _ => self.type_err(Type::CellState),
        }
    }
    pub fn cell_state_ref(&self) -> LangResult<&Spanned<CellStateExpr>> {
        match self {
            Self::CellState(e) => Ok(e),
            _ => self.type_err(Type::CellState),
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::CellState(_) => Type::CellState,
        }
    }
    pub fn get_span(&self) -> Span {
        *match self {
            Self::Int(Spanned { span, .. }) => span,
            Self::CellState(Spanned { span, .. }) => span,
        }
    }
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

/// An expression that resolves to an integer.
#[derive(Debug, Clone)]
pub enum IntExpr {
    FnCall(FnCall),
    Var(String),
    Literal(i64),
    /// Two operands joined by a single operator.
    Op {
        lhs: Box<Spanned<IntExpr>>,
        op: Op,
        rhs: Box<Spanned<IntExpr>>,
    },
    Neg(Box<Spanned<IntExpr>>),
    CmpInt(CmpExpr<IntExpr, Cmp>),
    CmpCellState(CmpExpr<CellStateExpr, EqCmp>),
}
impl IntExpr {
    pub fn as_generic(self, span: Span) -> Expr {
        Expr::Int(Spanned { span, inner: self })
    }
}
impl TryFrom<Expr> for Spanned<IntExpr> {
    type Error = LangError;
    fn try_from(expr: Expr) -> LangResult<Self> {
        expr.int()
    }
}

/// An expression that resolves to a cell state.
#[derive(Debug, Clone)]
pub enum CellStateExpr {
    FnCall(FnCall),
    Var(String),
    FromId(Box<Spanned<IntExpr>>),
}
impl CellStateExpr {
    pub fn as_generic(self, span: Span) -> Expr {
        Expr::CellState(Spanned { span, inner: self })
    }
}
impl TryFrom<Expr> for Spanned<CellStateExpr> {
    type Error = LangError;
    fn try_from(expr: Expr) -> LangResult<Self> {
        expr.cell_state()
    }
}

#[derive(Debug, Clone)]
pub enum Builtin {}

#[derive(Debug, Clone)]
pub struct FnCall {
    name: String,
    args: Vec<Expr>,
    is_method: bool,
}

/// N operands joined by N-1 comparison operators, used for chained comparisons
/// (e.g. `0 < x <= 3`).
#[derive(Debug, Clone)]
pub struct CmpExpr<ExprType, CmpType: Copy> {
    pub initial: Box<Spanned<ExprType>>,
    pub comparisons: Vec<(CmpType, Spanned<ExprType>)>,
}
impl<E, C: Copy> CmpExpr<E, C> {
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
    pub fn eq_only(self, ty: Type) -> LangResult<CmpExpr<E, EqCmp>> {
        let mut new_comparisons = Vec::with_capacity(self.comparisons.len());
        let mut lhs_span = self.initial.span;
        for (cmp, expr) in self.comparisons {
            let rhs_span = expr.span;
            new_comparisons.push((
                cmp.try_into().map_err(|_| {
                    CmpError {
                        cmp_sym: cmp.get_symbol(),
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
// impl<E, C> CmpExpr<E, C> {
//     pub fn new(
//         initial: Spanned<E>,
//         comparisons: Vec<(C, Spanned<E>)>,
//     ) -> LangResult<Self> {
//         Ok(Self {
//             initial: Box::new(initial),
//             comparisons: comparisons
//                 .into_iter()
//                 .map(|(cmp, expr)| (cmp, expr.try_into()?))
//                 .collect::<LangResult<_>>()?,
//         })
//     }
// }
