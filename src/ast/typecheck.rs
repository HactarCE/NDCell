use super::super::errors::*;
use super::super::types::Type;
use super::super::Spanned;
use super::components::{typed, untyped};
use typed::{FunctionMeta, FunctionType};
use LangErrorMsg::{
    BecomeInHelperFunction, Expected, OpError, ReturnInTransitionFunction, TypeError,
    UseOfUninitializedVariable,
};

pub trait ResolveTypes {
    type TypedSelf;
    fn resolve_types(self, meta: &mut FunctionMeta) -> LangResult<Self::TypedSelf>;
}

impl ResolveTypes for Spanned<untyped::Statement> {
    type TypedSelf = Spanned<typed::Statement>;
    fn resolve_types(self, meta: &mut FunctionMeta) -> LangResult<Spanned<typed::Statement>> {
        let span = self.span;
        Ok(Spanned {
            span,
            inner: match self.inner {
                untyped::Statement::SetVar {
                    var_expr,
                    value_expr,
                } => {
                    let var_expr_span = var_expr.span;
                    let value_expr_span = value_expr.span;
                    let typed_value_expr = value_expr.resolve_types(meta)?;
                    let new_type = typed_value_expr.get_type();
                    if let untyped::Expr::Var(var_name) = var_expr.inner {
                        if let Some(var_type) = meta.vars.get(&var_name) {
                            // The variable already exists.
                            if *var_type != new_type {
                                // ... with a different type. This is bad. :(
                                Err(TypeError {
                                    expected: *var_type,
                                    got: new_type,
                                }
                                .with_span(value_expr_span))?;
                            }
                        } else {
                            // The variable does not already exist, so create it.
                            meta.vars.insert(var_name.clone(), new_type);
                        }
                        typed::Statement::SetVar {
                            var_name: Spanned {
                                span: var_expr_span,
                                inner: var_name,
                            },
                            value_expr: typed_value_expr,
                        }
                    } else {
                        Err(Expected("variable name").with_span(var_expr))?
                    }
                }

                untyped::Statement::If {
                    cond_expr,
                    if_true,
                    if_false,
                } => typed::Statement::If {
                    cond_expr: cond_expr.resolve_types(meta)?.int()?,
                    if_true: if_true.resolve_types(meta)?,
                    if_false: if_false.resolve_types(meta)?,
                },

                untyped::Statement::Become(return_expr) => {
                    let typed_return_expr = return_expr.resolve_types(meta)?;
                    match meta.fn_type {
                        // 'become' is only allowed in transition functions.
                        FunctionType::Transition => typed::Statement::Return(typed_return_expr),
                        FunctionType::Helper(_) => Err(BecomeInHelperFunction.with_span(span))?,
                    }
                }

                untyped::Statement::Return(return_expr) => {
                    let typed_return_expr = return_expr.resolve_types(meta)?;
                    match meta.fn_type {
                        // 'return' is only allowed in helper functions.
                        FunctionType::Transition => {
                            Err(ReturnInTransitionFunction.with_span(span))?
                        }
                        FunctionType::Helper(ref mut return_type) => {
                            let this_return_type = typed_return_expr.get_type();
                            if let Some(expected_return_type) = *return_type {
                                if this_return_type != expected_return_type {
                                    Err(TypeError {
                                        expected: expected_return_type,
                                        got: this_return_type,
                                    })?;
                                }
                            } else {
                                *return_type = Some(this_return_type);
                            }
                            typed::Statement::Return(typed_return_expr)
                        }
                    }
                }
            },
        })
    }
}

impl ResolveTypes for Spanned<untyped::Expr> {
    type TypedSelf = typed::Expr;
    fn resolve_types(self, meta: &mut FunctionMeta) -> LangResult<typed::Expr> {
        let span = self.span;
        match self.inner {
            untyped::Expr::Int(i) => Ok(typed::IntExpr::Literal(i).as_generic(span)),
            untyped::Expr::Tag(expr) => {
                let x = expr.resolve_types(meta)?.int()?;
                Ok(typed::CellStateExpr::FromId(Box::new(x)).as_generic(span))
            }
            untyped::Expr::Neg(expr) => {
                let x = expr.resolve_types(meta)?.int()?;
                Ok(typed::IntExpr::Neg(Box::new(x)).as_generic(span))
            }
            untyped::Expr::Op(expr1, op, expr2) => {
                let lhs = expr1.resolve_types(meta)?;
                let rhs = expr2.resolve_types(meta)?;
                use Type::*;
                match (lhs.get_type(), op, rhs.get_type()) {
                    (Int, op, Int) => Ok(typed::IntExpr::Op {
                        lhs: Box::new(lhs.int()?),
                        op,
                        rhs: Box::new(rhs.int()?),
                    }
                    .as_generic(span)),
                    _ => Err(OpError {
                        op_sym: op.get_symbol(),
                        lhs: lhs.get_type(),
                        rhs: rhs.get_type(),
                    }
                    .with_span(span)),
                }
            }
            untyped::Expr::Cmp(initial, comparisons) => {
                let initial = initial.resolve_types(meta)?;
                let comparisons = comparisons
                    .into_iter()
                    .map(|(cmp, expr)| Ok((cmp, expr.resolve_types(meta)?)));
                match initial.get_type() {
                    Type::Int => Ok(typed::IntExpr::CmpInt(typed::CmpExpr::new(
                        initial,
                        comparisons,
                    )?)
                    .as_generic(span)),
                    Type::CellState => Ok(typed::IntExpr::CmpCellState(
                        typed::CmpExpr::new(initial, comparisons)?.eq_only(Type::CellState)?,
                    )
                    .as_generic(span)),
                }
            }
            untyped::Expr::Var(var_name) => {
                if let Some(var_type) = meta.vars.get(&var_name) {
                    match var_type {
                        Type::Int => Ok(typed::IntExpr::Var(var_name).as_generic(span)),
                        Type::CellState => Ok(typed::CellStateExpr::Var(var_name).as_generic(span)),
                    }
                } else {
                    Err(UseOfUninitializedVariable.with_span(span))
                }
            }
        }
    }
}

impl<T: ResolveTypes> ResolveTypes for Vec<T> {
    type TypedSelf = Vec<T::TypedSelf>;
    fn resolve_types(self, meta: &mut FunctionMeta) -> LangResult<Vec<T::TypedSelf>> {
        self.into_iter().map(|x| x.resolve_types(meta)).collect()
    }
}
