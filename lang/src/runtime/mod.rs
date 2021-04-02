use codemap::{Span, Spanned};
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::data::{SpannedValueExt, Value};
use crate::errors::{Error, Result};
use crate::functions::{self, FuncCall, Function};

pub enum RuntimeError {
    CannotConstEval(Span),
    Other(Error),
}
impl From<Error> for RuntimeError {
    fn from(e: Error) -> Self {
        Self::Other(e)
    }
}
pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;

pub struct AssignableValue<'f, R, V, E> {
    assign_fn: Box<dyn 'f + FnOnce(&mut R, Spanned<V>) -> std::result::Result<(), E>>,
    lhs_value: std::result::Result<Spanned<V>, E>,
}
pub type AssignableRuntimeValue<'f> = AssignableValue<'f, Runtime, Value, RuntimeError>;

/// NDCA runtime state.
#[derive(Debug, Default, Clone)]
pub struct Runtime {
    /// Variable values.
    pub vars: HashMap<Arc<String>, Value>,
    // /// Rule name.
    // rule_name: Option<String>,
    // /// Number of dimensions.
    // ndim: Option<usize>,
    // /// Number of states.
    // states: Option<usize>,
    errors: Vec<Error>,
}

/// Control flow command.
#[must_use]
#[derive(Debug, Clone)]
enum Flow {
    /// Execute the next statement as normal.
    Proceed,

    /// Break out of the loop (contains span of `break` statement).
    Break(Span),
    /// Continue to the next loop iteration (contains span of `continue`
    /// statement).
    Continue(Span),

    /// Return from a function (contains span of `return` statement and value
    /// returned).
    Return(Span, Option<Spanned<Value>>),
    /// Return from the transition function without modifying the cell (contains
    /// span of `remain` statement).
    Remain(Span),
    /// Return from the transition function (contains span of `become` statement
    /// and value returned).
    Become(Span, Spanned<Value>),
}
impl Default for Flow {
    fn default() -> Self {
        Self::Proceed
    }
}

impl Runtime {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn error(&mut self, e: Error) -> ast::AlreadyReported {
        self.errors.push(e);
        ast::AlreadyReported
    }

    pub fn run_init(&mut self, ast: &ast::Program) -> std::result::Result<(), &[Error]> {
        for &directive_id in ast.directives() {
            match ast.get_node(directive_id).data() {
                ast::DirectiveData::Init(block) => match self.exec_stmt(ast.get_node(*block)) {
                    Ok(Flow::Proceed) => (),

                    Ok(Flow::Break(span)) => {
                        self.error(Error::break_not_in_loop(span));
                        break;
                    }
                    Ok(Flow::Continue(span)) => {
                        self.error(Error::continue_not_in_loop(span));
                        break;
                    }

                    Ok(Flow::Return(span, _)) => {
                        self.error(Error::return_not_in_fn(span));
                        break;
                    }
                    Ok(Flow::Remain(span)) => {
                        self.error(Error::remain_not_in_fn(span));
                        break;
                    }
                    Ok(Flow::Become(span, _)) => {
                        self.error(Error::become_not_in_fn(span));
                        break;
                    }

                    Err(RuntimeError::CannotConstEval(_)) => {
                        self.error(internal_error_value!("run_init() got CannotConstEval"));
                        break;
                    }
                    Err(RuntimeError::Other(e)) => {
                        self.error(e);
                        break;
                    }
                },
            }
        }
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(&self.errors)
        }
    }

    fn exec_stmt(&mut self, stmt: ast::Stmt<'_>) -> RuntimeResult<Flow> {
        let ast = stmt.ast;
        match stmt.data() {
            ast::StmtData::Block(stmt_ids) => {
                for &stmt_id in stmt_ids {
                    match self.exec_stmt(ast.get_node(stmt_id))? {
                        Flow::Proceed => (),
                        flow => return Ok(flow),
                    }
                }
                Ok(Flow::Proceed)
            }

            ast::StmtData::Assign { lhs, op, rhs } => {
                let lhs = ast.get_node(*lhs);
                let rhs = ast.get_node(*rhs);
                let mut rhs_value = self.eval_expr(rhs)?;

                let AssignableValue {
                    assign_fn,
                    lhs_value,
                } = self.eval_expr_assignable(lhs)?;

                if let Some(op_func) = Option::<functions::math::BinaryOp>::from(op.node) {
                    rhs_value = Spanned {
                        node: op_func.eval_on_values(op.span, lhs_value?, rhs_value)?,
                        span: lhs.span().merge(rhs.span()),
                    };
                }

                assign_fn(self, rhs_value)?;

                Ok(Flow::Proceed)
            }

            ast::StmtData::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let condition = ast.get_node(*condition);
                if self.eval_expr(condition)?.to_bool()? {
                    if_true.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                } else {
                    if_false.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                }
            }

            ast::StmtData::Assert { condition, msg } => {
                let condition = ast.get_node(*condition);
                if self.eval_expr(condition)?.to_bool()? {
                    Err(match msg {
                        Some(msg) => Error::assertion_failed_with_msg(stmt.span(), msg),
                        None => Error::assertion_failed(stmt.span()),
                    }
                    .into())
                } else {
                    Ok(Flow::Proceed)
                }
            }
            ast::StmtData::Error { msg } => Err(match msg {
                Some(msg) => Error::user_error_with_msg(stmt.span(), msg),
                None => Error::user_error(stmt.span()),
            }
            .into()),

            ast::StmtData::Break => Ok(Flow::Break(stmt.span())),
            ast::StmtData::Continue => Ok(Flow::Continue(stmt.span())),
            ast::StmtData::ForLoop {
                iter_var,
                iter_expr: iter_expr_id,
                block,
            } => {
                let iter_expr = ast.get_node(*iter_expr_id);
                for it in self.eval_expr(iter_expr)?.iterate()? {
                    // TODO: when #[feature(hash_raw_entry)] stabalizes, use
                    // that here to avoid the extra `Arc::clone()` (and consider
                    // changing `vars` to a `HashMap<String, Value>`)
                    self.vars.insert(Arc::clone(&iter_var), it.node);
                    match self.exec_stmt(ast.get_node(*block))? {
                        Flow::Proceed | Flow::Continue(_) => (),
                        Flow::Break(_) => break,
                        flow => return Ok(flow),
                    }
                }
                Ok(Flow::Proceed)
            }

            ast::StmtData::Become(expr_id) => {
                let expr = ast.get_node(*expr_id);
                Ok(Flow::Become(stmt.span(), self.eval_expr(expr)?))
            }
            ast::StmtData::Remain => Ok(Flow::Remain(stmt.span())),
            ast::StmtData::Return(None) => Ok(Flow::Return(stmt.span(), None)),
            ast::StmtData::Return(Some(expr_id)) => {
                let expr = ast.get_node(*expr_id);
                Ok(Flow::Return(stmt.span(), Some(self.eval_expr(expr)?)))
            }
        }
    }

    fn eval_expr_assignable<'ast>(
        &mut self,
        expr: ast::Expr<'ast>,
    ) -> RuntimeResult<AssignableRuntimeValue<'ast>> {
        let ast = expr.ast;
        match expr.data() {
            ast::ExprData::Identifier(var_name) => {
                // TODO: when #[feature(hash_raw_entry)] stabalizes, use
                // that here to avoid the extra `Arc::clone()` (and
                // consider changing `vars` to a `HashMap<String,
                // Value>`)
                Ok(AssignableRuntimeValue {
                    assign_fn: Box::new(move |state, new_value| {
                        state.vars.insert(Arc::clone(var_name), new_value.node);
                        Ok(())
                    }),
                    lhs_value: self.eval_expr(expr),
                })
            }
            ast::ExprData::MethodCall { obj, attr, args } => {
                Err(Error::unimplemented(expr.span()).into())
            }
            ast::ExprData::IndexOp { obj, args } => Err(Error::unimplemented(expr.span()).into()),

            _ => Err(Error::cannot_assign_to(expr.span()).into()),
        }
    }

    pub fn eval_expr(&mut self, expr: ast::Expr<'_>) -> RuntimeResult<Spanned<Value>> {
        let ast = expr.ast;
        let value = match expr.data() {
            ast::ExprData::Paren(expr_id) => self.eval_expr(ast.get_node(*expr_id))?.node,

            ast::ExprData::Identifier(var_name) => self
                .vars
                .get(var_name)
                .cloned()
                .ok_or_else(|| Error::uninitialized_variable(expr.span()))?,

            ast::ExprData::Constant(value) => value.clone(),

            ast::ExprData::BinaryOp(lhs, op, rhs) => Box::<dyn Function>::from(op.node).eval(
                self,
                FuncCall {
                    span: op.span,
                    args: &vec![ast.get_node(*lhs), ast.get_node(*rhs)],
                },
            )?,
            ast::ExprData::PrefixOp(_, _) => todo!(),
            ast::ExprData::CmpChain(_, _) => todo!(),

            ast::ExprData::MethodCall { obj, attr, args } => todo!(),
            ast::ExprData::FuncCall { func, args } => todo!(),
            ast::ExprData::IndexOp { obj, args } => todo!(),

            ast::ExprData::VectorConstruct(_) => todo!(),
        };

        Ok(Spanned {
            node: value,
            span: expr.span(),
        })
    }
}
