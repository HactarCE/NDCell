use codemap::{Span, Spanned};
use std::collections::HashMap;
use std::sync::Arc;

use super::builtins::Expression;
use crate::ast;
use crate::data::{GetType, RtVal, SpannedRuntimeValueExt};
use crate::errors::{Error, Result};
use crate::exec::{Ctx, CtxTrait};

/// NDCA runtime state.
#[derive(Debug, Clone)]
pub struct Runtime {
    /// Global initialization execution context.
    pub(super) ctx: Ctx,

    /// Variable values.
    pub vars: HashMap<Arc<String>, RtVal>,
}

impl CtxTrait for Runtime {
    fn ctx(&mut self) -> &mut Ctx {
        &mut self.ctx
    }
}

/// Control flow command.
#[must_use]
#[derive(Debug, Clone)]
pub enum Flow {
    /// Execute the next statement as normal.
    Proceed,

    /// Break out of the loop (contains span of `break` statement).
    Break(Span),
    /// Continue to the next loop iteration (contains span of `continue`
    /// statement).
    Continue(Span),

    /// Return from a function (contains span of `return` statement and value
    /// returned).
    Return(Span, Option<Spanned<RtVal>>),
    /// Return from the transition function without modifying the cell (contains
    /// span of `remain` statement).
    Remain(Span),
    /// Return from the transition function (contains span of `become` statement
    /// and value returned).
    Become(Span, Spanned<RtVal>),
}
impl Default for Flow {
    fn default() -> Self {
        Self::Proceed
    }
}

impl Runtime {
    /// Constructs a new runtime.
    pub fn init_new(ast: &ast::Program) -> Self {
        let mut this = Self {
            ctx: Ctx::new(&ast),
            vars: HashMap::new(),
        };

        for directive in ast.directives() {
            let result = match directive.data() {
                ast::DirectiveData::Compile { .. } => match this.ctx.compile_directive {
                    Some(_) => {
                        // Lie to the user; tell them this directive name is
                        // invalid, because they shouldn't be using it!
                        Err(Error::invalid_directive_name(directive.span()))
                    }
                    None => {
                        this.ctx.compile_directive = Some(directive.id);
                        Ok(())
                    }
                },

                ast::DirectiveData::Init(block) => {
                    this.exec_stmt(ast.get_node(*block))
                        .and_then(|flow| match flow {
                            Flow::Proceed => Ok(()),

                            Flow::Break(span) => Err(Error::break_not_in_loop(span)),
                            Flow::Continue(span) => Err(Error::continue_not_in_loop(span)),

                            Flow::Return(span, _) => Err(Error::return_not_in_fn(span)),
                            Flow::Remain(span) => Err(Error::remain_not_in_fn(span)),
                            Flow::Become(span, _) => Err(Error::become_not_in_fn(span)),
                        })
                }

                ast::DirectiveData::Ndim(expr) => this
                    .eval_expr(ast.get_node(*expr))
                    .and_then(|ndim_value| this.ctx.set_ndim(directive.span(), &ndim_value)),

                ast::DirectiveData::Radius(expr) => this
                    .eval_expr(ast.get_node(*expr))
                    .and_then(|radius_value| this.ctx.set_radius(directive.span(), &radius_value)),

                ast::DirectiveData::States(expr) => this
                    .eval_expr(ast.get_node(*expr))
                    .and_then(|states_value| this.ctx.set_states(directive.span(), &states_value)),
            };

            // Abort if there is an error.
            if let Err(e) = result {
                this.report_error(e);
                break;
            }
        }

        // Report an error for missing values only if there are no other errors
        // already.
        if this.ctx.errors.is_empty() {
            if let Err(e) = this.ctx.error_if_missing_values() {
                this.report_error(e);
            }
        }

        this
    }

    /// Assigns a value to a variable.
    pub fn assign_var(&mut self, name: &Arc<String>, value: RtVal) {
        if &**name != crate::THROWAWAY_VARIABLE {
            self.vars.insert(Arc::clone(name), value);
        }
    }
    /// Returns all variable values.
    pub fn vars(&self) -> &HashMap<Arc<String>, RtVal> {
        &self.vars
    }

    /// Executes a statement.
    pub fn exec_stmt(&mut self, stmt: ast::Stmt<'_>) -> Result<Flow> {
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

            ast::StmtData::Assign { lhs, rhs } => {
                let lhs = ast.get_node(*lhs);
                let rhs = ast.get_node(*rhs);
                let expr_span = lhs.span();
                let stmt_span = lhs.span().merge(rhs.span());
                let new_value = self.eval_expr(rhs)?;
                let lhs_expression = Box::<dyn Expression>::from(lhs);
                lhs_expression.eval_assign(self, expr_span, stmt_span, new_value)?;
                Ok(Flow::Proceed)
            }

            ast::StmtData::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let condition = ast.get_node(*condition);
                if self.eval_bool_expr(condition)? {
                    if_true.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                } else {
                    if_false.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                }
            }

            ast::StmtData::Assert { condition, msg } => {
                let condition = ast.get_node(*condition);
                if self.eval_bool_expr(condition)? {
                    Ok(Flow::Proceed)
                } else {
                    Err(match msg {
                        Some(msg) => Error::assertion_failed_with_msg(stmt.span(), msg),
                        None => Error::assertion_failed(stmt.span()),
                    })
                }
            }
            ast::StmtData::Error { msg } => Err(match msg {
                Some(msg) => Error::user_error_with_msg(stmt.span(), msg),
                None => Error::user_error(stmt.span()),
            }),

            ast::StmtData::Break => Ok(Flow::Break(stmt.span())),
            ast::StmtData::Continue => Ok(Flow::Continue(stmt.span())),
            ast::StmtData::ForLoop {
                iter_var,
                iter_expr,
                block,
            } => {
                let iter_expr = ast.get_node(*iter_expr);
                for it in self.eval_expr(iter_expr)?.iterate()? {
                    self.assign_var(iter_var, it);
                    match self.exec_stmt(ast.get_node(*block))? {
                        Flow::Proceed | Flow::Continue(_) => (),
                        Flow::Break(_) => break,
                        flow => return Ok(flow),
                    }
                }
                Ok(Flow::Proceed)
            }
            ast::StmtData::WhileLoop { condition, block } => {
                let condition = ast.get_node(*condition);
                let block = ast.get_node(*block);
                while self.eval_bool_expr(condition)? {
                    match self.exec_stmt(block)? {
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

    /// Evaluates an expression.
    pub fn eval_expr(&mut self, expr: ast::Expr<'_>) -> Result<Spanned<RtVal>> {
        let span = expr.span();
        let expression = Box::<dyn Expression>::from(expr);
        expression
            .eval(self, span)
            .map(|v| Spanned { node: v, span })
    }
    /// Evaluates an expression and converts the result to a boolean.
    pub fn eval_bool_expr(&mut self, expr: ast::Expr<'_>) -> Result<bool> {
        self.eval_expr(expr)?.to_bool()
    }
}
