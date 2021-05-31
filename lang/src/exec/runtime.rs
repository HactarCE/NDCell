use codemap::{Span, Spanned};
use std::collections::HashMap;
use std::sync::Arc;

use super::builtins::{self, Expression};
use crate::ast;
use crate::data::{RtVal, SpannedRuntimeValueExt};
use crate::errors::{AlreadyReported, Error, Fallible};
use crate::exec::{Ctx, CtxTrait};

// TODO: consider making `vars` only `pub(super)` and adding `fn vars(&mut self) -> &mut HashMap<_, _>`

/// NDCA runtime state.
#[derive(Debug, Default, Clone)]
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn run_init(&mut self, ast: &ast::Program) -> Result<(), &[Error]> {
        let directives = ast.get_node_list(ast.directives());

        // Fill in defaults for missing directives.
        self.ctx = Ctx::new(&directives);

        for directive in directives {
            match directive.data() {
                ast::DirectiveData::Compile { .. } => match self.ctx.compile_directive {
                    Some(_) => {
                        // Lie to the user; tell them this directive name is
                        // invalid, because they shouldn't be using it!
                        self.error(Error::invalid_directive_name(directive.span()));
                    }
                    None => {
                        self.ctx.compile_directive = Some(directive.id);
                    }
                },

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

                    Err(AlreadyReported) => (),
                },

                ast::DirectiveData::Ndim(expr) => {
                    let ndim_expr_result = match self.eval_expr(ast.get_node(*expr)) {
                        Ok(x) => x,
                        Err(AlreadyReported) => break,
                    };
                    if let Err(e) = self.ctx.set_ndim(directive.span(), ndim_expr_result) {
                        self.error(e);
                        break;
                    }
                }

                ast::DirectiveData::States(expr) => {
                    let states_expr_result = match self.eval_expr(ast.get_node(*expr)) {
                        Ok(x) => x,
                        Err(AlreadyReported) => break,
                    };
                    if let Err(e) = self.ctx.set_states(directive.span(), states_expr_result) {
                        self.error(e);
                        break;
                    }
                }
            }
        }
        if !self.ctx.errors.is_empty() {
            return Err(&self.ctx.errors);
        }
        if let Err(e) = self.ctx.error_if_missing_values() {
            self.error(e);
            return Err(&self.ctx.errors);
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: ast::Stmt<'_>) -> Fallible<Flow> {
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
                let new_value = self.eval_expr(rhs)?;

                let lhs_expression = Box::<dyn builtins::Expression>::from(lhs);
                lhs_expression.eval_assign(self, lhs.span(), *op, new_value)?;

                Ok(Flow::Proceed)
            }

            ast::StmtData::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let condition = ast.get_node(*condition);
                if self
                    .eval_expr(condition)?
                    .to_bool()
                    .map_err(|e| self.error(e))?
                {
                    if_true.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                } else {
                    if_false.map_or(Ok(Flow::Proceed), |id| self.exec_stmt(ast.get_node(id)))
                }
            }

            ast::StmtData::Assert { condition, msg } => {
                let condition = ast.get_node(*condition);
                if self
                    .eval_expr(condition)?
                    .to_bool()
                    .map_err(|e| self.error(e))?
                {
                    Err(self.error(match msg {
                        Some(msg) => Error::assertion_failed_with_msg(stmt.span(), msg),
                        None => Error::assertion_failed(stmt.span()),
                    }))
                } else {
                    Ok(Flow::Proceed)
                }
            }
            ast::StmtData::Error { msg } => Err(self.error(match msg {
                Some(msg) => Error::user_error_with_msg(stmt.span(), msg),
                None => Error::user_error(stmt.span()),
            })),

            ast::StmtData::Break => Ok(Flow::Break(stmt.span())),
            ast::StmtData::Continue => Ok(Flow::Continue(stmt.span())),
            ast::StmtData::ForLoop {
                iter_var,
                iter_expr: iter_expr_id,
                block,
            } => {
                let iter_expr = ast.get_node(*iter_expr_id);
                for it in self
                    .eval_expr(iter_expr)?
                    .iterate()
                    .map_err(|e| self.error(e))?
                {
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

    pub fn eval_expr(&mut self, expr: ast::Expr<'_>) -> Fallible<Spanned<RtVal>> {
        let span = expr.span();
        let expression = Box::<dyn Expression>::from(expr);
        expression
            .eval(self, span)
            .map(|v| Spanned { node: v, span })
    }
    pub fn eval_expr_list(&mut self, exprs: &[ast::Expr<'_>]) -> Fallible<Vec<Spanned<RtVal>>> {
        exprs.iter().map(|expr| self.eval_expr(*expr)).collect()
    }
}
