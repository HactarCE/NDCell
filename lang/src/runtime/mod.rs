use codemap::{Span, Spanned};
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::data::{SpannedValueExt, Value};
use crate::errors::{Error, Result};

/// NDCA interpreter state.
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

    pub fn run_init(&mut self, ast: &ast::Program) -> Result<()> {
        for &directive_id in ast.directives() {
            match ast.get_node(directive_id).data() {
                ast::DirectiveData::Init(block) => match self.exec_stmt(ast.get_node(*block))? {
                    Flow::Proceed => (),

                    Flow::Break(span) => return Err(Error::break_not_in_loop(span)),
                    Flow::Continue(span) => return Err(Error::continue_not_in_loop(span)),

                    Flow::Return(span, _) => return Err(Error::return_not_in_fn(span)),
                    Flow::Remain(span) => return Err(Error::remain_not_in_fn(span)),
                    Flow::Become(span, _) => return Err(Error::become_not_in_fn(span)),
                },
            }
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: ast::Stmt<'_>) -> Result<Flow> {
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

                let (lhs_assign_fn, lhs_value) = self.eval_expr_assignable(lhs)?;

                let op_span = op.span;
                if let Some(op) = Option::<ast::BinaryOp>::from(op.node) {
                    rhs_value = self.eval_bin_op(
                        lhs_value?,
                        Spanned {
                            span: op_span,
                            node: op,
                        },
                        rhs_value,
                    )?;
                }

                lhs_assign_fn(self, rhs_value)?;

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
                    let msg = match msg {
                        Some(s) => s.node.as_ref(),
                        None => "Assertion failed",
                    };
                    Err(Error::custom(stmt.span(), msg))
                } else {
                    Ok(Flow::Proceed)
                }
            }
            ast::StmtData::Error { msg } => {
                let msg = match msg {
                    Some(s) => s.node.as_ref(),
                    None => "Error",
                };
                Err(Error::custom(stmt.span(), msg))
            }

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
    ) -> Result<(
        Box<dyn 'ast + FnOnce(&mut Runtime, Spanned<Value>) -> Result<()>>,
        Result<Spanned<Value>>,
    )> {
        let ast = expr.ast;
        match expr.data() {
            ast::ExprData::Identifier(var_name) => {
                // TODO: when #[feature(hash_raw_entry)] stabalizes, use
                // that here to avoid the extra `Arc::clone()` (and
                // consider changing `vars` to a `HashMap<String,
                // Value>`)
                Ok((
                    Box::new(move |state, new_value| {
                        state.vars.insert(Arc::clone(var_name), new_value.node);
                        Ok(())
                    }),
                    self.eval_expr(expr),
                ))
            }
            ast::ExprData::MethodCall { obj, attr, args } => Err(Error::unimplemented(expr.span())),
            ast::ExprData::IndexOp { obj, args } => Err(Error::unimplemented(expr.span())),

            _ => Err(Error::cannot_assign_to(expr.span())),
        }
    }

    pub fn eval_expr(&mut self, expr: ast::Expr<'_>) -> Result<Spanned<Value>> {
        let ast = expr.ast;
        let value = match expr.data() {
            ast::ExprData::Paren(expr_id) => self.eval_expr(ast.get_node(*expr_id))?.node,

            ast::ExprData::Identifier(var_name) => self
                .vars
                .get(var_name)
                .cloned()
                .ok_or_else(|| Error::uninitialized_variable(expr.span()))?,

            ast::ExprData::Constant(value) => value.clone(),

            ast::ExprData::BinaryOp(lhs, op, rhs) => {
                let lhs = self.eval_expr(ast.get_node(*lhs))?;
                let rhs = self.eval_expr(ast.get_node(*rhs))?;
                self.eval_bin_op(lhs, *op, rhs)?.node
            }
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

    fn eval_bin_op(
        &mut self,
        lhs: Spanned<Value>,
        op: Spanned<ast::BinaryOp>,
        rhs: Spanned<Value>,
    ) -> Result<Spanned<Value>> {
        let span = op.span.merge(lhs.span).merge(rhs.span);
        let value = match op.node {
            ast::BinaryOp::Add => match lhs.as_integer()?.checked_add(rhs.as_integer()?) {
                Some(sum) => Value::Integer(sum),
                None => Err(Error::integer_overflow(op.span))?,
            },
            ast::BinaryOp::Sub => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Mul => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Div => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Mod => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Pow => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Shl => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::ShrSigned => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::ShrUnsigned => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::BitwiseAnd => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::BitwiseOr => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::BitwiseXor => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::LogicalAnd => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::LogicalOr => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::LogicalXor => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Range => Err(Error::unimplemented(op.span))?,
            ast::BinaryOp::Is => Err(Error::unimplemented(op.span))?,
        };
        Ok(Spanned { node: value, span })
        // match (lhs, rhs) {
        //     (Value::Integer(_), Value::Integer(_)) => {}
        //     (Value::Integer(_), Value::Cell(_)) => {}
        //     (Value::Integer(_), Value::Vector(_)) => {}
        //     (Value::Vector(_), Value::Integer(_)) => {}
        //     (Value::Vector(_), Value::Vector(_)) => {}
        //     (Value::Array(_), Value::Integer(_)) => {}
        //     (Value::Array(_), Value::Vector(_)) => {}
        //     (Value::IntegerSet(_), Value::Integer(_)) => {}
        //     (Value::VectorSet(_), Value::Integer(_)) => {}
        //     (Value::VectorSet(_), Value::Vector(_)) => {}
        //     (Value::Pattern(_), Value::Integer(_)) => {}
        //     (Value::Pattern(_), Value::Vector(_)) => {}
        // }
        // match op.node {
        //     ast::BinaryOp::Compare(_) => {}
        //     ast::BinaryOp::Add => {}
        //     ast::BinaryOp::Sub => {}
        //     ast::BinaryOp::Mul => {}
        //     ast::BinaryOp::Div => {}
        //     ast::BinaryOp::Mod => {}
        //     ast::BinaryOp::Pow => {}
        //     ast::BinaryOp::Shl => {}
        //     ast::BinaryOp::ShrSigned => {}
        //     ast::BinaryOp::ShrUnsigned => {}
        //     ast::BinaryOp::BitwiseAnd => {}
        //     ast::BinaryOp::BitwiseOr => {}
        //     ast::BinaryOp::BitwiseXor => {}
        //     ast::BinaryOp::LogicalAnd => {}
        //     ast::BinaryOp::LogicalOr => {}
        //     ast::BinaryOp::LogicalXor => {}
        //     ast::BinaryOp::Range => {}
        //     ast::BinaryOp::Is => {}
        // }
    }
}
