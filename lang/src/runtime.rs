use codemap::{Span, Spanned};
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::builtins::{self, FuncCall, Function};
use crate::data::{RtVal, SpannedRuntimeValueExt, Type, Val};
use crate::errors::{AlreadyReported, Error, Fallible};

pub struct AssignableValue<'f, R> {
    pub assign_fn: Box<dyn 'f + FnOnce(&mut R, Spanned<Val>) -> Fallible<()>>,
    pub get_existing_value: Box<dyn 'f + FnOnce(&mut R) -> Fallible<Spanned<Val>>>,
}
pub type AssignableRuntimeValue<'f> = AssignableValue<'f, Runtime>;

/// NDCA runtime state.
#[derive(Debug, Default, Clone)]
pub struct Runtime {
    /// Variable values.
    pub vars: HashMap<Arc<String>, Val>,
    // /// Rule name.
    // rule_name: Option<String>,
    // /// Number of dimensions.
    // ndim: Option<usize>,
    // /// Number of states.
    // states: Option<usize>,
    /// Runtime and compile-time errors.
    pub errors: Vec<Error>,

    /// `@compile` directive. (There must be exactly one.)
    pub compile_directive: Option<ast::DirectiveId>,
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

    pub(crate) fn error(&mut self, e: Error) -> AlreadyReported {
        self.errors.push(e);
        AlreadyReported
    }
    pub(crate) fn get_val_type(&mut self, v: &Spanned<Val>) -> Fallible<Spanned<Type>> {
        let span = v.span;
        match &v.node {
            Val::Rt(v) => Ok(v.ty()),
            Val::Cp(v) => Ok(v.ty()),
            Val::Unknown(Some(ty)) => Ok(ty.clone()),
            Val::Unknown(None) => Err(self.error(Error::ambiguous_variable_type(span))),
            Val::MaybeUninit => Err(self.error(Error::maybe_uninitialized_variable(span))),
            Val::Err(e) => Err(*e),
        }
        .map(|node| Spanned { node, span })
    }
    pub(crate) fn get_rt_val(&mut self, v: Spanned<Val>) -> Fallible<Spanned<RtVal>> {
        let span = v.span;
        match v.node {
            Val::Rt(v) => Ok(v),
            Val::Cp(_) => Err(self.error(Error::cannot_const_eval(span))),
            Val::Unknown(ty) => Err(self.error(Error::ambiguous_variable_type(span))),
            Val::MaybeUninit => Err(self.error(Error::maybe_uninitialized_variable(span))),
            Val::Err(e) => Err(e),
        }
        .map(|node| Spanned { node, span })
    }

    pub fn run_init(&mut self, ast: &ast::Program) -> Result<(), &[Error]> {
        for &directive_id in ast.directives() {
            let directive = ast.get_node(directive_id);
            match directive.data() {
                ast::DirectiveData::Compile { .. } => match self.compile_directive {
                    Some(_) => {
                        // Lie to the user; tell them this directive name is
                        // invalid, because they shouldn't be using it!
                        self.error(Error::invalid_directive_name(directive.span()));
                    }
                    None => {
                        self.compile_directive = Some(directive_id);
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
            }
        }
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(&self.errors)
        }
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
                let mut rhs_value = self.eval_expr(rhs)?;

                let AssignableValue {
                    assign_fn,
                    get_existing_value,
                } = self.eval_expr_assignable(lhs)?;

                if let Some(op_func) = Option::<builtins::math::BinaryOp>::from(op.node) {
                    let lhs_value = get_existing_value(self)?;
                    let lhs_value = self.get_rt_val(lhs_value)?;
                    rhs_value = Spanned {
                        node: op_func
                            .eval_on_values(op.span, lhs_value, rhs_value)
                            .map_err(|e| self.error(e))?,
                        span: lhs.span().merge(rhs.span()),
                    };
                }

                assign_fn(self, rhs_value.map_node(Val::Rt))?;

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
                    // TODO: when #[feature(hash_raw_entry)] stabalizes, use
                    // that here to avoid the extra `Arc::clone()` (and consider
                    // changing `vars` to a `HashMap<String, Val>`)
                    self.vars.insert(Arc::clone(&iter_var), Val::Rt(it.node));
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
    ) -> Fallible<AssignableRuntimeValue<'ast>> {
        let ast = expr.ast;
        match expr.data() {
            ast::ExprData::Identifier(var_name) => {
                // TODO: when #[feature(hash_raw_entry)] stabalizes, use
                // that here to avoid the extra `Arc::clone()` (and
                // consider changing `vars` to a `HashMap<String,
                // Val>`)
                Ok(AssignableValue {
                    assign_fn: Box::new(move |runtime, new_value| {
                        runtime.vars.insert(Arc::clone(var_name), new_value.node);
                        Ok(())
                    }),
                    get_existing_value: Box::new(move |runtime| {
                        runtime.eval_expr(expr).map(|x| x.map_node(Val::Rt))
                    }),
                })
            }
            ast::ExprData::MethodCall { obj, attr, args } => {
                Err(self.error(Error::unimplemented(expr.span())))
            }
            ast::ExprData::IndexOp { obj, args } => {
                Err(self.error(Error::unimplemented(expr.span())))
            }

            _ => Err(self.error(Error::cannot_assign_to(expr.span()))),
        }
    }

    pub fn eval_expr(&mut self, expr: ast::Expr<'_>) -> Fallible<Spanned<RtVal>> {
        let ast = expr.ast;
        let span = expr.span();
        let value = match expr.data() {
            ast::ExprData::Paren(expr_id) => self.eval_expr(ast.get_node(*expr_id))?.node,

            ast::ExprData::Identifier(var_name) => {
                return self
                    .vars
                    .get(var_name)
                    .cloned()
                    .or_else(|| builtins::resolve_constant(var_name))
                    .ok_or_else(|| self.error(Error::uninitialized_variable(expr.span())))
                    .map(|node| Spanned { node, span })
                    .and_then(|v| self.get_rt_val(v))
            }

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

        Ok(Spanned { node: value, span })
    }
}
