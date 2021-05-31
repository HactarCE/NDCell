use codemap::{Span, Spanned};
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use super::functions::{self, CallInfo, Function};
use crate::ast;
use crate::data::{RtVal, SpannedRuntimeValueExt, Val};
use crate::errors::{Error, Fallible};
use crate::exec::{Compiler, CtxTrait, Runtime};

/// Expression that can be evaluated and/or compiled, including any relevant
/// arguments.
pub trait Expression: fmt::Debug {
    /// Evaluates the expression, including any necessary sub-expressions, and
    /// returns the resulting value.
    ///
    /// `span` is the span of the whole expression.
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Fallible<RtVal>;
    /// Compiles code to evaluate the expression, including any necessary
    /// sub-expressions, and returns the resulting value.
    ///
    /// `span` is the span of the whole expression.
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val>;

    /// Assigns a new value to the expression.
    ///
    /// `span` is the span of the expression being assigned to.
    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        _op: Spanned<ast::AssignOp>,
        _new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        Err(runtime.error(Error::cannot_assign_to(span)))
    }
    /// Compiles code to assign a new value to the expression.
    ///
    /// `span` is the span of the expression being assigned to.
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        _op: Spanned<ast::AssignOp>,
        _new_value: Spanned<Val>,
    ) -> Fallible<()> {
        Err(compiler.error(Error::cannot_assign_to(span)))
    }
}

impl<'ast> From<ast::Expr<'ast>> for Box<dyn 'ast + Expression> {
    fn from(expr: ast::Expr<'ast>) -> Self {
        let ast = expr.ast;
        match expr.data() {
            ast::ExprData::Paren(expr) => Box::new(Identity(ast.get_node(*expr))),

            ast::ExprData::Identifier(name) => Box::new(Identifier(name)),

            ast::ExprData::Constant(v) => Box::new(Constant(v)),

            ast::ExprData::BinaryOp(lhs, op, rhs) => {
                use crate::ast::BinaryOp::*;
                match op.node {
                    Add | Sub | Mul | Div | Mod | Pow | Shl | ShrSigned | ShrUnsigned
                    | BitwiseAnd | BitwiseOr | BitwiseXor => Box::new(FuncCall {
                        f: Option::<functions::math::BinaryOp>::from(op.node),
                        f_span: op.span,
                        args: vec![ast.get_node(*lhs), ast.get_node(*rhs)],
                    }),
                    LogicalAnd => todo!("'LogicalAnd' expr"),
                    LogicalOr => todo!("'LogicalOr' expr"),
                    LogicalXor => todo!("'LogicalXor' expr"),
                    Range => todo!("'Range' func"),
                    Is => todo!("'Is' func"),
                }
            }
            ast::ExprData::PrefixOp(op, arg) => {
                use crate::ast::PrefixOp::*;
                let op_func: Box<dyn Function> = match op.node {
                    Pos => Box::new(functions::math::UnaryOp::Pos),
                    Neg => Box::new(functions::math::UnaryOp::Neg),
                    BitwiseNot => Box::new(functions::math::UnaryOp::BitwiseNot),
                    LogicalNot => Box::new(functions::math::UnaryOp::LogicalNot),
                    IntToCell => Box::new(functions::convert::IntToCell),
                };
                Box::new(FuncCall {
                    f: Some(op_func),
                    f_span: op.span,
                    args: vec![ast.get_node(*arg)],
                })
            }
            ast::ExprData::CmpChain(_, _) => {
                todo!("cmp chain expr")
            }

            ast::ExprData::FuncCall { func, args } => Box::new(FuncCall {
                f: super::resolve_function(&func.node),
                f_span: func.span,
                args: ast.get_node_list(args),
            }),
            ast::ExprData::MethodCall { attr, args } => Box::new(MethodCall {
                attr: Spanned {
                    // `.map_node()` doesn't work here; I tried.
                    node: &attr.node,
                    span: attr.span,
                },
                args: ast.get_node_list(args),
            }),
            ast::ExprData::IndexOp { args } => match ast.get_node(args[0]).data() {
                ast::ExprData::Identifier(s) if s.as_str() == "__compiled_arg__" => {
                    // `[1..]` omits the `__compiled_arg__` expression.
                    Box::new(CompiledArg(ast.get_node_list(&args[1..])))
                }
                _ => Box::new(MethodCall {
                    attr: Spanned {
                        node: "index",
                        span: args.span,
                    },
                    args: ast.get_node_list(args),
                }),
            },

            ast::ExprData::VectorConstruct(_) => {
                todo!("vector construct expr")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identity<'ast>(ast::Expr<'ast>);
impl Expression for Identity<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        Ok(runtime.eval_expr(self.0)?.node)
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        Ok(compiler.build_expr(self.0)?.node)
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall<'ast, F> {
    /// Function to call.
    f: Option<F>,
    /// Span of the function name.
    f_span: Span,
    /// Arguments to the function.
    args: Vec<ast::Expr<'ast>>,
}
impl<F: Function> Expression for FuncCall<'_, F> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        let args = runtime.eval_expr_list(&self.args)?;
        let span = self.f_span;

        match &self.f {
            Some(f) => f.eval(runtime.ctx(), CallInfo { span, args }),
            None => Err(runtime.error(Error::no_such_function(span))),
        }
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let args = compiler.build_expr_list(&self.args)?;
        let span = self.f_span;

        let f = self
            .f
            .as_ref()
            .ok_or_else(|| compiler.error(Error::no_such_function(span)))?;
        if let Some(args) = all_rt_vals(&args) {
            // All arguments are compile-time constants, so compile-time
            // evaluate the function call.
            let x = f.eval(compiler.ctx(), CallInfo { span, args }).map(Val::Rt);
            x
        } else {
            f.compile(compiler, CallInfo { span, args })
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodCall<'ast> {
    /// Name of method to call.
    attr: Spanned<&'ast str>,
    /// Arguments to the method, including the receiver. **Calling `eval()` or
    /// `compile()` panics if this list is empty.**
    args: Vec<ast::Expr<'ast>>,
}
impl Expression for MethodCall<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        let args = runtime.eval_expr_list(&self.args)?;
        let span = self.attr.span;

        let receiver = args.first().expect("method call has no receiver");
        let receiver_ty = receiver.node.ty();
        let f = super::resolve_method(&receiver_ty, &self.attr.node)
            .ok_or_else(|| runtime.error(Error::no_such_method(span, receiver_ty)))?;
        f.eval(runtime.ctx(), CallInfo { span, args })
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let args = compiler.build_expr_list(&self.args)?;
        let span = self.attr.span;

        let receiver = args.first().expect("method call has no receiver");
        let receiver_ty = compiler.get_val_type(receiver)?.node;
        let f = super::resolve_method(&receiver_ty, &self.attr.node)
            .ok_or_else(|| compiler.error(Error::no_such_method(span, receiver_ty)))?;
        if let Some(args) = all_rt_vals(&args) {
            // All arguments are compile-time constants, so compile-time
            // evaluate the method call.
            f.eval(compiler.ctx(), CallInfo { span, args }).map(Val::Rt)
        } else {
            f.compile(compiler, CallInfo { span, args })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Index<'ast> {
    /// Indexing arguments, including the object being indexed. **Calling
    /// `eval()` or `compile()` panics if this list is empty.**
    args: Spanned<Vec<ast::Expr<'ast>>>,
}
impl Expression for Index<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        let args = runtime.eval_expr_list(&self.args.node)?;
        let span = self.args.span;

        let obj = args.first().expect("method call has no receiver");
        let obj_ty = obj.node.ty();
        let f = super::resolve_index_method(&obj_ty)
            .ok_or_else(|| runtime.error(Error::cannot_index_type(span, obj_ty)))?;
        f.eval(runtime.ctx(), CallInfo { span, args })
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let args = compiler.build_expr_list(&self.args.node)?;
        let span = self.args.span;

        let obj = args.first().expect("method call has no receiver");
        let obj_ty = compiler.get_val_type(obj)?.node;
        let f = super::resolve_index_method(&obj_ty)
            .ok_or_else(|| compiler.error(Error::cannot_index_type(span, obj_ty)))?;
        f.compile(compiler, CallInfo { span, args })
    }
}

#[derive(Debug, Clone)]
pub struct CompiledArg<'ast>(Vec<ast::Expr<'ast>>);
impl<'ast> CompiledArg<'ast> {
    fn arg_index(&self, compiler: &mut Compiler, span: Span) -> Fallible<Spanned<u32>> {
        // There should be exactly one expression.
        let index_expr = *self
            .0
            .iter()
            .exactly_one()
            .map_err(|_| compiler.error(Error::custom(span, "expected exactly one index")))?;
        let index_value = compiler.build_expr(index_expr)?;
        let span = index_expr.span();
        // It should be const-evaluatable
        let index = compiler
            .get_rt_val(index_value)?
            // ... should be an integer
            .as_integer()
            .map_err(|e| compiler.error(e))?
            // ... should fit in `u32`
            .try_into()
            .map_err(|_| compiler.error(Error::custom(span, "compiled arg index out of range")))?;
        // ... and it should be within range.
        if index < compiler.param_types().len() as u32 {
            Ok(Spanned {
                node: index,
                span: index_expr.span(),
            })
        } else {
            Err(compiler.error(Error::custom(span, "compiled arg index out of range")))
        }
    }
}
impl Expression for CompiledArg<'_> {
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Fallible<RtVal> {
        Err(runtime.error(Error::cannot_const_eval(span)))
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val> {
        let index = self.arg_index(compiler, span)?;
        compiler.build_load_arg(index).map(Val::Cp)
    }

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        _op: Spanned<ast::AssignOp>,
        _new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        Err(runtime.error(Error::cannot_const_eval(span)))
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        op: Spanned<ast::AssignOp>,
        new_value: Spanned<Val>,
    ) -> Fallible<()> {
        let index = self.arg_index(compiler, span)?;
        let old_value_fn = |c: &mut Compiler| c.build_load_arg(index).map(Val::Cp);
        let new_arg_value = compile_assign_op(compiler, old_value_fn, span, op, new_value)?;
        compiler.build_store_arg(index, new_arg_value)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier<'ast>(&'ast Arc<String>);
impl Expression for Identifier<'_> {
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Fallible<RtVal> {
        runtime
            .vars
            .get(self.0)
            .cloned()
            .or_else(|| super::resolve_constant(self.0))
            .ok_or_else(|| runtime.error(Error::uninitialized_variable(span)))
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val> {
        compiler
            .vars
            .get(self.0)
            .cloned()
            .or_else(|| super::resolve_constant(self.0).map(Val::Rt))
            .ok_or_else(|| compiler.error(Error::uninitialized_variable(span)))
            .map(|node| Spanned { node, span })
            .map(|v| v.node)
    }

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        op: Spanned<ast::AssignOp>,
        new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        let new_value =
            eval_assign_op(runtime, |rt| self.eval(rt, span), span, op, new_value)?.node;
        runtime.vars.insert(Arc::clone(self.0), new_value);
        Ok(())
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        op: Spanned<ast::AssignOp>,
        new_value: Spanned<Val>,
    ) -> Fallible<()> {
        let new_value =
            compile_assign_op(compiler, |c| self.compile(c, span), span, op, new_value)?.node;
        compiler.vars.insert(Arc::clone(self.0), new_value);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Constant<'a>(&'a RtVal);
impl Expression for Constant<'_> {
    fn eval(&self, _runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        Ok(self.0.clone())
    }
    fn compile(&self, _compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        Ok(Val::Rt(self.0.clone()))
    }
}

fn eval_assign_op(
    runtime: &mut Runtime,
    old_value_fn: impl FnOnce(&mut Runtime) -> Fallible<RtVal>,
    old_value_span: Span,
    op: Spanned<ast::AssignOp>,
    mut new_value: Spanned<RtVal>,
) -> Fallible<Spanned<RtVal>> {
    if let Some(op_func) = Option::<functions::math::BinaryOp>::from(op.node) {
        let old_value = Spanned {
            node: old_value_fn(runtime)?,
            span: old_value_span,
        };
        let span = old_value.span.merge(new_value.span);
        new_value = Spanned {
            node: op_func.eval_on_values(runtime.ctx(), op.span, old_value, new_value)?,
            span,
        };
    }
    Ok(new_value)
}
fn compile_assign_op(
    compiler: &mut Compiler,
    old_value_fn: impl FnOnce(&mut Compiler) -> Fallible<Val>,
    old_value_span: Span,
    op: Spanned<ast::AssignOp>,
    mut new_value: Spanned<Val>,
) -> Fallible<Spanned<Val>> {
    if let Some(op_func) = Option::<functions::math::BinaryOp>::from(op.node) {
        let old_value = Spanned {
            node: old_value_fn(compiler)?,
            span: old_value_span,
        };
        let span = old_value.span.merge(new_value.span);
        new_value = Spanned {
            node: op_func.compile_for_values(compiler, op.span, old_value, new_value)?,
            span,
        };
    }
    Ok(new_value)
}

/// Returns a list of `RtVal`s if all the values are compile-time constants, or
/// `None` if any of them is not.
fn all_rt_vals(xs: &[Spanned<Val>]) -> Option<Vec<Spanned<RtVal>>> {
    xs.iter()
        .cloned()
        .map(|v| {
            Some(Spanned {
                span: v.span,
                node: v.node.rt_val()?,
            })
        })
        .collect()
}
