use codemap::{Span, Spanned};
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use super::functions::{self, CallInfo, Function};
use crate::data::{CpVal, LangInt, RtVal, SpannedRuntimeValueExt, Val};
use crate::errors::{AlreadyReported, Error, Fallible};
use crate::exec::{Compiler, CtxTrait, Runtime};
use crate::{ast, llvm};

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
                let op_span = op.span;
                let lhs = ast.get_node(*lhs);
                let rhs = ast.get_node(*rhs);
                let args = [lhs, rhs];

                let make_func_call = |f: Option<Box<dyn Function>>| -> Box<FuncCall<'ast>> {
                    Box::new(FuncCall {
                        f,
                        f_span: op_span,
                        args: vec![(None, lhs), (None, rhs)],
                    })
                };

                match op.node {
                    Add | Sub | Mul | Div | Mod | Pow | Shl | ShrSigned | ShrUnsigned
                    | BitwiseAnd | BitwiseOr | BitwiseXor => make_func_call(
                        Option::<functions::math::BinaryMathOp>::from(op.node).map(|f| f.boxed()),
                    ),
                    LogicalAnd => Box::new(LogicalAndExpr { op_span, args }),
                    LogicalOr => Box::new(LogicalOrExpr { op_span, args }),
                    LogicalXor => make_func_call(Some(functions::bools::LogicalXor.boxed())),
                    Range => make_func_call(Some(functions::range::Range.boxed())),
                    Is => todo!("'Is' func"),
                }
            }
            ast::ExprData::PrefixOp(op, arg) => {
                use crate::ast::PrefixOp::*;
                let op_func: Box<dyn Function> = match op.node {
                    Pos => functions::math::UnaryMathOp::Pos.boxed(),
                    Neg => functions::math::UnaryMathOp::Neg.boxed(),
                    BitwiseNot => functions::math::UnaryMathOp::BitwiseNot.boxed(),
                    LogicalNot => functions::bools::LogicalNot.boxed(),
                    IntToCell => functions::cells::IntToCell.boxed(),
                };
                Box::new(FuncCall {
                    f: Some(op_func),
                    f_span: op.span,
                    args: vec![(None, ast.get_node(*arg))],
                })
            }
            ast::ExprData::CmpChain(_, _) => {
                todo!("cmp chain expr")
            }

            ast::ExprData::FuncCall { func, args } => Box::new(FuncCall {
                f: super::resolve_function(&func.node),
                f_span: func.span,
                args: get_arg_nodes(ast, args),
            }),
            ast::ExprData::MethodCall { attr, args } => Box::new(MethodCall {
                attr: Spanned {
                    // `.map_node()` doesn't work here; I tried.
                    node: &attr.node,
                    span: attr.span,
                },
                args: get_arg_nodes(ast, &args.node),
            }),
            ast::ExprData::IndexOp { args } => match ast.get_node(args[0]).data() {
                ast::ExprData::Identifier(s) if s.as_str() == "__compiled_arg__" => {
                    // `[1..]` omits the `__compiled_arg__` expression.
                    Box::new(CompiledArg(ast.get_node_list(&args[1..])))
                }
                _ => Box::new(Index {
                    bracket_span: ast.get_node(args[0]).span(),
                    args: ast
                        .get_node_list(args)
                        .into_iter()
                        .map(|expr| (None, expr))
                        .collect(),
                }),
            },

            ast::ExprData::VectorConstruct(components) => Box::new(FuncCall {
                f: Some(functions::vectors::VectorConstruct.boxed()),
                f_span: expr.span(),
                args: ast
                    .get_node_list(components)
                    .into_iter()
                    .map(|expr| (None, expr))
                    .collect(),
            }),
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

#[derive(Debug)]
struct FuncCall<'ast> {
    /// Function to call.
    f: Option<Box<dyn Function>>,
    /// Span of the function name.
    f_span: Span,
    /// Arguments to the function, each with an optional keyword.
    args: Vec<ast::FuncArg<ast::Expr<'ast>>>,
}
impl Expression for FuncCall<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        let span = self.f_span;
        let args = eval_args_list(runtime, &self.args)?;

        match &self.f {
            None => Err(runtime.error(Error::no_such_function(span))),
            Some(f) => {
                check_kwargs(runtime, f, &args)?;
                f.eval(runtime.ctx(), CallInfo::new(span, args))
            }
        }
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let span = self.f_span;
        let args = compile_args_list(compiler, &self.args)?;

        match &self.f {
            None => Err(compiler.error(Error::no_such_function(span))),
            Some(f) => {
                check_kwargs(compiler, f, &args)?;
                if let Some(args) = all_rt_vals(&args) {
                    // All arguments are compile-time constants, so compile-time
                    // evaluate the function call.
                    f.eval(compiler.ctx(), CallInfo::new(span, args))
                        .map(Val::Rt)
                } else {
                    f.compile(compiler, CallInfo::new(span, args))
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct LogicalOrExpr<'ast> {
    /// Span of the operator.
    op_span: Span,
    /// Arguments to the operator.
    args: [ast::Expr<'ast>; 2],
}
impl Expression for LogicalOrExpr<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        Ok(RtVal::Integer(
            (runtime.eval_bool_expr(self.args[0])? || runtime.eval_bool_expr(self.args[1])?)
                as LangInt,
        ))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let lhs = compiler.build_expr(self.args[0])?;

        match lhs.node {
            Val::Rt(_) => {
                let l = compiler.get_rt_val(lhs).unwrap();
                let l_bool = l.to_bool().map_err(|e| compiler.error(e))?;
                match l_bool {
                    // LHS is statically `true`; do not compile RHS.
                    true => Ok(Val::Rt(RtVal::Integer(1))),
                    // LHS is statically `false`; compile RHS, convert to bool,
                    // and return it.
                    false => {
                        let rhs = compiler.build_expr(self.args[1])?;
                        Ok(Val::Cp(CpVal::Integer(
                            compiler.build_convert_to_bool(rhs)?,
                        )))
                    }
                }
            }
            Val::Cp(_) => {
                let l_bool = compiler.build_convert_to_bool(lhs)?;
                let bool_result = compiler.build_conditional(
                    l_bool,
                    // LHS is dynamically `true`; do not evaluate RHS.
                    |_| Ok(llvm::const_int(1)),
                    // RHS is dynamically `true`; compile RHS, convert to bool,
                    // and return it.
                    |c| {
                        let rhs = c.build_expr(self.args[1])?;
                        c.build_convert_to_bool(rhs)
                    },
                )?;
                Ok(Val::Cp(CpVal::Integer(bool_result)))
            }
            Val::Unknown(_) => Err(compiler.error(Error::ambiguous_variable_type(lhs.span))),
            Val::MaybeUninit => Err(compiler.error(Error::maybe_uninitialized_variable(lhs.span))),
            Val::Err(AlreadyReported) => Err(AlreadyReported),
        }
    }
}

#[derive(Debug, Clone)]
struct LogicalAndExpr<'ast> {
    /// Span of the operator.
    op_span: Span,
    /// Arguments to the operator.
    args: [ast::Expr<'ast>; 2],
}
impl Expression for LogicalAndExpr<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        Ok(RtVal::Integer(
            (runtime.eval_bool_expr(self.args[0])? && runtime.eval_bool_expr(self.args[1])?)
                as LangInt,
        ))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let lhs = compiler.build_expr(self.args[0])?;

        match lhs.node {
            Val::Rt(_) => {
                let l = compiler.get_rt_val(lhs).unwrap();
                let l_bool = l.to_bool().map_err(|e| compiler.error(e))?;
                match l_bool {
                    // LHS is statically `true`; compile RHS, convert to bool,
                    // and return it.
                    true => {
                        let rhs = compiler.build_expr(self.args[1])?;
                        Ok(Val::Cp(CpVal::Integer(
                            compiler.build_convert_to_bool(rhs)?,
                        )))
                    }
                    // LHS is statically `false`; do not compile RHS.
                    false => Ok(Val::Rt(RtVal::Integer(0))),
                }
            }
            Val::Cp(_) => {
                let l_bool = compiler.build_convert_to_bool(lhs)?;
                let bool_result = compiler.build_conditional(
                    l_bool,
                    // LHS is dynamically `true`; compile RHS, convert to bool,
                    // and return it.
                    |c| {
                        let rhs = c.build_expr(self.args[1])?;
                        c.build_convert_to_bool(rhs)
                    },
                    // RHS is dynamically `false`; do not evaluate RHS.
                    |_| Ok(llvm::const_int(0)),
                )?;
                Ok(Val::Cp(CpVal::Integer(bool_result)))
            }
            Val::Unknown(_) => Err(compiler.error(Error::ambiguous_variable_type(lhs.span))),
            Val::MaybeUninit => Err(compiler.error(Error::maybe_uninitialized_variable(lhs.span))),
            Val::Err(AlreadyReported) => Err(AlreadyReported),
        }
    }
}

#[derive(Debug, Clone)]
struct MethodCall<'ast> {
    /// Name of method to call.
    attr: Spanned<&'ast str>,
    /// Arguments to the method, including the receiver, each with an optional
    /// keyword. **Calling `eval()` or `compile()` panics if this list is
    /// empty.**
    args: Vec<ast::FuncArg<ast::Expr<'ast>>>,
}
impl Expression for MethodCall<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        let span = self.attr.span;
        let args = eval_args_list(runtime, &self.args)?;

        // Resolve the method based on the type of the method receiver, which is
        // the first argument.
        let receiver = &args
            .first()
            .ok_or_else(|| runtime.error(internal_error_value!("method call has no receiver")))?
            .1;
        let receiver_type = receiver.node.ty();
        let f = super::resolve_method(&receiver_type, &self.attr.node)
            .ok_or_else(|| runtime.error(Error::no_such_method(span, receiver_type)))?;

        check_kwargs(runtime, &f, &args)?;
        f.eval(runtime.ctx(), CallInfo::new(span, args))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let span = self.attr.span;
        let args = compile_args_list(compiler, &self.args)?;

        // Resolve the method based on the type of the method receiver, which is
        // the first argument.
        let receiver = &args
            .first()
            .ok_or_else(|| compiler.error(internal_error_value!("method call has no receiver")))?
            .1;
        let receiver_type = compiler.get_val_type(&receiver)?.node;
        let f = super::resolve_method(&receiver_type, &self.attr.node)
            .ok_or_else(|| compiler.error(Error::no_such_method(span, receiver_type)))?;

        check_kwargs(compiler, &f, &args)?;
        if let Some(args) = all_rt_vals(&args) {
            // All arguments are compile-time constants, so compile-time
            // evaluate the method call.
            f.eval(compiler.ctx(), CallInfo::new(span, args))
                .map(Val::Rt)
        } else {
            f.compile(compiler, CallInfo::new(span, args))
        }
    }
}

#[derive(Debug, Clone)]
struct Index<'ast> {
    /// Span of the brackets containing the index arguments.
    bracket_span: Span,
    /// Indexing arguments, including the object being indexed. **Calling
    /// `eval()` or `compile()` panics if this list is empty.**
    args: Vec<ast::FuncArg<ast::Expr<'ast>>>,
}
impl Expression for Index<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Fallible<RtVal> {
        let span = self.bracket_span;
        let args = eval_args_list(runtime, &self.args)?;

        let obj = &args
            .first()
            .ok_or_else(|| runtime.error(internal_error_value!("index expr has no receiver")))?
            .1;
        let obj_ty = obj.node.ty();
        let f = super::resolve_index_method(&obj_ty)
            .ok_or_else(|| runtime.error(Error::cannot_index_type(span, obj_ty)))?;

        check_kwargs(runtime, &f, &args)?;
        f.eval(runtime.ctx(), CallInfo::new(span, args))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Fallible<Val> {
        let span = self.bracket_span;
        let args = compile_args_list(compiler, &self.args)?;

        let obj = &args
            .first()
            .ok_or_else(|| compiler.error(internal_error_value!("index expr has no receiver")))?
            .1;
        let obj_ty = compiler.get_val_type(obj)?.node;
        let f = super::resolve_index_method(&obj_ty)
            .ok_or_else(|| compiler.error(Error::cannot_index_type(span, obj_ty)))?;

        check_kwargs(compiler, &f, &args)?;
        if let Some(args) = all_rt_vals(&args) {
            // All arguments are compile-time constants, so compile-time
            // evaluate the index operation.
            f.eval(compiler.ctx(), CallInfo::new(span, args))
                .map(Val::Rt)
        } else {
            f.compile(compiler, CallInfo::new(span, args))
        }
    }
}

#[derive(Debug, Clone)]
struct CompiledArg<'ast>(Vec<ast::Expr<'ast>>);
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
struct Identifier<'ast>(&'ast Arc<String>);
impl Expression for Identifier<'_> {
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Fallible<RtVal> {
        if let Some(variable) = runtime.vars.get(self.0) {
            Ok(variable.clone())
        } else if let Some(constant) = super::resolve_constant(self.0, span, runtime.ctx()) {
            constant
        } else {
            Err(runtime.error(Error::uninitialized_variable(span)))
        }
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val> {
        if let Some(variable) = compiler.vars.get(self.0) {
            Ok(variable.clone())
        } else if let Some(constant) = super::resolve_constant(self.0, span, compiler.ctx()) {
            constant.map(Val::Rt)
        } else {
            Err(compiler.error(Error::uninitialized_variable(span)))
        }
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
struct Constant<'a>(&'a RtVal);
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
    if let Some(op_func) = Option::<functions::math::BinaryMathOp>::from(op.node) {
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
    if let Some(op_func) = Option::<functions::math::BinaryMathOp>::from(op.node) {
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
fn all_rt_vals(xs: &[ast::FuncArg<Spanned<Val>>]) -> Option<Vec<ast::FuncArg<Spanned<RtVal>>>> {
    xs.iter()
        .cloned()
        .map(|(k, v)| {
            let v = Spanned {
                span: v.span,
                node: v.node.rt_val()?,
            };
            Some((k, v))
        })
        .collect()
}

fn get_arg_nodes<'ast>(
    ast: &'ast ast::Program,
    ids: &[ast::FuncArg<ast::ExprId>],
) -> Vec<ast::FuncArg<ast::Expr<'ast>>> {
    ids.iter()
        .cloned()
        .map(|(kw, id)| (kw, ast.get_node(id)))
        .collect()
}

fn eval_args_list<'ast>(
    runtime: &mut Runtime,
    args: &[ast::FuncArg<ast::Expr<'_>>],
) -> Fallible<Vec<ast::FuncArg<Spanned<RtVal>>>> {
    args.iter()
        .cloned()
        .map(|(k, v)| Ok((k, runtime.eval_expr(v)?)))
        .collect()
}
fn compile_args_list(
    compiler: &mut Compiler,
    args: &[ast::FuncArg<ast::Expr<'_>>],
) -> Fallible<Vec<ast::FuncArg<Spanned<Val>>>> {
    args.iter()
        .cloned()
        .map(|(k, v)| Ok((k, compiler.build_expr(v)?)))
        .collect()
}

fn check_kwargs<C: CtxTrait, V>(
    ctx: &mut C,
    func: &impl Function,
    args: &[ast::FuncArg<Spanned<V>>],
) -> Fallible<()> {
    let allowed_kwarg_keys = func.kwarg_keys();
    let mut kwargs_present = vec![false; allowed_kwarg_keys.len()];
    for (key, expr) in args {
        if let Some(key) = key {
            if let Some(i) = allowed_kwarg_keys.iter().position(|&k| k == &***key) {
                if kwargs_present[i] {
                    return Err(ctx.error(Error::duplicate_keyword_argument(expr.span)));
                }
                kwargs_present[i] = true;
            } else {
                return Err(ctx.error(Error::invalid_keyword_argument(expr.span, func)));
            }
        }
    }
    Ok(())
}
