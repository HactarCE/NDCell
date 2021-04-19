use codemap::{Span, Spanned};
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use super::functions::{self, CallInfo, Function};
use crate::ast;
use crate::compiler::{Compiler, ParamType};
use crate::data::{CpVal, FallibleTypeOf, RtVal, SpannedRuntimeValueExt, Type, Val};
use crate::errors::{Error, Fallible, ReportError};
use crate::llvm;
use crate::runtime::Runtime;

/// Expression that can be evaluated and/or compiled, including any relevant
/// arguments.
pub trait Expression: fmt::Debug {
    /// Evaluates the expression and returns the resulting value, with the
    /// option to lazily evaluate arguments or short-circuit.
    ///
    /// The default implementation eager evaluates all arguments in order and
    /// then passes the values to `eval()`.
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Fallible<RtVal>;
    /// Compiles code to evaluate the expression and returns the resulting
    /// value, with the option to lazily evaluate arguments or short-circuit.
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val>;

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        op: Spanned<ast::AssignOp>,
        new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        Err(runtime.error(Error::cannot_assign_to(span)))
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        op: Spanned<ast::AssignOp>,
        new_value: Spanned<Val>,
    ) -> Fallible<()> {
        Err(compiler.error(Error::cannot_assign_to(span)))
    }
}

impl<'ast> From<ast::Expr<'ast>> for Box<dyn 'ast + Expression> {
    fn from(expr: ast::Expr<'ast>) -> Self {
        let ast = expr.ast;
        match expr.data() {
            ast::ExprData::Paren(_) => todo!("identity func"),

            ast::ExprData::Identifier(name) => Box::new(Identifier(name)),

            ast::ExprData::Constant(v) => Box::new(Constant(v)),

            ast::ExprData::BinaryOp(lhs, op, rhs) => {
                use functions::math::BinaryOp;

                let make_binop_func_call = |f| {
                    Box::new(FuncCall {
                        f,
                        span: op.span,
                        args: vec![ast.get_node(*lhs), ast.get_node(*rhs)],
                    })
                };

                match op.node {
                    ast::BinaryOp::Add => make_binop_func_call(BinaryOp::Add),
                    ast::BinaryOp::Sub => make_binop_func_call(BinaryOp::Sub),
                    ast::BinaryOp::Mul => make_binop_func_call(BinaryOp::Mul),
                    ast::BinaryOp::Div => make_binop_func_call(BinaryOp::Div),
                    ast::BinaryOp::Mod => make_binop_func_call(BinaryOp::Mod),
                    ast::BinaryOp::Pow => make_binop_func_call(BinaryOp::Pow),
                    ast::BinaryOp::Shl => make_binop_func_call(BinaryOp::Shl),
                    ast::BinaryOp::ShrSigned => make_binop_func_call(BinaryOp::ShrSigned),
                    ast::BinaryOp::ShrUnsigned => make_binop_func_call(BinaryOp::ShrUnsigned),
                    ast::BinaryOp::BitwiseAnd => make_binop_func_call(BinaryOp::BitwiseAnd),
                    ast::BinaryOp::BitwiseOr => make_binop_func_call(BinaryOp::BitwiseOr),
                    ast::BinaryOp::BitwiseXor => make_binop_func_call(BinaryOp::BitwiseXor),
                    ast::BinaryOp::LogicalAnd => todo!("'LogicalAnd' expr"),
                    ast::BinaryOp::LogicalOr => todo!("'LogicalOr' expr"),
                    ast::BinaryOp::LogicalXor => todo!("'LogicalXor' expr"),
                    ast::BinaryOp::Range => todo!("'Range' func"),
                    ast::BinaryOp::Is => todo!("'Is' func"),
                }
            }
            ast::ExprData::PrefixOp(_, _) => {
                todo!("prefix op func")
            }
            ast::ExprData::CmpChain(_, _) => {
                todo!("cmp chain expr")
            }

            ast::ExprData::MethodCall { obj, attr, args } => {
                todo!("find method by name")
            }
            ast::ExprData::FuncCall { func, args } => {
                todo!("find func by name")
            }
            ast::ExprData::IndexOp { obj, args } => match ast.get_node(*obj).data() {
                ast::ExprData::Identifier(s) if s.as_str() == "__compiled_arg__" => Box::new(
                    CompiledArg(args.iter().map(|id| ast.get_node(*id)).collect()),
                ),
                _ => todo!("index op expr"),
            },

            ast::ExprData::VectorConstruct(_) => {
                todo!("vector construct expr")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall<'ast, F> {
    f: F,
    span: Span,
    args: Vec<ast::Expr<'ast>>,
}
impl<F: Function> Expression for FuncCall<'_, F> {
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Fallible<RtVal> {
        let args = self
            .args
            .iter()
            .map(|expr| runtime.eval_expr(*expr))
            .collect::<Fallible<Vec<Spanned<RtVal>>>>()?;
        let span = self.span;

        let call = CallInfo { span, args };
        self.f.eval(runtime, call)
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val> {
        let args = self
            .args
            .iter()
            .map(|expr| compiler.build_expr(*expr))
            .collect::<Fallible<Vec<Spanned<Val>>>>()?;
        let span = self.span;

        let call = CallInfo { span, args };
        self.f.compile(compiler, call)
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
        op: Spanned<ast::AssignOp>,
        new_value: Spanned<RtVal>,
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
            .map(|node| Spanned { node, span })
            .and_then(|v| runtime.get_rt_val(v))
            .map(|v| v.node)
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Fallible<Val> {
        compiler
            .runtime
            .vars
            .get(self.0)
            .cloned()
            .or_else(|| super::resolve_constant(self.0))
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
        runtime.vars.insert(Arc::clone(self.0), Val::Rt(new_value));
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
        compiler.runtime.vars.insert(Arc::clone(self.0), new_value);
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
            node: op_func.eval_on_values(runtime, op.span, old_value, new_value)?,
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
