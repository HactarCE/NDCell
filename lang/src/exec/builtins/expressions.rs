use codemap::{Span, Spanned};
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use super::functions::{self, CallInfo, Function};
use crate::data::{CpVal, LangInt, RtVal, SpannedRuntimeValueExt, TryGetType, Type, Val};
use crate::errors::{Error, Result};
use crate::exec::{Compiler, CtxTrait, Runtime};
use crate::{ast, llvm};

/// Expression that can be evaluated and/or compiled, including any relevant
/// arguments.
pub trait Expression: fmt::Debug {
    /// Evaluates the expression, including any necessary sub-expressions, and
    /// returns the resulting value.
    ///
    /// `span` is the span of the whole expression.
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Result<RtVal>;
    /// Compiles code to evaluate the expression, including any necessary
    /// sub-expressions, and returns the resulting value.
    ///
    /// `span` is the span of the whole expression.
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Result<Val>;

    /// Assigns a new value to the expression.
    ///
    /// `span` is the span of the expression being assigned to.
    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        _op: Option<Spanned<ast::AssignOp>>,
        _new_value: Spanned<RtVal>,
    ) -> Result<()> {
        Err(Error::cannot_assign_to(span))
    }
    /// Compiles code to assign a new value to the expression.
    ///
    /// `span` is the span of the expression being assigned to.
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        _op: Option<Spanned<ast::AssignOp>>,
        _new_value: Spanned<Val>,
    ) -> Result<()> {
        Err(Error::cannot_assign_to(span))
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
                        name: op.map_node(|op| Arc::new(op.to_string())),
                        args: vec![(None, lhs), (None, rhs)],
                    })
                };

                match op.node {
                    Add | Sub | Mul | Div | Mod | Pow | Shl | ShrSigned | ShrUnsigned | And
                    | Or | Xor => make_func_call(
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
                    name: op.map_node(|op| Arc::new(op.to_string())),
                    args: vec![(None, ast.get_node(*arg))],
                })
            }
            ast::ExprData::CmpChain(args, ops) => {
                let args = args.iter().map(|&id| ast.get_node(id)).collect();
                Box::new(CmpChain { args, ops })
            }

            ast::ExprData::FuncCall { func, args } => Box::new(FuncCall {
                f: super::resolve_function(&func.node),
                name: func.clone(),
                args: get_arg_nodes(ast, args),
            }),
            ast::ExprData::MethodCall { attr, obj, args } => Box::new(MethodCall {
                method: Method::MethodCall { name: attr.clone() },
                obj: ast.get_node(*obj),
                args: get_arg_nodes(ast, &args.node),
            }),
            ast::ExprData::IndexOp { obj, args } => {
                let bracket_span = args.span;
                let obj = ast.get_node(*obj);
                let args = ast.get_node_list(args);
                match obj.data() {
                    ast::ExprData::Identifier(s) if s.as_str() == "__compiled_arg__" => {
                        Box::new(CompiledArg(args))
                    }
                    _ => Box::new(MethodCall {
                        method: Method::Index { bracket_span },
                        obj,
                        args: args.into_iter().map(|arg| (None, arg)).collect(),
                    }),
                }
            }

            ast::ExprData::VectorConstruct(components) => Box::new(FuncCall {
                f: Some(functions::vectors::VectorLiteral.boxed()),
                name: Spanned {
                    node: Arc::new("vector literal".to_owned()),
                    span: expr.span(),
                },
                args: ast
                    .get_node_list(components)
                    .into_iter()
                    .map(|expr| (None, expr))
                    .collect(),
            }),
            ast::ExprData::SetConstruct(members) => Box::new(FuncCall {
                f: Some(functions::sets::SetLiteral.boxed()),
                name: Spanned {
                    node: Arc::new("set literal".to_owned()),
                    span: expr.span(),
                },
                args: ast
                    .get_node_list(members)
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
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        Ok(runtime.eval_expr(self.0)?.node)
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Result<Val> {
        Ok(compiler.build_expr(self.0).node)
    }
}

#[derive(Debug)]
struct FuncCall<'ast> {
    /// Function to call.
    f: Option<Box<dyn Function>>,
    /// Function name.
    name: Spanned<Arc<String>>,
    /// Arguments to the function, each with an optional keyword.
    args: Vec<ast::FuncArg<ast::Expr<'ast>>>,
}
impl Expression for FuncCall<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        let name = Arc::clone(&self.name);
        let span = self.name.span;
        let args = eval_args_list(runtime, &self.args)?;

        match &self.f {
            None => Err(Error::no_such_function(span)),
            Some(f) => {
                let call = CallInfo::new(self.name.clone(), args);
                call.check_kwargs(f)?;
                f.eval(runtime.ctx(), call)
            }
        }
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Result<Val> {
        let name = Arc::clone(&self.name);
        let span = self.name.span;
        let args = compile_args_list(compiler, &self.args)?;

        match &self.f {
            None => Err(Error::no_such_function(span)),
            Some(f) => {
                if let Some(args) = all_rt_vals(&args) {
                    // All arguments are compile-time constants, so compile-time
                    // evaluate the function call.
                    let call = CallInfo::new(self.name.clone(), args);
                    call.check_kwargs(f)?;
                    f.eval(compiler.ctx(), call).map(Val::Rt)
                } else {
                    let call = CallInfo::new(self.name.clone(), args);
                    call.check_kwargs(f)?;
                    f.compile(compiler, call)
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
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        Ok(RtVal::Integer(
            (runtime.eval_bool_expr(self.args[0])? || runtime.eval_bool_expr(self.args[1])?)
                as LangInt,
        ))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Result<Val> {
        let lhs = compiler.build_expr(self.args[0]);

        match lhs.node {
            Val::Rt(_) => {
                let l = compiler.get_rt_val(&lhs).unwrap();
                let l_bool = l.to_bool()?;
                match l_bool {
                    // LHS is statically `true`; do not compile RHS.
                    true => Ok(Val::Rt(RtVal::Integer(1))),
                    // LHS is statically `false`; compile RHS, convert to bool,
                    // and return it.
                    false => {
                        let rhs = compiler.build_expr(self.args[1]);
                        Ok(Val::Cp(CpVal::Integer(
                            compiler.build_convert_to_bool(&rhs)?,
                        )))
                    }
                }
            }
            Val::Cp(_) => {
                let l_bool = compiler.build_convert_to_bool(&lhs)?;
                let bool_result = compiler.build_conditional(
                    l_bool,
                    // LHS is dynamically `true`; do not evaluate RHS.
                    |_| Ok(llvm::const_int(1)),
                    // RHS is dynamically `true`; compile RHS, convert to bool,
                    // and return it.
                    |c| {
                        let rhs = c.build_expr(self.args[1]);
                        c.build_convert_to_bool(&rhs)
                    },
                )?;
                Ok(Val::Cp(CpVal::Integer(bool_result)))
            }
            Val::Unknown(_) => Err(Error::ambiguous_variable_type(lhs.span)),
            Val::MaybeUninit => Err(Error::maybe_uninitialized_variable(lhs.span)),
            Val::Error => Err(Error::AlreadyReported),
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
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        Ok(RtVal::Integer(
            (runtime.eval_bool_expr(self.args[0])? && runtime.eval_bool_expr(self.args[1])?)
                as LangInt,
        ))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Result<Val> {
        let lhs = compiler.build_expr(self.args[0]);

        match lhs.node {
            Val::Rt(_) => {
                let l = compiler.get_rt_val(&lhs).unwrap();
                let l_bool = l.to_bool()?;
                match l_bool {
                    // LHS is statically `true`; compile RHS, convert to bool,
                    // and return it.
                    true => {
                        let rhs = compiler.build_expr(self.args[1]);
                        Ok(Val::Cp(CpVal::Integer(
                            compiler.build_convert_to_bool(&rhs)?,
                        )))
                    }
                    // LHS is statically `false`; do not compile RHS.
                    false => Ok(Val::Rt(RtVal::Integer(0))),
                }
            }
            Val::Cp(_) => {
                let l_bool = compiler.build_convert_to_bool(&lhs)?;
                let bool_result = compiler.build_conditional(
                    l_bool,
                    // LHS is dynamically `true`; compile RHS, convert to bool,
                    // and return it.
                    |c| {
                        let rhs = c.build_expr(self.args[1]);
                        c.build_convert_to_bool(&rhs)
                    },
                    // RHS is dynamically `false`; do not evaluate RHS.
                    |_| Ok(llvm::const_int(0)),
                )?;
                Ok(Val::Cp(CpVal::Integer(bool_result)))
            }
            Val::Unknown(_) => Err(Error::ambiguous_variable_type(lhs.span)),
            Val::MaybeUninit => Err(Error::maybe_uninitialized_variable(lhs.span)),
            Val::Error => Err(Error::AlreadyReported),
        }
    }
}

#[derive(Debug, Clone)]
struct CmpChain<'ast> {
    args: Vec<ast::Expr<'ast>>,
    ops: &'ast [Spanned<ast::CompareOp>],
}
impl Expression for CmpChain<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        if self.args.len() != self.ops.len() + 1 {
            return Err(internal_error_value!("CmpChain ops/args length mismatch"));
        }
        let mut lhs = runtime.eval_expr(self.args[0])?;
        let other_args = &self.args[1..];
        for (&op, &rhs_id) in self.ops.iter().zip(other_args) {
            let rhs = runtime.eval_expr(rhs_id)?;

            let cmp_result = functions::cmp::eval(runtime.ctx(), op.node, &lhs, &rhs)?;

            if !cmp_result {
                // This comparison evaluated to `false`, so the whole expression
                // evaluates to `false`.
                return Ok(RtVal::Integer(0));
            }

            lhs = rhs;
        }
        // All comparisons evaluated to `true`, so the whole expression
        // evaluates to `true`.
        return Ok(RtVal::Integer(1));
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Result<Val> {
        if self.args.len() != self.ops.len() + 1 {
            internal_error!("CmpChain ops/args length mismatch");
        }

        let mut false_bb = None;
        let mut build_cmp_return_false = |c: &mut Compiler| {
            let f_bb = *false_bb.get_or_insert_with(|| c.append_basic_block("cmp_false"));
            c.builder().build_unconditional_branch(f_bb);
            Ok(())
        };

        let mut lhs = compiler.build_expr(self.args[0]);
        let other_args = &self.args[1..];
        for (&op, &rhs_id) in self.ops.iter().zip(other_args) {
            let rhs = compiler.build_expr(rhs_id);

            if lhs.is_rt_val() && rhs.is_rt_val() {
                // Both values are known at compile time, so compute the result
                // statically right here.
                let l = lhs.clone().map_node(|v| v.rt_val().unwrap());
                let r = rhs.clone().map_node(|v| v.rt_val().unwrap());
                let cmp_result = functions::cmp::eval(compiler.ctx(), op.node, &l, &r)?;

                match cmp_result {
                    true => (), // Proceed to the next comparison.
                    false => {
                        // Return false immediately.
                        build_cmp_return_false(compiler)?;
                        // Do not compile any other comparisons.
                        break;
                    }
                }
            } else {
                // Values are not known at compile time, so compute the result
                // at runtime.
                let l = compiler.get_cp_val(&lhs)?;
                let r = compiler.get_cp_val(&rhs)?;
                let cmp_result = functions::cmp::compile(compiler, op.node, &l, &r)?;

                compiler.build_conditional(
                    cmp_result,
                    |_| Ok(()),                  // Proceed to the next comparison.
                    &mut build_cmp_return_false, // Return false immediately.
                )?;
            }

            lhs = rhs;
        }

        if let Some(false_bb) = false_bb {
            if compiler.needs_terminator() {
                let (end_bb, phi) =
                    compiler.append_basic_block_with_phi("cmp_end", llvm::int_type(), "cmp_result");

                // If we execute the current basic block, then all comparisons
                // returned true.
                let true_bb = compiler.current_block();
                let b = compiler.builder();
                b.build_unconditional_branch(end_bb);

                // If we execute `false_bb`, then some comparison returned false.
                b.position_at_end(false_bb);
                b.build_unconditional_branch(end_bb);

                b.position_at_end(end_bb);
                phi.add_incoming(&[
                    (&llvm::const_int(1), true_bb),
                    (&llvm::const_int(0), false_bb),
                ]);

                Ok(Val::Cp(CpVal::Integer(
                    phi.as_basic_value().into_int_value(),
                )))
            } else {
                // The current basic block unconditionally jumps to `false_bb`,
                // so the last comparison definitely returned false.
                compiler.builder().position_at_end(false_bb);
                Ok(Val::Rt(RtVal::Integer(0)))
            }
        } else {
            // All comparisons returned true, so just return true.
            Ok(Val::Rt(RtVal::Integer(1)))
        }
    }
}

#[derive(Debug)]
struct MethodCall<'ast> {
    /// Closure that resolves a method based on the type of the receiver.
    method: Method,
    /// Method receiver.
    obj: ast::Expr<'ast>,
    /// Arguments to the method, each with an optional keyword.
    args: Vec<ast::FuncArg<ast::Expr<'ast>>>,
}
impl MethodCall<'_> {
    fn eval_args_and_get_method(
        &self,
        runtime: &mut Runtime,
    ) -> Result<(
        Box<dyn Function>,
        Spanned<Arc<String>>,
        Vec<ast::FuncArg<Spanned<RtVal>>>,
    )> {
        let obj = runtime.eval_expr(self.obj)?;
        let mut args = eval_args_list(runtime, &self.args)?;

        // Resolve the method based on the type of the method receiver.
        let obj_ty = obj.ty();
        let name = self.method.display_name(&obj_ty);
        let f = self.method.resolve(&obj_ty, obj.span)?;

        // Add the method receiver as the first argument.
        args.insert(0, (None, obj));

        Ok((f, name, args))
    }
    fn compile_args_and_get_method(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(
        Box<dyn Function>,
        Spanned<Arc<String>>,
        Vec<ast::FuncArg<Spanned<Val>>>,
    )> {
        let obj = compiler.build_expr(self.obj);
        let mut args = compile_args_list(compiler, &self.args)?;

        // Resolve the method based on the type of the method receiver.
        let obj_ty = compiler.get_val_type(&obj)?;
        let name = self.method.display_name(&obj_ty);
        let f = self.method.resolve(&obj_ty, obj.span)?;

        // Add the method receiver as the first argument.
        args.insert(0, (None, obj));

        Ok((f, name, args))
    }
}
impl Expression for MethodCall<'_> {
    fn eval(&self, runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        let (f, name, args) = self.eval_args_and_get_method(runtime)?;
        f.eval(runtime.ctx(), CallInfo::new(name, args))
    }
    fn compile(&self, compiler: &mut Compiler, _span: Span) -> Result<Val> {
        let (f, name, args) = self.compile_args_and_get_method(compiler)?;
        if let Some(args) = all_rt_vals(&args) {
            // All arguments are compile-time constants, so compile-time
            // evaluate the method call.
            let call = CallInfo::new(name, args);
            call.check_kwargs(&f)?;
            f.eval(compiler.ctx(), call).map(Val::Rt)
        } else {
            let call = CallInfo::new(name, args);
            call.check_kwargs(&f)?;
            f.compile(compiler, call)
        }
    }

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        op: Option<Spanned<ast::AssignOp>>,
        new_value: Spanned<RtVal>,
    ) -> Result<()> {
        let (f, name, args) = self.eval_args_and_get_method(runtime)?;
        let call_info = CallInfo::new(name, args);

        let new_value = eval_assign_op(
            runtime,
            |rt| f.eval(rt.ctx(), call_info.clone()),
            span,
            op,
            new_value,
        )?;

        f.eval_assign(runtime, call_info, self.obj, new_value)
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        op: Option<Spanned<ast::AssignOp>>,
        new_value: Spanned<Val>,
    ) -> Result<()> {
        let (f, name, args) = self.compile_args_and_get_method(compiler)?;
        let call = CallInfo::new(name.clone(), args.clone());
        call.check_kwargs(&f)?;

        let new_value = compile_assign_op(
            compiler,
            |c| {
                if let Some(args) = all_rt_vals(&args) {
                    // All arguments are compile-time constants, so compile-time
                    // evaluate the method call.
                    let call = CallInfo::new(name, args);
                    f.eval(c.ctx(), call).map(Val::Rt)
                } else {
                    f.compile(c, call.clone())
                }
            },
            span,
            op,
            new_value,
        )?;

        f.compile_assign(compiler, call, self.obj, new_value)
    }
}

#[derive(Debug, Clone)]
pub enum Method {
    MethodCall { name: Spanned<Arc<String>> },
    Index { bracket_span: Span },
}
impl Method {
    pub fn resolve(&self, obj_type: &Type, obj_span: Span) -> Result<Box<dyn Function>> {
        match self {
            Method::MethodCall { name } => super::resolve_method(obj_type, &name)
                .ok_or(Error::no_such_method(name.span, &obj_type)),
            Method::Index { .. } => super::resolve_index_method(obj_type)
                .ok_or_else(|| Error::cannot_index_type(obj_span, obj_type)),
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Method::MethodCall { name } => name.span,
            Method::Index { bracket_span } => *bracket_span,
        }
    }
    pub fn display_name(&self, obj_type: &Type) -> Spanned<Arc<String>> {
        let s = match self {
            Method::MethodCall { name } => {
                format!("{}.{}", obj_type, name.node)
            }
            Method::Index { .. } => match obj_type {
                Type::Type => "type specification".to_string(), // TODO: better name for this
                _ => format!("{} indexing", obj_type),
            },
        };
        Spanned {
            node: Arc::new(s),
            span: self.span(),
        }
    }
}

#[derive(Debug, Clone)]
struct CompiledArg<'ast>(Vec<ast::Expr<'ast>>);
impl<'ast> CompiledArg<'ast> {
    fn arg_index(&self, compiler: &mut Compiler, span: Span) -> Result<Spanned<u32>> {
        // There should be exactly one expression.
        let index_expr = *self
            .0
            .iter()
            .exactly_one()
            .map_err(|_| Error::custom(span, "expected exactly one index"))?;
        let index_value = compiler.build_expr(index_expr);
        let span = index_expr.span();
        // It should be const-evaluatable
        let index = compiler
            .get_rt_val(&index_value)?
            // ... should be an integer
            .as_integer()?
            // ... should fit in `u32`
            .try_into()
            .map_err(|_| Error::custom(span, "compiled arg index out of range"))?;
        // ... and it should be within range.
        if index < compiler.param_types().len() as u32 {
            Ok(Spanned {
                node: index,
                span: index_expr.span(),
            })
        } else {
            Err(Error::custom(span, "compiled arg index out of range"))
        }
    }
}
impl Expression for CompiledArg<'_> {
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Result<RtVal> {
        Err(Error::cannot_const_eval(span))
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Result<Val> {
        let index = self.arg_index(compiler, span)?;
        compiler.build_load_arg(index).map(Val::Cp)
    }

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        _op: Option<Spanned<ast::AssignOp>>,
        _new_value: Spanned<RtVal>,
    ) -> Result<()> {
        Err(Error::cannot_const_eval(span))
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        op: Option<Spanned<ast::AssignOp>>,
        new_value: Spanned<Val>,
    ) -> Result<()> {
        let index = self.arg_index(compiler, span)?;
        let old_value_fn = |c: &mut Compiler| c.build_load_arg(index).map(Val::Cp);
        let new_arg_value = compile_assign_op(compiler, old_value_fn, span, op, new_value)?;
        compiler.build_store_arg(index, &new_arg_value)
    }
}

#[derive(Debug, Clone)]
struct Identifier<'ast>(&'ast Arc<String>);
impl Expression for Identifier<'_> {
    fn eval(&self, runtime: &mut Runtime, span: Span) -> Result<RtVal> {
        if let Some(variable) = runtime.vars.get(self.0) {
            Ok(variable.clone())
        } else if let Some(constant) = super::resolve_constant(self.0, span, runtime.ctx()) {
            constant
        } else {
            Err(Error::uninitialized_variable(span))
        }
    }
    fn compile(&self, compiler: &mut Compiler, span: Span) -> Result<Val> {
        if let Some(variable) = compiler.vars.get(self.0) {
            Ok(variable.clone())
        } else if let Some(constant) = super::resolve_constant(self.0, span, compiler.ctx()) {
            constant.map(Val::Rt)
        } else {
            Err(Error::uninitialized_variable(span))
        }
    }

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        span: Span,
        op: Option<Spanned<ast::AssignOp>>,
        new_value: Spanned<RtVal>,
    ) -> Result<()> {
        let new_value =
            eval_assign_op(runtime, |rt| self.eval(rt, span), span, op, new_value)?.node;
        runtime.vars.insert(Arc::clone(self.0), new_value);
        Ok(())
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        span: Span,
        op: Option<Spanned<ast::AssignOp>>,
        new_value: Spanned<Val>,
    ) -> Result<()> {
        let new_value = compile_assign_op(compiler, |c| self.compile(c, span), span, op, new_value)
            .map(|v| v.node)
            .unwrap_or_else(|e| {
                compiler.report_error(e);
                Val::Error
            });
        compiler.vars.insert(Arc::clone(self.0), new_value);
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Constant<'a>(&'a RtVal);
impl Expression for Constant<'_> {
    fn eval(&self, _runtime: &mut Runtime, _span: Span) -> Result<RtVal> {
        Ok(self.0.clone())
    }
    fn compile(&self, _compiler: &mut Compiler, _span: Span) -> Result<Val> {
        Ok(Val::Rt(self.0.clone()))
    }
}

fn eval_assign_op(
    runtime: &mut Runtime,
    old_value_fn: impl FnOnce(&mut Runtime) -> Result<RtVal>,
    old_value_span: Span,
    op: Option<Spanned<ast::AssignOp>>,
    new_value: Spanned<RtVal>,
) -> Result<Spanned<RtVal>> {
    match op {
        None => Ok(new_value),
        Some(op) => {
            let old_value = Spanned {
                node: old_value_fn(runtime)?,
                span: old_value_span,
            };
            let op_func = functions::math::BinaryMathOp::from(op.node);
            let node = op_func.eval_on_values(runtime.ctx(), op.span, &old_value, &new_value)?;
            let span = old_value.span.merge(new_value.span);
            Ok(Spanned { node, span })
        }
    }
}
fn compile_assign_op(
    compiler: &mut Compiler,
    old_value_fn: impl FnOnce(&mut Compiler) -> Result<Val>,
    old_value_span: Span,
    op: Option<Spanned<ast::AssignOp>>,
    new_value: Spanned<Val>,
) -> Result<Spanned<Val>> {
    match op {
        None => Ok(new_value),
        Some(op) => {
            let old_value = Spanned {
                node: old_value_fn(compiler)?,
                span: old_value_span,
            };
            let op_func = functions::math::BinaryMathOp::from(op.node);
            let node = op_func.compile_for_values(compiler, op.span, &old_value, &new_value)?;
            let span = old_value.span.merge(new_value.span);
            Ok(Spanned { node, span })
        }
    }
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
) -> Result<Vec<ast::FuncArg<Spanned<RtVal>>>> {
    args.iter()
        .cloned()
        .map(|(k, v)| Ok((k, runtime.eval_expr(v)?)))
        .collect()
}
fn compile_args_list(
    compiler: &mut Compiler,
    args: &[ast::FuncArg<ast::Expr<'_>>],
) -> Result<Vec<ast::FuncArg<Spanned<Val>>>> {
    args.iter()
        .cloned()
        .map(|(k, v)| Ok((k, compiler.build_expr(v))))
        .collect()
}
