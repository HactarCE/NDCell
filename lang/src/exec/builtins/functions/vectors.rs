//! Functions and methods that construct or operate on vectors.

use codemap::{Span, Spanned};
use std::fmt;

use ndcell_core::axis::Axis;

use super::{CallInfo, Function};
use crate::ast;
use crate::data::{
    self, CpVal, LangInt, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt, Type, Val,
};
use crate::errors::{Error, Fallible};
use crate::exec::builtins::Expression;
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt};
use crate::llvm;

/// Built-in function that constructs a vector from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VectorLiteral;
impl fmt::Display for VectorLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vector expression")
    }
}
impl Function for VectorLiteral {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let mut ret = vec![];
        for arg in call.args {
            match arg.node {
                RtVal::Integer(i) => Ok(ret.push(i)),
                RtVal::Vector(v) => Ok(ret.extend_from_slice(&v)),
                RtVal::IntegerSet(_) => Err(Error::unimplemented(arg.span)),
                RtVal::VectorSet(_) => Err(Error::unimplemented(arg.span)),
                _ => Err(Error::expected(arg.span, "integer or vector")),
            }
            .report_err(ctx)?;
        }
        data::check_vector_len(call.span, ret.len()).report_err(ctx)?;
        Ok(RtVal::Vector(ret))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let mut ret = vec![];
        for arg in &call.args {
            let arg = compiler.get_cp_val(arg)?;
            match arg.node {
                CpVal::Integer(i) => Ok(ret.push(i)),
                CpVal::Vector(v) => Ok(ret.extend_from_slice(&compiler.build_split_vector(v))),
                _ => Err(Error::expected(arg.span, "integer or vector")),
            }
            .report_err(compiler)?;
        }
        data::check_vector_len(call.span, ret.len()).report_err(compiler)?;
        Ok(Val::Cp(CpVal::Vector(llvm::VectorType::const_vector(&ret))))
    }
}

/// Built-in function that constructs a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecConstructor;
impl fmt::Display for VecConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec")
    }
}
impl Function for VecConstructor {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["len"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len = match call.kwarg("len") {
            Some(x) => x
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))
                .report_err(ctx)?,
            None => ctx.get_ndim(call.span)?,
        };
        eval_vec_construct(ctx, call, len)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let len = match call.kwarg("len") {
            Some(x) => compiler
                .get_rt_val(x)?
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))
                .report_err(compiler)?,
            None => compiler.get_ndim(call.span)?,
        };
        compile_vec_construct(compiler, call, len)
    }
}

/// Built-in function that constructs a vector with a specific length.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VecWithLen(pub usize);
impl fmt::Display for VecWithLen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vec{}", self.0)
    }
}
impl Function for VecWithLen {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let len = self.0;
        eval_vec_construct(ctx, call, len)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let len = self.0;
        compile_vec_construct(compiler, call, len)
    }
}

fn eval_vec_construct(
    ctx: &mut Ctx,
    call: CallInfo<Spanned<RtVal>>,
    len: usize,
) -> Fallible<RtVal> {
    match &call.args[..] {
        [] => Ok(RtVal::Vector(vec![0; len])),
        [arg] => Ok(RtVal::Vector(arg.to_vector(len).report_err(ctx)?)),
        _ => Err(call.invalid_args_error(ctx)),
    }
}
fn compile_vec_construct(
    compiler: &mut Compiler,
    call: CallInfo<Spanned<Val>>,
    len: usize,
) -> Fallible<Val> {
    match &call.args[..] {
        [] => Ok(Val::Rt(RtVal::Vector(vec![0; len]))),
        [arg] => Ok(Val::Cp(CpVal::Vector(
            compiler.build_convert_to_vector(arg, len)?,
        ))),
        _ => Err(call.invalid_args_error(compiler)),
    }
}

/// Built-in function that indexes a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct IndexVector(pub Option<Axis>);
impl fmt::Display for IndexVector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(Axis::X) => write!(f, "{}.x", Type::Vector(None)),
            Some(Axis::Y) => write!(f, "{}.y", Type::Vector(None)),
            Some(Axis::Z) => write!(f, "{}.z", Type::Vector(None)),
            Some(Axis::W) => write!(f, "{}.w", Type::Vector(None)),
            Some(Axis::U) => write!(f, "{}.u", Type::Vector(None)),
            Some(Axis::V) => write!(f, "{}.v", Type::Vector(None)),
            None => write!(f, "vector indexing"),
        }
    }
}
impl IndexVector {
    fn eval_args(
        &self,
        ctx: &mut Ctx,
        call: &CallInfo<Spanned<RtVal>>,
    ) -> Fallible<(Vec<LangInt>, usize)> {
        let i;
        let span;
        match self.0 {
            Some(axis) => {
                call.check_args_len(1, ctx)?;
                i = axis as LangInt;
                span = call.span;
            }
            None => {
                call.check_args_len(2, ctx)?;
                let arg = call.arg(1, ctx)?;
                i = arg.as_integer().report_err(ctx)?;
                span = arg.span;
            }
        };

        let v = call.arg(0, ctx)?.as_vector().report_err(ctx)?;

        // Error if the index is out of bounds.
        let vec_len = v.len() as LangInt;
        if !(0 <= i && i < vec_len) {
            return Err(ctx.error(Error::index_out_of_bounds(span)));
        }

        Ok((v, i as usize))
    }
    fn compile_args(
        &self,
        compiler: &mut Compiler,
        call: &CallInfo<Spanned<Val>>,
    ) -> Fallible<(llvm::VectorValue, llvm::IntValue)> {
        let i;
        let span;
        match self.0 {
            Some(axis) => {
                call.check_args_len(1, compiler)?;
                i = llvm::const_int(axis as LangInt);
                span = call.span;
            }
            None => {
                call.check_args_len(2, compiler)?;
                let arg = call.arg(1, compiler)?;
                i = compiler
                    .get_cp_val(arg)?
                    .as_integer()
                    .report_err(compiler)?;

                span = arg.span;
            }
        };

        let arg = call.arg(0, compiler)?;
        let v = compiler.get_cp_val(arg)?.as_vector().report_err(compiler)?;

        // Error if the index is out of bounds.
        let vec_len = v.get_type().get_size() as LangInt;
        if let Some(i) = i.get_zero_extended_constant() {
            // We know the index at compile-time, so report the error at
            // compile-time or don't bother checking at all.
            if !(i < vec_len as u64) {
                return Err(compiler.error(Error::index_out_of_bounds(span)));
            }
        } else {
            // Check at runtime.
            let index_out_of_bounds = compiler.builder().build_int_compare(
                llvm::IntPredicate::UGE,
                i,
                llvm::const_int(vec_len),
                "index_out_of_bounds",
            );
            let error_index = compiler.add_runtime_error(Error::index_out_of_bounds(span));
            compiler.build_return_err_if(index_out_of_bounds, error_index)?;
        }

        Ok((v, i))
    }
}
impl Function for IndexVector {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let (v, i) = self.eval_args(ctx, &call)?;
        Ok(RtVal::Integer(v[i]))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        let (v, i) = self.compile_args(compiler, &call)?;
        let ret = compiler
            .builder()
            .build_extract_element(v, i, "vector_component")
            .into_int_value();
        Ok(Val::Cp(CpVal::Integer(ret)))
    }

    fn eval_assign(
        &self,
        runtime: &mut crate::exec::Runtime,
        call: CallInfo<Spanned<RtVal>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<RtVal>,
    ) -> Fallible<()> {
        let (mut v, i) = self.eval_args(runtime.ctx(), &call)?;

        // Assign the new component.
        v[i as usize] = new_value.as_integer().report_err(runtime)?;
        let new_value = Spanned {
            node: RtVal::Vector(v),
            span: call.span,
        };

        // Assign the new vector value.
        let op = None;
        Box::<dyn Expression>::from(first_arg).eval_assign(runtime, first_arg.span(), op, new_value)
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        call: CallInfo<Spanned<Val>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<Val>,
    ) -> Fallible<()> {
        let (v, i) = self.compile_args(compiler, &call)?;

        // Assign the new component.
        let new_component = compiler
            .get_cp_val(&new_value)?
            .as_integer()
            .report_err(compiler)?;
        let v =
            compiler
                .builder()
                .build_insert_element(v, new_component, i, "assign_vector_component");
        let new_value = Spanned {
            node: Val::Cp(CpVal::Vector(v)),
            span: call.span,
        };

        // Assign the new vector value.
        let op = None;
        Box::<dyn Expression>::from(first_arg).compile_assign(
            compiler,
            first_arg.span(),
            op,
            new_value,
        )
    }
}

/// Built-in function that returns the length of a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VectorLen;
impl_display!(for VectorLen, "Vector.len");
impl Function for VectorLen {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        call.check_args_len(1, ctx)?;
        let arg = call.arg(0, ctx)?;
        let len = arg.as_vector().report_err(ctx)?.len() as LangInt;
        Ok(RtVal::Integer(len))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        call.check_args_len(1, compiler)?;
        let arg = call.arg(0, compiler)?;
        let len = compiler
            .get_cp_val(arg)?
            .as_vector()
            .report_err(compiler)?
            .get_type()
            .get_size() as LangInt;
        Ok(Val::Rt(RtVal::Integer(len)))
    }
}
