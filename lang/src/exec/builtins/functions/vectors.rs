//! Functions and methods that construct or operate on vectors.

use codemap::{Span, Spanned};

use ndcell_core::axis::Axis;

use super::{CallInfo, Function};
use crate::ast;
use crate::data::{
    self, CpVal, LangInt, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt, Val,
};
use crate::errors::{Error, Result};
use crate::exec::builtins::Expression;
use crate::exec::{Compiler, Ctx, CtxTrait};
use crate::llvm;

/// Built-in function that constructs a vector from components.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VectorLiteral;
impl Function for VectorLiteral {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let ret = eval_vector_literal(call.span, &call.args)?;
        Ok(RtVal::Vector(ret))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        let ret = compile_vector_literal(compiler, call.span, &call.args)?;
        Ok(Val::Cp(CpVal::Vector(ret)))
    }
}

pub(super) fn eval_vector_literal(span: Span, args: &[Spanned<RtVal>]) -> Result<Vec<LangInt>> {
    let mut ret = vec![];
    for arg in args {
        match &arg.node {
            RtVal::Integer(i) => Ok(ret.push(*i)),
            RtVal::Vector(v) => Ok(ret.extend_from_slice(v)),
            _ => Err(Error::expected(arg.span, "integer or vector")),
        }?;
    }
    data::check_vector_len(span, ret.len())?;
    Ok(ret)
}
pub(super) fn compile_vector_literal(
    compiler: &mut Compiler,
    span: Span,
    args: &[Spanned<Val>],
) -> Result<llvm::VectorValue> {
    let mut components = vec![];
    for arg in args {
        let arg = compiler.get_cp_val(arg)?;
        match &arg.node {
            CpVal::Integer(i) => Ok(components.push(*i)),
            CpVal::Vector(v) => Ok(components.extend_from_slice(&compiler.build_split_vector(*v))),
            _ => Err(Error::expected(arg.span, "integer or vector")),
        }?;
    }
    data::check_vector_len(span, components.len())?;
    Ok(compiler.build_construct_vector(&components))
}

/// Built-in function that constructs a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VecConstructor;
impl Function for VecConstructor {
    fn kwarg_keys(&self) -> &[&'static str] {
        &["len"]
    }

    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let len = match call.kwarg("len") {
            Some(x) => x
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))?,
            None => ctx.get_ndim(call.span)?,
        };
        eval_vec_construct(call, len)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        let len = match call.kwarg("len") {
            Some(x) => compiler
                .get_rt_val(x)?
                .as_integer()
                .and_then(|n| data::check_vector_len(x.span, n))?,
            None => compiler.get_ndim(call.span)?,
        };
        compile_vec_construct(compiler, call, len)
    }
}

/// Built-in function that constructs a vector with a specific length.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VecWithLen(pub usize);
impl Function for VecWithLen {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let len = self.0;
        eval_vec_construct(call, len)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        let len = self.0;
        compile_vec_construct(compiler, call, len)
    }
}

fn eval_vec_construct(call: CallInfo<Spanned<RtVal>>, len: usize) -> Result<RtVal> {
    match &call.args[..] {
        [] => Ok(RtVal::Vector(vec![0; len])),
        [arg] => Ok(RtVal::Vector(arg.to_vector(len)?)),
        _ => Err(call.invalid_args_error()),
    }
}
fn compile_vec_construct(
    compiler: &mut Compiler,
    call: CallInfo<Spanned<Val>>,
    len: usize,
) -> Result<Val> {
    match &call.args[..] {
        [] => Ok(Val::Rt(RtVal::Vector(vec![0; len]))),
        [arg] => Ok(Val::Cp(CpVal::Vector(
            compiler.build_convert_to_vector(arg, len)?,
        ))),
        _ => Err(call.invalid_args_error()),
    }
}

/// Built-in function that indexes a vector.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct IndexVector(pub Option<Axis>);
impl IndexVector {
    fn eval_args(&self, call: &CallInfo<Spanned<RtVal>>) -> Result<(Vec<LangInt>, usize)> {
        let i;
        let span;
        match self.0 {
            Some(axis) => {
                call.check_args_len(1)?;
                i = axis as LangInt;
                span = call.span;
            }
            None => {
                call.check_args_len(2)?;
                let arg = call.arg(1)?;
                i = arg.as_integer()?;
                span = arg.span;
            }
        };

        let v = call.arg(0)?.as_vector()?;

        // Error if the index is out of bounds.
        let vec_len = v.len() as LangInt;
        if !(0 <= i && i < vec_len) {
            return Err(Error::index_out_of_bounds(span));
        }

        Ok((v, i as usize))
    }
    fn compile_args(
        &self,
        compiler: &mut Compiler,
        call: &CallInfo<Spanned<Val>>,
    ) -> Result<(llvm::VectorValue, llvm::IntValue)> {
        let i;
        let span;
        match self.0 {
            Some(axis) => {
                call.check_args_len(1)?;
                i = llvm::const_int(axis as LangInt);
                span = call.span;
            }
            None => {
                call.check_args_len(2)?;
                let arg = call.arg(1)?;
                i = compiler.get_cp_val(arg)?.as_integer()?;

                span = arg.span;
            }
        };

        let arg = call.arg(0)?;
        let v = compiler.get_cp_val(arg)?.as_vector()?;
        let vec_len = v.get_type().get_size() as LangInt;

        // Warn at compile-time if the index is out of bounds.
        if let Some(i) = i.get_zero_extended_constant() {
            if !(i < vec_len as u64) {
                compiler.report_error(Error::index_out_of_bounds(span).to_warning());
            }
        }

        // Error at runtime if the index is out of bounds.
        let index_out_of_bounds = compiler.builder().build_int_compare(
            llvm::IntPredicate::UGE,
            i,
            llvm::const_int(vec_len),
            "index_out_of_bounds",
        );
        let error_index = compiler.add_runtime_error(Error::index_out_of_bounds(span));
        compiler.build_return_err_if(index_out_of_bounds, error_index)?;

        Ok((v, i))
    }
}
impl Function for IndexVector {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let (v, i) = self.eval_args(&call)?;
        Ok(RtVal::Integer(v[i]))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
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
    ) -> Result<()> {
        let (mut v, i) = self.eval_args(&call)?;

        // Assign the new component.
        v[i as usize] = new_value.as_integer()?;
        let new_value = Spanned {
            node: RtVal::Vector(v),
            span: first_arg.span(),
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
    ) -> Result<()> {
        let (v, i) = self.compile_args(compiler, &call)?;

        // Assign the new component.
        let new_component = compiler.get_cp_val(&new_value)?.as_integer()?;
        let v =
            compiler
                .builder()
                .build_insert_element(v, new_component, i, "assign_vector_component");
        let new_value = Spanned {
            node: Val::Cp(CpVal::Vector(v)),
            span: first_arg.span(),
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
impl Function for VectorLen {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(1)?;
        let arg = call.arg(0)?;
        let len = arg.as_vector()?.len() as LangInt;
        Ok(RtVal::Integer(len))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(1)?;
        let arg = call.arg(0)?;
        let len = compiler.get_cp_val(arg)?.as_vector()?.get_type().get_size() as LangInt;
        Ok(Val::Rt(RtVal::Integer(len)))
    }
}
