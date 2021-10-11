//! Functions and methods that construct or operate on arrays.

use codemap::Spanned;
use std::sync::Arc;

use super::{CallInfo, Function};
use crate::ast;
use crate::data::{
    CellArray, CpVal, LangCell, LangInt, LlvmCellArray, RtVal, SpannedCompileValueExt,
    SpannedRuntimeValueExt, Val,
};
use crate::errors::{Error, Result};
use crate::exec::builtins::Expression;
use crate::exec::{Compiler, Ctx, CtxTrait, Runtime};
use crate::llvm;
use crate::parser;

/// Built-in method that constructs a `CellArray`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct FillVectorSet;
impl Function for FillVectorSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let arg_count = call.args.len();

        if arg_count < 1 {
            return Err(call.invalid_args_error());
        }

        let ndim = ctx.get_ndim(call.span)?;
        let shape = call.arg(0)?.as_vector_set(ndim)?;

        let cell_array = if arg_count - 1 == shape.len() {
            let contents: Vec<LangCell> = call.args[1..]
                .iter()
                .map(|arg| arg.select_cell())
                .collect::<Result<_>>()?;
            CellArray::from_cells(call.span, shape, &contents)?
        } else if arg_count - 1 == 1 {
            let contents = call.arg(1)?;
            if let Ok(contents_str) = contents.as_string() {
                let contents =
                    parser::strings::parse_cell_array_string(ctx, contents.span, &contents_str)?;
                CellArray::from_cells(call.arg(1)?.span, shape, &contents)?
            } else if let Ok(cell) = contents.select_cell() {
                CellArray::from_cell(shape, cell)
            } else {
                return Err(Error::type_error(
                    contents.span,
                    "TODO cell array contents",
                    &contents.ty(),
                ));
            }
        } else {
            return Err(call.invalid_args_error());
        };

        Ok(RtVal::CellArray(cell_array))
    }
}

/// Built-in method that returns the shape of a `CellArray`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Shape;
impl Function for Shape {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(1)?;
        let shape = Arc::clone(call.arg(0)?.as_cell_array()?.shape());
        Ok(RtVal::VectorSet(shape))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(1)?;
        let shape = Arc::clone(compiler.get_cp_val(call.arg(0)?)?.as_cell_array()?.shape());
        Ok(Val::Rt(RtVal::VectorSet(shape)))
    }
}

/// Built-in method that returns the shape of a `CellArray`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct IndexCellArray;
impl IndexCellArray {
    fn eval_args(&self, call: &CallInfo<Spanned<RtVal>>) -> Result<(CellArray, Vec<LangInt>)> {
        let array = call.arg(0)?.as_cell_array()?;
        let pos = super::vectors::eval_vector_literal(call.span, &call.args[1..])?;
        Ok((array, pos))
    }
    fn compile_args(
        &self,
        compiler: &mut Compiler,
        call: &CallInfo<Spanned<Val>>,
    ) -> Result<(LlvmCellArray, llvm::VectorValue)> {
        let array = compiler.get_cp_val(call.arg(0)?)?.as_cell_array()?;
        let pos = super::vectors::compile_vector_literal(compiler, call.span, &call.args[1..])?;

        // Warn at compile-time if the position is out of bounds.
        if let Some(p) = llvm::vector_value_as_constant(pos) {
            if !array.shape().contains_vector(&p) {
                compiler.report_error(Error::position_out_of_bounds(call.span).to_warning());
            }
        }

        Ok((array, pos))
    }
}
impl Function for IndexCellArray {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let (array, pos) = self.eval_args(&call)?;
        let cell = array
            .get_cell(&pos)
            .ok_or_else(|| Error::position_out_of_bounds(call.span))?;
        Ok(RtVal::Cell(cell))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        let (array, pos) = self.compile_args(compiler, &call)?;
        // let cell_ptr = compiler.build_cell_array_gep(call.span, &array, pos)?;
        // let cell = compiler
        //     .builder()
        //     .build_load(cell_ptr, "cell_from_array")
        //     .into_int_value();
        let cell = llvm::const_cell(5);
        Ok(Val::Cp(CpVal::Cell(cell)))
    }

    fn eval_assign(
        &self,
        runtime: &mut Runtime,
        call: CallInfo<Spanned<RtVal>>,
        first_arg: ast::Expr<'_>,
        new_value: Spanned<RtVal>,
    ) -> Result<()> {
        let (mut array, pos) = self.eval_args(&call)?;

        // Assign the new cell.
        let cell = new_value.as_cell()?;
        *array
            .get_cell_mut(&pos)
            .ok_or_else(|| Error::position_out_of_bounds(call.span))? = cell;
        let new_value = Spanned {
            node: RtVal::CellArray(array),
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
        _first_arg: ast::Expr<'_>,
        new_value: Spanned<Val>,
    ) -> Result<()> {
        let (array, pos) = self.compile_args(compiler, &call)?;
        let cell_ptr = compiler.build_cell_array_gep(call.span, &array, pos)?;
        let cell = compiler.get_cp_val(&new_value)?.as_cell()?;
        compiler.builder().build_store(cell_ptr, cell);
        Ok(())
    }
}
