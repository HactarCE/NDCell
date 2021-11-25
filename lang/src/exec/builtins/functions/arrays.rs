//! Functions and methods that construct or operate on arrays.

use codemap::Spanned;
use std::sync::Arc;

use super::{CallInfo, Function};
use crate::ast;
use crate::data::{
    CellArray, CpVal, GetType, LangCell, RtVal, SpannedCompileValueExt, SpannedRuntimeValueExt,
    SpannedValExt, Type, Val, VectorSet,
};
use crate::errors::{Error, Result};
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

        let ndim = ctx.get_ndim(call.expr_span)?;
        let shape = call.arg(0)?.as_vector_set(ndim)?;

        let cell_array = if arg_count - 1 == shape.len() {
            let contents: Vec<LangCell> = call.args[1..]
                .iter()
                .map(|arg| arg.select_cell())
                .collect::<Result<_>>()?;
            CellArray::from_cells(call.expr_span, shape, &contents)?
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

        Ok(RtVal::CellArray(Arc::new(cell_array.into())))
    }
    fn compile(&self, _compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        Err(Error::unimplemented(call.span))
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

/// Built-in method that returns an immutable reference to a `CellArray` or
/// `CellArrayMut`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct AsImmut;
impl Function for AsImmut {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        call.check_args_len(1)?;
        call.arg(0)?.as_cell_array().map(RtVal::CellArray)
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(1)?;
        Ok(Val::Cp(CpVal::CellArray(
            compiler.get_cp_val(call.arg(0)?)?.as_cell_array()?,
        )))
    }
}

// /// Built-in method that returns a mutable reference to a `CellArray` or
// /// `CellArrayMut`.
// #[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
// pub struct AsMut;
// impl Function for AsMut {
//     fn can_const_eval(&self) -> bool {
//         false
//     }

//     fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
//         internal_error!("cannot construct mutable cell array in interpreted code");
//     }
//     fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
//         call.check_args_len(1)?;
//         call.check_args_len(1)?;
//         Ok(Val::Cp(CpVal::CellArrayMut(
//             compiler.get_cp_val(call.arg(0)?)?.as_cell_array()?,
//         )))
//     }
// }

/// Built-in method that indexes a `CellArray`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct IndexCellArray;
impl IndexCellArray {
    fn compile_pos_arg(
        &self,
        compiler: &mut Compiler,
        shape: &VectorSet,
        call: &CallInfo<Spanned<Val>>,
    ) -> Result<llvm::VectorValue> {
        if let Ok(constant_pos) = call.args[1..]
            .iter()
            .map(|v| v.try_rt_val())
            .collect::<Result<Vec<Spanned<RtVal>>>>()
        {
            let p = super::vectors::eval_vector_literal(call.span, &constant_pos)?;

            // Error at compile-time if the position is out of bounds.
            if !shape.contains_vector(&p) {
                compiler.report_error(Error::position_out_of_bounds(call.span).to_warning());
            }

            Ok(llvm::const_vector(p))
        } else {
            super::vectors::compile_vector_literal(compiler, call.span, &call.args[1..])
        }
    }
}
impl Function for IndexCellArray {
    fn eval(&self, _ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        let array = call.arg(0)?.as_cell_array()?;
        let pos = super::vectors::eval_vector_literal(call.span, &call.args[1..])?;
        let cell = array
            .get_cell(&pos)
            .ok_or_else(|| Error::position_out_of_bounds(call.span))?;
        Ok(RtVal::Cell(cell))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        let array = compiler.get_cp_val(call.arg(0)?)?.as_cell_array()?;
        let pos = self.compile_pos_arg(compiler, array.shape(), &call)?;
        let cell_ptr = compiler.build_cell_array_gep(call.span, &array, pos)?;
        let cell = compiler
            .builder()
            .build_load(cell_ptr, "cell_from_array")
            .into_int_value();
        Ok(Val::Cp(CpVal::Cell(cell)))
    }

    fn eval_assign(
        &self,
        _runtime: &mut Runtime,
        call: CallInfo<Spanned<RtVal>>,
        _first_arg: ast::Expr<'_>,
        _new_value: Spanned<RtVal>,
    ) -> Result<()> {
        Err(Error::cannot_assign_to(call.expr_span))
    }
    fn compile_assign(
        &self,
        compiler: &mut Compiler,
        call: CallInfo<Spanned<Val>>,
        _first_arg: ast::Expr<'_>,
        new_value: Result<Spanned<Val>>,
    ) -> Result<()> {
        if matches!(call.arg(0)?.ty(), Type::CellArray(_)) {
            return Err(Error::cannot_assign_to(call.expr_span));
        }
        let array = compiler.get_cp_val(call.arg(0)?)?.as_cell_array_mut()?;
        let pos = self.compile_pos_arg(compiler, array.shape(), &call)?;
        let cell_ptr = compiler.build_cell_array_gep(call.span, &array, pos)?;
        let cell = compiler.get_cp_val(&new_value?)?.as_cell()?;
        compiler.builder().build_store(cell_ptr, cell);
        Ok(())
    }
}

/// Built-in method that returns a new mutable `CellArray` buffer.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct NewBuffer;
impl Function for NewBuffer {
    fn eval(&self, _ctx: &mut Ctx, _call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        internal_error!("cannot construct mutable cell array in interpreted code");
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Result<Val> {
        call.check_args_len(1)?;

        let ndim = compiler.get_ndim(call.expr_span)?;
        let shape = call.arg(0)?.try_rt_val()?.as_vector_set(ndim)?;

        Ok(Val::Cp(CpVal::CellArrayMut(
            compiler.build_alloca_cell_array(shape, "new_buffer")?,
        )))
    }
}
