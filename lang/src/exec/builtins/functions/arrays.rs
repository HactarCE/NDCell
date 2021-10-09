//! Functions and methods that construct or operate on arrays.

use codemap::Spanned;
use std::sync::Arc;
use std::{fmt, panic};

use super::{CallInfo, Function};
use crate::data::{CellArray, LangCell, RtVal, SpannedRuntimeValueExt, Type, Val, VectorSet};
use crate::errors::{Error, Fallible, Result};
use crate::exec::{Compiler, Ctx, CtxTrait, ErrorReportExt};
use crate::parser;

/// Built-in function that constructs an `CellArray`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct New(pub Arc<VectorSet>);
impl fmt::Display for New {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.new", Type::CellArray(Some(Arc::clone(&self.0))))
    }
}
impl Function for New {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        match call.args.len() {
            0 => Ok(CellArray::from_cell(Arc::clone(&self.0), 0_u8).into()),

            1 => {
                let cell_state = call.arg(1, ctx)?.select_cell().report_err(ctx)?;
                Ok(CellArray::from_cell(Arc::clone(&self.0), cell_state).into())
            }

            _ => Err(call.invalid_args_error(ctx, self)),
        }
    }
}

/// Built-in method that constructs a `CellArray`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct FillVectorSet;
impl fmt::Display for FillVectorSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.fill", Type::VectorSet(None))
    }
}
impl Function for FillVectorSet {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Fallible<RtVal> {
        let arg_count = call.args.len();

        if arg_count < 1 {
            return Err(call.invalid_args_error(ctx, self));
        }

        let ndim = ctx.get_ndim(call.span)?;
        let shape = call.args[0].as_vector_set(ndim).report_err(ctx)?;

        let cell_array = if arg_count - 1 == shape.len() {
            let contents: Vec<LangCell> = call.args[1..]
                .iter()
                .map(|arg| arg.select_cell())
                .collect::<Result<_>>()
                .report_err(ctx)?;
            CellArray::from_cells(call.span, shape, &contents).report_err(ctx)?
        } else if arg_count - 1 == 1 {
            let contents = &call.args[1];
            if let Ok(contents_str) = contents.as_string() {
                let contents =
                    parser::strings::parse_cell_array_string(ctx, contents.span, &contents_str)?;
                CellArray::from_cells(call.args[1].span, shape, &contents).report_err(ctx)?
            } else if let Ok(cell) = contents.select_cell() {
                CellArray::from_cell(shape, cell)
            } else {
                return Err(ctx.error(Error::type_error(
                    contents.span,
                    "TODO cell array contents",
                    &contents.ty(),
                )));
            }
        } else {
            return Err(call.invalid_args_error(ctx, self));
        };

        Ok(RtVal::CellArray(cell_array))
    }
    fn compile(&self, compiler: &mut Compiler, call: CallInfo<Spanned<Val>>) -> Fallible<Val> {
        Err(compiler.error(Error::cannot_compile(call.span)))
    }
}

/// Built-in method that returns the shape of a `CellArray`.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Shape;
impl fmt::Display for Shape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.shape", Type::VectorSet(None))
    }
}
