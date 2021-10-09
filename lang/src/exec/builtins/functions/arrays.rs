//! Functions and methods that construct or operate on arrays.

use codemap::Spanned;
use std::sync::Arc;
use std::{fmt, panic};

use super::{CallInfo, Function};
use crate::data::{CellArray, LangCell, RtVal, SpannedRuntimeValueExt, Type, Val, VectorSet};
use crate::errors::{Error, Result};
use crate::exec::{Compiler, Ctx, CtxTrait};
use crate::parser;

/// Built-in function that constructs an `CellArray`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct New(pub Arc<VectorSet>);
impl Function for New {
    fn eval(&self, ctx: &mut Ctx, call: CallInfo<Spanned<RtVal>>) -> Result<RtVal> {
        match call.args.len() {
            0 => Ok(CellArray::from_cell(Arc::clone(&self.0), 0_u8).into()),

            1 => {
                let cell_state = call.arg(1)?.select_cell()?;
                Ok(CellArray::from_cell(Arc::clone(&self.0), cell_state).into())
            }

            _ => Err(call.invalid_args_error()),
        }
    }
}

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
        let shape = call.args[0].as_vector_set(ndim)?;

        let cell_array = if arg_count - 1 == shape.len() {
            let contents: Vec<LangCell> = call.args[1..]
                .iter()
                .map(|arg| arg.select_cell())
                .collect::<Result<_>>()?;
            CellArray::from_cells(call.span, shape, &contents)?
        } else if arg_count - 1 == 1 {
            let contents = &call.args[1];
            if let Ok(contents_str) = contents.as_string() {
                let contents =
                    parser::strings::parse_cell_array_string(ctx, contents.span, &contents_str)?;
                CellArray::from_cells(call.args[1].span, shape, &contents)?
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
// TODO: implement this
