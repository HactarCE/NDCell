//! Functions and methods that construct or operate on arrays.

use codemap::Spanned;
use std::fmt;
use std::sync::Arc;

use super::{CallInfo, Function};
use crate::data::{CellArray, CellSet, RtVal, SpannedRuntimeValueExt, Type, VectorSet};
use crate::errors::{Error, Fallible, Result};
use crate::exec::{Ctx, CtxTrait, ErrorReportExt};

/// Built-in function that constructs an `CellArray`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct New(Arc<VectorSet>);
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
