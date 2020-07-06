//! Conversion functions.

use inkwell::IntPredicate;

use super::{FuncConstructor, FuncResult};
use crate::ast::{ErrorPointRef, FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{self, Compiler, Value};
use crate::errors::*;
use crate::types::{LangCellState, LangInt};
use crate::{ConstValue, Type};
use LangErrorMsg::CellStateOutOfRange;

// TODO: support int-to-cell-state on int range?

/// Built-in function that returns the cell state with the given ID.
#[derive(Debug)]
pub struct IntToCellState {
    /// Error returned if the given cell state ID is out of range.
    out_of_range_error: ErrorPointRef,
}
impl IntToCellState {
    /// Constructs a new IntToCellState instance.
    pub fn construct(info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self {
            out_of_range_error: info.add_error_point(CellStateOutOfRange),
        }))
    }
}
impl Function for IntToCellState {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        typecheck!(info.arg_types()[0], Int)?;
        Ok(Type::CellState)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();

        // Check that the value is a valid cell state.
        let cell_state_value = args.compile(compiler, 0)?.as_int()?;
        // Treat the signed integer as an unsigned integer, and build a
        // condition testing whether that value is less than the number of cell
        // states. (A negative number will be interpreted as a very large
        // positive number, which will be too large.)
        let cell_state_count_value = cell_state_value
            .get_type()
            .const_int(info.userfunc.rule_meta().states.len() as u64, false);
        let condition = compiler.builder().build_int_compare(
            IntPredicate::ULT, // Unsigned Less-Than
            cell_state_value,
            cell_state_count_value,
            "cellStateRangeCheck",
        );

        // Branch based on the whether the cell state is in range.
        compiler.build_conditional(
            condition,
            // The cell state is in range.
            |_| Ok(()),
            // The cell state is out of range.
            |c| Ok(self.out_of_range_error.compile(c)),
        )?;
        // Now that we know the cell state is in range, perform an integer cast.
        Ok(Value::CellState(compiler.builder().build_int_cast(
            cell_state_value,
            compiler::types::cell_state(),
            "tmp_cellStateFromInt",
        )))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let arg = args.const_eval(0)?.as_int()?;
        let cell_state_count = info.userfunc.rule_meta().states.len() as LangInt;
        if 0 <= arg && arg < cell_state_count {
            Ok(ConstValue::CellState(arg as LangCellState))
        } else {
            self.out_of_range_error.err()
        }
    }
}

/// Built-in function that converts a value to a boolean.
#[derive(Debug)]
pub struct ToBool;
impl ToBool {
    /// Constructs a new ToBool instance.
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for ToBool {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        info.arg_types()[0].typecheck_can_convert_to_bool()?;
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?;
        compiler.build_convert_to_bool(arg).map(|i| Value::Int(i))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let arg = args.const_eval(0)?;
        Ok(ConstValue::Int(arg.to_bool()? as LangInt))
    }
}

/// Built-in function that converts an integer or vector of one length into a
/// vector of another length. Assumes 0 if no argument is supplied.
#[derive(Debug)]
pub struct ToVector {
    /// Length of resultant vector.
    result_len: usize,
}
impl ToVector {
    /// Returns a constructor for a new ToVector instance that constructs a
    /// vector with the given length.
    pub fn with_len(vector_len: Option<usize>) -> FuncConstructor {
        Box::new(move |info| {
            Ok(Box::new(Self {
                result_len: vector_len.unwrap_or(info.userfunc.rule_meta().ndim as usize),
            }))
        })
    }
}
impl Function for ToVector {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.arg_types().len() > 1 {
            Err(info.invalid_args_err())?;
        }
        if let Some(arg) = info.arg_types().get(0) {
            typecheck!(arg, [Int, Vector])?;
        }
        Ok(Type::Vector(self.result_len))
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();

        let arg = if args.len() == 0 {
            Value::Int(compiler.const_int(0))
        } else {
            args.compile(compiler, 0)?
        };

        compiler
            .build_vector_cast(arg, self.result_len)
            .map(Value::Vector)
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();

        let arg = if args.len() == 0 {
            ConstValue::Int(0)
        } else {
            args.const_eval(0)?
        };

        Ok(ConstValue::Vector(arg.coerce_to_vector(self.result_len)?))
    }
}

/// Built-in function that converts an integer, vector, range, or rectangle of
/// one dimensionality into a rectangle of another dimensionality. Assumes 0 if
/// no argument is supplied.
#[derive(Debug)]
pub struct ToRectangle {
    /// Number of dimensions of resultant rectangle.
    result_ndim: usize,
}
impl ToRectangle {
    /// Returns a constructor for a new ToRectangle instance that constructs a
    /// rectangle with the given number of dimensions.
    pub fn with_ndim(rect_ndim: Option<usize>) -> FuncConstructor {
        Box::new(move |info| {
            Ok(Box::new(Self {
                result_ndim: rect_ndim.unwrap_or(info.userfunc.rule_meta().ndim as usize),
            }))
        })
    }
}
impl Function for ToRectangle {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.arg_types().len() > 1 {
            Err(info.invalid_args_err())?;
        }
        if let Some(arg) = info.arg_types().get(0) {
            typecheck!(arg, [Int, Vector, IntRange, Rectangle])?;
        }
        Ok(Type::Rectangle(self.result_ndim))
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();

        let arg = if args.len() == 0 {
            Value::Int(compiler.const_int(0))
        } else {
            args.compile(compiler, 0)?
        };

        compiler
            .build_rectangle_cast(arg, self.result_ndim)
            .map(Value::Rectangle)
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();

        let arg = if args.len() == 0 {
            ConstValue::Int(0)
        } else {
            args.const_eval(0)?
        };

        let (start, end) = arg.coerce_to_rectangle(self.result_ndim)?;
        Ok(ConstValue::Rectangle(start, end))
    }
}
