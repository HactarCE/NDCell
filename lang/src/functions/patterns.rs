//! Pattern functions.

use itertools::Itertools;

pub use super::enums::{MinMaxMode, RangeProperty, SumOrProduct};

use super::FuncResult;
use crate::ast::{AssignableFunction, ErrorPointRef, FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::{ConstValue, Type};
use ErrorKind::{AccessOutOfBounds, Unimplemented};

/// Built-in function that returns a single cell state from a cell pattern.
#[derive(Debug)]
pub struct Access {
    /// Error returned if the position is out of bounds.
    out_of_bounds_error: ErrorPointRef,
}
impl Access {
    /// Returns a constructor for a new Access instance that returns the
    /// cell state at the given position.
    pub fn construct(info: &mut FuncCallInfoMut) -> FuncResult {
        let out_of_bounds_error = info.add_error_point(AccessOutOfBounds);
        Ok(Box::new(Self {
            out_of_bounds_error,
        }))
    }
}
impl Function for Access {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        if info.args.len() < 2 {
            Err(info.invalid_args_err())?;
        }
        let arg_types = info.arg_types();
        typecheck!(arg_types[0], Pattern)?;
        for arg_type in &arg_types[1..] {
            typecheck!(arg_type, [Int, Vector])?;
        }
        Ok(Type::CellState)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let mut args = info.arg_values().compile_all(compiler)?.into_iter();
        let pattern = args.next().unwrap().as_pattern()?;
        let pos_args = args;

        // Gather components of the position vector.
        let components = pos_args
            .flat_map(|arg| match arg {
                Value::Int(i) => vec![i],
                Value::Vector(v) => compiler.build_split_vector(v),
                _ => unreachable!(),
            })
            .take(pattern.shape.ndim())
            .collect_vec();
        // And make a vector.
        let pos = compiler.build_construct_vector(&components);

        // Now fetch the cell state from the pattern.
        let ret = compiler.build_get_pattern_cell_state(&pattern, pos, |c| {
            Ok(self.out_of_bounds_error.compile(c))
        })?;

        if pattern.has_lut() {
            Err(Unimplemented.with_span(info.span))?;
        }

        Ok(Value::CellState(ret))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let mut args = info.arg_values().const_eval_all()?.into_iter();
        let pattern = args.next().unwrap().as_pattern()?;
        let pos_args = args;

        // Gather components of the position vector.
        let pos = pos_args
            .flat_map(|arg| match arg {
                ConstValue::Int(i) => vec![i as isize],
                ConstValue::Vector(v) => v.into_iter().map(|i| i as isize).collect_vec(),
                _ => unreachable!(),
            })
            .take(pattern.shape.ndim())
            .collect_vec();

        // Now fetch the cell state from the pattern.
        let cell_idx = pattern
            .shape
            .flatten_idx(&pos)
            .ok_or_else(|| self.out_of_bounds_error.error())?;
        let ret = pattern.cells[cell_idx];

        if pattern.lut.is_some() {
            Err(Unimplemented.with_span(info.span))?;
        }

        Ok(ConstValue::CellState(ret))
    }
    fn as_assignable(&self, info: FuncCallInfo) -> Option<&dyn AssignableFunction> {
        if info.arg_values().can_assign(0) {
            Some(self)
        } else {
            None
        }
    }
}
impl AssignableFunction for Access {
    fn compile_assign(
        &self,
        _compiler: &mut Compiler,
        _value: Value,
        info: FuncCallInfo,
    ) -> LangResult<()> {
        Err(Unimplemented.with_span(info.span))
    }
}
