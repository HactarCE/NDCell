//! Conversion functions.

use inkwell::IntPredicate;
use std::rc::Rc;

use super::FuncResult;
use crate::ast::{
    ArgTypes, ArgValues, ErrorPointRef, Function, FunctionKind, RuleMeta, UserFunction,
};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::lexer::OperatorToken;
use crate::types::{LangCellState, LangInt};
use crate::{ConstValue, Span, Type};
use LangErrorMsg::CellStateOutOfRange;

/// Built-in function that returns the cell state with the given ID.
#[derive(Debug)]
pub struct IntToCellState {
    /// Type to convert from (should be vec![Type::Int]).
    arg_types: ArgTypes,
    /// Rule metadata (used to determine maximum cell state ID).
    rule_meta: Rc<RuleMeta>,
    /// Error returned if the given cell state ID is out of range.
    out_of_range_error: ErrorPointRef,
}
impl IntToCellState {
    /// Constructs a new IntToCellState instance.
    pub fn construct(userfunc: &mut UserFunction, span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self {
            rule_meta: userfunc.rule_meta().clone(),
            out_of_range_error: userfunc.add_error_point(CellStateOutOfRange.with_span(span)),
            arg_types,
        }))
    }
}
impl Function for IntToCellState {
    fn name(&self) -> String {
        format!("unary {:?} operator", OperatorToken::Tag.to_string())
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }
    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        if self.arg_types.len() != 1 {
            Err(self.invalid_args_err(span))?;
        }
        self.arg_types[0].check_eq(Type::Int)?;
        Ok(Type::CellState)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        // Check that the value is a valid cell state.
        let cell_state_value = args.compile(compiler, 0)?.as_int()?;
        // Treat the signed integer as an unsigned integer, and build a
        // condition testing whether that value is less than the number of cell
        // states. (A negative number will be interpreted as a very large
        // positive number, which will be too large.)
        let cell_state_count_value = cell_state_value
            .get_type()
            .const_int(self.rule_meta.states.len() as u64, false);
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
        let cell_state_type = compiler.cell_state_type();
        Ok(Value::CellState(compiler.builder().build_int_cast(
            cell_state_value,
            cell_state_type,
            "tmp_cellStateFromInt",
        )))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let arg = args.const_eval(0)?.as_int()?;
        if 0 <= arg && arg < self.rule_meta.states.len() as LangInt {
            Ok(Some(ConstValue::CellState(arg as LangCellState)))
        } else {
            self.out_of_range_error.err()
        }
    }
}

/// Built-in function that converts a value to a boolean.
#[derive(Debug)]
pub struct ToBool {
    /// Type to convert from (should have a length of 1).
    arg_types: ArgTypes,
}
impl ToBool {
    /// Constructs a new ToBool instance.
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for ToBool {
    fn name(&self) -> String {
        "bool".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Function
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        match self.arg_types.as_slice() {
            [ty] => {
                ty.check_can_convert_to_bool()?;
                Ok(Type::Int)
            }
            _ => Err(self.invalid_args_err(span)),
        }
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let arg = args.compile(compiler, 0)?;
        compiler.build_convert_to_bool(arg).map(|i| Value::Int(i))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        Ok(Some(ConstValue::Int(
            args.const_eval(0)?.to_bool()? as LangInt
        )))
    }
}
