//! Boolean logic functions.

pub use super::enums::LogicalBinOpType;

use super::{FuncConstructor, FuncResult};
use crate::ast::{FuncCallInfo, FuncCallInfoMut, Function};
use crate::compiler::{const_uint, Compiler, Value};
use crate::errors::*;
use crate::types::LangInt;
use crate::{ConstValue, Type};

/// Built-in function that inverts a boolean value.
#[derive(Debug)]
pub struct LogicalNot;
impl LogicalNot {
    pub fn construct(_info: &mut FuncCallInfoMut) -> FuncResult {
        Ok(Box::new(Self))
    }
}
impl Function for LogicalNot {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(1)?;
        info.arg_types()[0].typecheck_can_convert_to_bool()?;
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let arg = args.compile(compiler, 0)?;
        let arg = compiler.build_convert_to_bool(arg)?;
        // Flip the lowest bit.
        Ok(Value::Int(compiler.builder().build_xor(
            arg,
            const_uint(1),
            "not",
        )))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let arg = args.const_eval(0)?.to_bool()?;
        Ok(ConstValue::Int(!arg as LangInt))
    }
}

/// Built-in function that performs a fixed two-input boolean logic operation.
#[derive(Debug)]
pub struct LogicalBinaryOp {
    /// Which boolean operation to perform.
    op: LogicalBinOpType,
}
impl LogicalBinaryOp {
    pub fn with_op(op: LogicalBinOpType) -> FuncConstructor {
        Box::new(move |_info| Ok(Box::new(Self { op })))
    }
}
impl Function for LogicalBinaryOp {
    fn return_type(&self, info: &mut FuncCallInfoMut) -> LangResult<Type> {
        info.check_args_len(2)?;
        info.arg_types()[0].typecheck_can_convert_to_bool()?;
        info.arg_types()[1].typecheck_can_convert_to_bool()?;
        Ok(Type::Int)
    }
    fn compile(&self, compiler: &mut Compiler, info: FuncCallInfo) -> LangResult<Value> {
        let args = info.arg_values();
        let lhs = args.compile(compiler, 0)?;
        let rhs = args.compile(compiler, 1)?;
        let lhs = compiler.build_convert_to_bool(lhs)?;
        let rhs = compiler.build_convert_to_bool(rhs)?;
        use LogicalBinOpType::*;
        Ok(Value::Int(match self.op {
            Or => compiler.builder().build_or(lhs, rhs, "or"),
            Xor => compiler.builder().build_xor(lhs, rhs, "xor"),
            And => compiler.builder().build_and(lhs, rhs, "and"),
        }))
    }
    fn const_eval(&self, info: FuncCallInfo) -> LangResult<ConstValue> {
        let args = info.arg_values();
        let lhs = args.const_eval(0)?.to_bool()? as LangInt;
        let rhs = args.const_eval(1)?.to_bool()? as LangInt;
        use LogicalBinOpType::*;
        Ok(ConstValue::Int(match self.op {
            Or => lhs | rhs,
            Xor => lhs ^ rhs,
            And => lhs & rhs,
        }))
    }
}
