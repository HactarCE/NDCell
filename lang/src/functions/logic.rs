//! Boolean logic functions.

pub use super::enums::LogicalBinOpType;
use super::{FuncConstructor, FuncResult};
use crate::ast::{ArgTypes, ArgValues, Function, FunctionKind, UserFunction};
use crate::compiler::{Compiler, Value};
use crate::errors::*;
use crate::types::LangInt;
use crate::{ConstValue, Span, Type};

/// Built-in function that inverts a boolean value.
#[derive(Debug)]
pub struct LogicalNot {
    /// Type to invert (should have a length of 1).
    arg_types: ArgTypes,
}
impl LogicalNot {
    pub fn construct(_userfunc: &mut UserFunction, _span: Span, arg_types: ArgTypes) -> FuncResult {
        Ok(Box::new(Self { arg_types }))
    }
}
impl Function for LogicalNot {
    fn name(&self) -> String {
        "unary 'not' operator".to_owned()
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        self.check_args_len(span, 1)?;
        self.arg_types[0].typecheck_can_convert_to_bool()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
        let arg = args.compile(compiler, 0)?;
        let arg = compiler.build_convert_to_bool(arg)?;
        let one = compiler.const_uint(1);
        // Flip the lowest bit.
        Ok(Value::Int(compiler.builder().build_xor(one, arg, "not")))
    }
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let arg = args.const_eval(0)?.to_bool()?;
        Ok(Some(ConstValue::Int(!arg as LangInt)))
    }
}

/// Built-in function that performs a fixed two-input boolean logic operation.
#[derive(Debug)]
pub struct LogicalBinaryOp {
    /// Which boolean operation to perform.
    op: LogicalBinOpType,
    /// Types to apply the operation to (should have a length of 2).
    arg_types: ArgTypes,
}
impl LogicalBinaryOp {
    pub fn with_op(op: LogicalBinOpType) -> FuncConstructor {
        Box::new(move |_userfunc, _span, arg_types| Ok(Box::new(Self { op, arg_types })))
    }
}
impl Function for LogicalBinaryOp {
    fn name(&self) -> String {
        format!("binary '{}' operator", self.op)
    }
    fn kind(&self) -> FunctionKind {
        FunctionKind::Operator
    }

    fn arg_types(&self) -> ArgTypes {
        self.arg_types.clone()
    }
    fn return_type(&self, span: Span) -> LangResult<Type> {
        self.check_args_len(span, 2)?;
        self.arg_types[0].typecheck_can_convert_to_bool()?;
        self.arg_types[1].typecheck_can_convert_to_bool()?;
        Ok(Type::Int)
    }

    fn compile(&self, compiler: &mut Compiler, args: ArgValues) -> LangResult<Value> {
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
    fn const_eval(&self, args: ArgValues) -> LangResult<Option<ConstValue>> {
        let lhs = args.const_eval(0)?.to_bool()? as LangInt;
        let rhs = args.const_eval(1)?.to_bool()? as LangInt;
        use LogicalBinOpType::*;
        Ok(Some(ConstValue::Int(match self.op {
            Or => lhs | rhs,
            Xor => lhs ^ rhs,
            And => lhs & rhs,
        })))
    }
}
