//! Compiler intrinsics.

use codemap::{Span, Spanned};
use inkwell::values::{BasicValueEnum, IntMathValue};
use inkwell::IntPredicate;
use inkwell::{types::VectorType, values::BasicValue};
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;

use super::{AssignableCompileValue, FuncCall, Function};
use crate::ast;
use crate::compiler::{Compiler, ParamType};
use crate::data::{
    CpVal, LangInt, LangUint, RtVal, SpannedRuntimeValueExt, SpannedTypeExt, Type, Val,
};
use crate::errors::{Error, Fallible, Result};
use crate::llvm;
use crate::runtime::Runtime;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct CompiledArg;
impl fmt::Display for CompiledArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "__compiled_arg__()")
    }
}
impl Function for CompiledArg {
    fn eval(&self, runtime: &mut Runtime, call: FuncCall<'_>) -> Fallible<RtVal> {
        Err(runtime.error(Error::cannot_const_eval(call.span)))
    }
    fn compile(&self, compiler: &mut Compiler, call: FuncCall<'_>) -> Fallible<Val> {
        let args = call.compile_args(compiler)?;

        if args.len() == 1 {
            let span = args[0].span;

            let idx = compiler
                .get_rt_val(args[0].clone())?
                .as_integer()
                .map_err(|e| compiler.error(e))?
                .try_into()
                .map_err(|_| {
                    compiler.error(Error::custom(span, "compiled arg index out of range"))
                })?;

            let (arg_ty, arg_ptr) = compiler.build_get_arg_ptr(idx, span)?;

            let arg_value = compiler
                .builder()
                .build_load(arg_ptr, &format!("arg_{}", idx));

            Ok(Val::Cp(match arg_ty {
                ParamType::Integer => CpVal::Integer(arg_value.into_int_value()),
                ParamType::Cell => CpVal::Cell(arg_value.into_int_value()),
                ParamType::Vector(_) => CpVal::Vector(arg_value.into_vector_value()),
            }))
        } else {
            let arg_types = args
                .iter()
                .map(|v| compiler.get_val_type(v).map(|ty| ty.node))
                .collect::<Fallible<Vec<Type>>>()?;
            Err(compiler.error(Error::invalid_arguments(call.span, self, &arg_types)))
        }
    }

    fn execute_assign(
        &self,
        runtime: &mut Runtime,
        call: FuncCall,
        _assign_rhs: RtVal,
    ) -> Fallible<crate::runtime::AssignableRuntimeValue<'_>> {
        Err(runtime.error(Error::cannot_const_eval(call.span)))
    }
    fn compile_assign<'f>(
        &self,
        compiler: &mut Compiler,
        call: FuncCall,
    ) -> Fallible<super::AssignableCompileValue<'f>> {
        let args = call.compile_args(compiler)?;

        if args.len() == 1 {
            let span = args[0].span;

            let idx = compiler
                .get_rt_val(args[0].clone())?
                .as_integer()
                .map_err(|e| compiler.error(e))?
                .try_into()
                .map_err(|_| {
                    compiler.error(Error::custom(span, "compiled arg index out of range"))
                })?;

            let (arg_ty, arg_ptr) = compiler.build_get_arg_ptr(idx, span)?;

            Ok(AssignableCompileValue {
                assign_fn: Box::new(move |compiler, new_val| {
                    let new_val = compiler.get_cp_val(new_val)?.llvm_value();
                    compiler.builder().build_store(arg_ptr, new_val);
                    Ok(())
                }),
                get_existing_value: Box::new(move |compiler| {
                    let arg_value = compiler
                        .builder()
                        .build_load(arg_ptr, &format!("arg_{}", idx));

                    Ok(Spanned {
                        node: Val::Cp(match arg_ty {
                            ParamType::Integer => CpVal::Integer(arg_value.into_int_value()),
                            ParamType::Cell => CpVal::Cell(arg_value.into_int_value()),
                            ParamType::Vector(_) => CpVal::Vector(arg_value.into_vector_value()),
                        }),
                        span,
                    })
                }),
            })
        } else {
            let arg_types = args
                .iter()
                .map(|v| compiler.get_val_type(v).map(|ty| ty.node))
                .collect::<Fallible<Vec<Type>>>()?;
            Err(compiler.error(Error::invalid_arguments(call.span, self, &arg_types)))
        }
    }
}
