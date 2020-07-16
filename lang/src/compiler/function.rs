//! Compiled user function.

use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::targets::TargetData;
use itertools::Itertools;
use std::rc::Rc;

use super::convert::ValueConvert;
use super::Compiler;
use crate::errors::*;
use crate::{ConstValue, RuleMeta, Type};

/// Compiled user function with allocated space for arguments, return value, and
/// optionally debug values to it.
///
/// This struct can be cloned and sent to different threads to run the same
/// function multiple times simultaneously.
#[derive(Debug, Clone)]
pub struct CompiledFunction {
    /// Immutable data that is the same, even if this struct is cloned.
    meta: Rc<CompiledFunctionMeta>,
    /// The JIT function to run. (This has an Rc internally.)
    jit_fn: JitFunction<'static, unsafe extern "C" fn(*mut *mut u64, *mut u64) -> u32>,

    /// Pointers to argument values, which are passed to the JIT function.
    arg_pointers: Vec<*mut u64>,
}
impl CompiledFunction {
    /// Completes the compilation process and returns a compiled function.
    ///
    /// The only reason the compiler is owned rather than mutably borrowed is to
    /// get a long-lasting reference to the TargetData.
    pub fn try_new(
        rule_meta: Rc<RuleMeta>,
        error_points: Vec<LangError>,
        compiler: Compiler,
    ) -> LangResult<Self> {
        // Make sure that the LLVM code is valid.
        if !compiler.llvm_fn().verify(true) {
            eprint!(
                "Error encountered during function compilation; dumping LLVM function to stderr"
            );
            compiler.llvm_fn().print_to_stderr();
            internal_error!("LLVM function is invalid! This is a big problem");
        }
        // JIT-compile the function.
        let jit_fn = unsafe { compiler.finish_jit_function() }?;

        // Make a list of all the arguments and the return value.
        let args = compiler
            .vars()
            .iter()
            .filter_map(|(name, var)| {
                if var.is_arg {
                    Some(Parameter {
                        name: name.clone(),
                        ty: var.ty.clone(),
                    })
                } else {
                    None
                }
            })
            .collect_vec();
        let return_type = compiler.function().return_type.clone();
        let arg_pointers = vec![0 as *mut u64; args.len()];

        Ok(Self {
            meta: Rc::new(CompiledFunctionMeta {
                rule_meta,
                error_points,

                args,
                return_type,

                execution_engine: compiler.execution_engine,
            }),
            jit_fn,

            arg_pointers,
        })
    }

    /// Calls this compiled function and returns its return value.
    pub fn call(&self, args: &mut [ConstValue]) -> LangResult<ConstValue> {
        // Set argument values.
        if args.len() != self.meta.args.len() {
            panic!("Wrong number of arguments passed to JIT function",);
        }
        let target_data = self.meta.target_data();
        let mut arg_values = args
            .iter_mut()
            .map(|v| ValueConvert::from_value(v, target_data))
            .collect_vec();
        let mut arg_pointers = arg_values
            .iter_mut()
            .map(|v| v.bytes().as_mut_ptr())
            .collect_vec();
        let args_pointer = arg_pointers.as_mut_ptr();

        // Make space for the return value.
        let mut ret_value = ConstValue::default(&self.meta.return_type)?;
        let mut ret_value_convert = ValueConvert::from_value(&mut ret_value, target_data);
        let ret_pointer = ret_value_convert.bytes().as_mut_ptr();

        // Call the function.
        let ret: u32 = unsafe { self.jit_fn.call(args_pointer, ret_pointer) };

        // Drop arguments AFTER calling the function.
        drop(arg_values);

        if ret == u32::MAX {
            // No error occurred; get the return value from self.out_bytes.
            ret_value_convert.update_value_from_bytes().map_err(|_| {
                internal_error_value!(
                    "Unable to get return value of type {}",
                    self.meta.return_type
                )
            })?;
            Ok(ret_value)
        } else {
            // An error occurred, and the return value holds the error index.
            Err(self
                .meta
                .error_points
                .get(ret as usize)
                .ok_or_else(|| {
                    internal_error_value!("Invalid error index returned from JIT function")
                })?
                .clone())
        }
    }

    /// Returns the number of arguments that this function takes.
    pub fn arg_count(&self) -> usize {
        self.meta.args.len()
    }

    /// Returns the metadata for the rule that this function is a part of.
    pub fn rule_meta(&self) -> &Rc<RuleMeta> {
        &self.meta.rule_meta
    }
}

/// Immutable metadata for a compiled function.
#[derive(Debug)]
struct CompiledFunctionMeta {
    /// Rule metadata.
    rule_meta: Rc<RuleMeta>,
    /// List of possible runtime errors.
    error_points: Vec<LangError>,

    /// Arguments.
    args: Vec<Parameter>,
    /// Return type.
    return_type: Type,

    /// Execution engine used to execute this function. If there were a way to
    /// get owned or refcounted access to the JIT TargetData, then this wouldn't
    /// be necesssary.
    execution_engine: ExecutionEngine<'static>,
}
impl CompiledFunctionMeta {
    fn target_data(&self) -> &TargetData {
        self.execution_engine.get_target_data()
    }
}

/// Parameter of a JIT function.
#[derive(Debug)]
struct Parameter {
    /// Name of the variable that holds this value (displayed by interactive
    /// debugger).
    name: String,
    /// Type of this value.
    ty: Type,
}
