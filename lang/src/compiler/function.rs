//! Compiled user function.

use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::targets::TargetData;
use std::rc::Rc;

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
    jit_fn: JitFunction<'static, unsafe extern "C" fn(*mut u8, *mut u8) -> u32>,
    /// Bytes used to store arguments and optionally debug values.
    param_bytes: Vec<u8>,
    /// Bytes used to store return value.
    out_bytes: Vec<u8>,
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
                "Error encountered during function compilation; dumping LLVM function to sttderr"
            );
            compiler.llvm_fn().print_to_stderr();
            internal_error!("LLVM function is invalid! This is a big problem");
        }
        // JIT-compile the function.
        let jit_fn = unsafe { compiler.finish_jit_function() }?;

        let target_data = compiler.target_data();

        // Make a list of all the inout values.
        let mut inout_values: Vec<ParamValue> = vec![];
        let mut arg_count = 0;
        for (name, var) in compiler.vars() {
            if let Some(byte_offset) = var.inout_byte_offset {
                if var.is_arg {
                    arg_count += 1;
                }
                inout_values.push(ParamValue {
                    name: name.clone(),
                    ty: var.ty.clone(),
                    byte_offset,
                });
            }
        }
        inout_values.sort_by_key(|v| v.byte_offset);
        // Allocate space for all the inout values.
        let param_bytes = vec![
            0u8;
            target_data.get_store_size(&compiler.function().inout_struct_type.unwrap())
                as usize
        ];

        // Allocate space for the return value.
        let out_type = compiler.function().return_type.clone();
        let out_bytes = vec![0u8; super::types::size_of(&out_type, target_data)];

        Ok(Self {
            meta: Rc::new(CompiledFunctionMeta {
                rule_meta,
                error_points,

                out_type,

                param_values: inout_values,
                arg_count,

                execution_engine: compiler.execution_engine,
            }),
            jit_fn,
            param_bytes,
            out_bytes,
        })
    }

    /// Calls this compiled function and returns its return value.
    pub fn call(&mut self, args: &[ConstValue]) -> LangResult<ConstValue> {
        // Set arguments.
        if args.len() != self.meta.arg_count {
            panic!("Wrong number of arguments passed to JIT function",);
        }
        for (idx, arg) in args.iter().enumerate() {
            self.param_mut(idx).set(arg);
        }

        // Call the function.
        let ret: u32 = unsafe {
            self.jit_fn
                .call(self.param_bytes.as_mut_ptr(), self.out_bytes.as_mut_ptr())
        };
        if ret == u32::MAX {
            // No error occurred; get the return value from self.out_bytes.
            Ok(super::convert::bytes_to_value(
                self.meta.out_type.clone(),
                &self.out_bytes,
                self.meta.target_data(),
            ))
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

    /// Returns a mutable reference to the raw bytes used for arguments and
    /// debug values.
    ///
    /// In order to mutate this safely, the bytes must encode valid ConstValues
    /// in the proper positions matching the LLVM struct.
    pub unsafe fn param_bytes(&mut self) -> &mut [u8] {
        &mut self.param_bytes
    }

    /// Returns the number of argument that this function takes.
    pub fn arg_count(&self) -> usize {
        self.meta.arg_count
    }
    /// Returns the number of in/out values of this function (including
    /// arguments).
    pub fn param_count(&self) -> usize {
        self.meta.param_values.len()
    }
    /// Returns a mutable reference to an in/out value of this function.
    pub fn param_mut<'a>(&'a mut self, idx: usize) -> ParamValueMut<'a> {
        let value = self
            .meta
            .param_values
            .get(idx)
            .expect("Invalid argument index for JIT function");
        let start = value.byte_offset;
        let end = start + super::types::size_of(&value.ty, self.meta.target_data());
        ParamValueMut {
            name: &value.name,
            ty: &value.ty,
            bytes: &mut self.param_bytes[start..end],
            target_data: self.meta.target_data(),
        }
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

    /// The return type of this function.
    out_type: Type,

    /// List of parameters (excluding the return value) for this function.
    param_values: Vec<ParamValue>,
    /// The number of arguments.
    arg_count: usize,

    /// Execution engine used to execute this function. If there were a way to
    /// get owned or refcounted access to the JIT TargetData, then this wouldn't
    /// be necesssary.
    execution_engine: ExecutionEngine<'static>,
}
impl CompiledFunctionMeta {
    fn arg_values(&self) -> &[ParamValue] {
        &self.param_values[..self.arg_count]
    }
    fn target_data(&self) -> &TargetData {
        self.execution_engine.get_target_data()
    }
}

/// Parameter of a JIT-compiled function.
#[derive(Debug)]
struct ParamValue {
    /// Name of the variable that holds this value (displayed by interactive
    /// debugger).
    name: String,
    /// Type of this value.
    ty: Type,
    /// Byte offset in param_bytes.
    byte_offset: usize,
}

/// Mutable reference to a parameter value of a JIT-compiled function.
#[derive(Debug)]
pub struct ParamValueMut<'a> {
    /// Name of the variable that holds this value (displayed by interactive
    /// debugger).
    name: &'a str,
    /// Type of this value.
    ty: &'a Type,
    /// Raw bytes that hold this value.
    bytes: &'a mut [u8],
    /// LLVM TargetData.
    target_data: &'a TargetData,
}
impl<'a> ParamValueMut<'a> {
    /// Returns the name of the variable that holds this value.
    pub fn name(&self) -> &'a str {
        self.name
    }
    /// Returns the type of this value.
    pub fn ty(&self) -> &'a Type {
        self.ty
    }
    /// Returns the value.
    pub fn get(&self) -> ConstValue {
        super::convert::bytes_to_value(self.ty.clone(), self.bytes, self.target_data)
    }
    /// Sets the value.
    ///
    /// Panics if given a value of the wrong type.
    pub fn set(&mut self, value: &ConstValue) {
        assert_eq!(
            &value.ty(),
            self.ty,
            "Wrong type for parameter value in JIT function",
        );
        super::convert::value_to_bytes(value, self.bytes, self.target_data);
    }
}
