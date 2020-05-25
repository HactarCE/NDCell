//! Compiled user function.

use inkwell::execution_engine::JitFunction;
use std::rc::Rc;

use super::Compiler;
use crate::errors::*;
use crate::{ConstValue, Type};
use LangErrorMsg::InternalError;

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
    inout_bytes: Vec<u8>,
    /// Bytes used to store return value.
    out_bytes: Vec<u8>,
}
impl CompiledFunction {
    /// Completes the compilation process and returns a compiled function.
    pub fn try_new(
        source_code: Rc<String>,
        error_points: Vec<LangError>,
        compiler: &mut Compiler,
    ) -> LangResult<Self> {
        // Make sure that the LLVM code is valid.
        if !compiler.llvm_fn().verify(true) {
            eprint!(
                "Error encountered during function compilation; dumping LLVM function to sttderr"
            );
            compiler.llvm_fn().print_to_stderr();
            Err(InternalError(
                "LLVM function is invalid! This is a big problem".into(),
            ))?;
        }
        // JIT-compile the function.
        let jit_fn = unsafe { compiler.get_jit_function() }?;

        // Make a list of all the inout values.
        let mut inout_values: Vec<InOutValue> = vec![];
        let mut arg_count = 0;
        for (name, var) in compiler.vars() {
            if let Some(byte_offset) = var.inout_byte_offset {
                if var.is_arg {
                    arg_count += 1;
                }
                inout_values.push(InOutValue {
                    name: name.clone(),
                    ty: var.ty,
                    byte_offset,
                });
            }
        }
        inout_values.sort_by_key(|v| v.byte_offset);
        // Allocate space for all the inout values.
        let inout_bytes = vec![
            0u8;
            compiler
                .execution_engine
                .get_target_data()
                .get_store_size(&compiler.function().inout_struct_type.unwrap())
                as usize
        ];

        // Allocate space for the return value.
        let out_type = compiler.function().return_type;
        let out_bytes = vec![0u8; out_type.size_of().unwrap()];

        Ok(Self {
            meta: Rc::new(CompiledFunctionMeta {
                source_code,
                error_points,

                out_type,

                inout_values,
                arg_count,
            }),
            jit_fn,
            inout_bytes,
            out_bytes,
        })
    }

    // TODO: document
    pub fn set_args(&mut self, args: &[ConstValue]) {
        if args.len() != self.meta.arg_count {
            panic!("Wrong number of arguments passed to JIT function",);
        }
        // TODO these aren't great variable names
        // maybe "Param" instead of "InOutValue"?
        for (idx, arg) in args.iter().enumerate() {
            self.value_mut(idx).set(arg);
        }
    }

    /// Calls this compiled function and returns its return value.
    pub fn call(&mut self) -> LangResult<ConstValue> {
        let ret: u32 = unsafe {
            self.jit_fn
                .call(self.inout_bytes.as_mut_ptr(), self.out_bytes.as_mut_ptr())
        };
        if ret == u32::MAX {
            // No error occurred; get the return value from self.out_bytes.
            Ok(ConstValue::from_bytes(self.meta.out_type, &self.out_bytes))
        } else {
            // An error occurred, and the return value holds the error index.
            Err(self
                .meta
                .error_points
                .get(ret as usize)
                .ok_or_else(|| {
                    InternalError("Invalid error index returned from JIT function".into())
                        .without_span()
                })?
                .clone())
        }
    }

    /// Returns a mutable reference to the raw bytes used for arguments and
    /// debug values.
    ///
    /// In order to mutate this safely, the bytes must encode valid ConstValues
    /// in the proper positions matching the LLVM struct.
    pub unsafe fn inout_bytes(&mut self) -> &mut [u8] {
        &mut self.inout_bytes
    }

    /// Returns the number of argument that this function takes.
    pub fn arg_count(&self) -> usize {
        self.meta.arg_count
    }
    /// Returns the number of in/out values of this function (including
    /// arguments).
    pub fn value_count(&self) -> usize {
        self.meta.inout_values.len()
    }
    /// Returns a mutable reference to an in/out value of this function.
    pub fn value_mut<'a>(&'a mut self, idx: usize) -> InOutValueMut<'a> {
        let value = self
            .meta
            .inout_values
            .get(idx)
            .expect("Invalid argument index for JIT function");
        let start = value.byte_offset;
        let end = start + value.ty.size_of().unwrap();
        InOutValueMut {
            name: &value.name,
            ty: value.ty,
            bytes: &mut self.inout_bytes[start..end],
        }
    }

    /// Returns the source code of the rule that this function is part of.
    pub fn source_code(&self) -> &Rc<String> {
        &self.meta.source_code
    }
}

/// Immutable metadata for a compiled function.
#[derive(Debug)]
struct CompiledFunctionMeta {
    /// Raw source code.
    source_code: Rc<String>,
    /// List of possible runtime errors.
    error_points: Vec<LangError>,

    /// The return type of this function.
    out_type: Type,

    /// List of in/out values (excluding the return value) for this function,
    /// with arguments first in order.
    inout_values: Vec<InOutValue>,
    /// The number of arguments.
    arg_count: usize,
}
impl CompiledFunctionMeta {
    fn arg_values(&self) -> &[InOutValue] {
        &self.inout_values[..self.arg_count]
    }
}

/// Input or hybrid input/output value of a JIT-compiled function.
#[derive(Debug)]
struct InOutValue {
    /// Name of the variable that holds this value (displayed by interactive
    /// debugger).
    name: String,
    /// Type of this value.
    ty: Type,
    /// Byte offset in inout_bytes.
    byte_offset: usize,
}

/// Mutable reference to an in/out value of a JIT-compiled function.
#[derive(Debug)]
pub struct InOutValueMut<'a> {
    /// Name of the variable that holds this value (displayed by interactive
    /// debugger).
    name: &'a str,
    /// Type of this value.
    ty: Type,
    /// Raw bytes that hold this value.
    bytes: &'a mut [u8],
}
impl<'a> InOutValueMut<'a> {
    /// Returns the name of the variable that holds this value.
    pub fn name(&self) -> &'a str {
        self.name
    }
    /// Returns the type of this value.
    pub fn ty(&self) -> Type {
        self.ty
    }
    /// Returns the value.
    pub fn get(&self) -> ConstValue {
        ConstValue::from_bytes(self.ty, self.bytes)
    }
    /// Sets the value.
    ///
    /// Panics if given a value of the wrong type.
    pub fn set(&mut self, value: &ConstValue) {
        assert_eq!(
            value.ty(),
            self.ty,
            "Wrong type for shared value in JIT function"
        );
        value.set_bytes(self.bytes);
    }
}
