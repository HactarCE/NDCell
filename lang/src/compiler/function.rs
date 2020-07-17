//! Compiled user function.

use itertools::Itertools;
use std::sync::{mpsc, Arc, Condvar, Mutex};

use super::convert::ValueConverter;
use super::Compiler;
use crate::errors::*;
use crate::{ConstValue, RuleMeta, Type};

/// Raw JIT-compiled function pointer.
type JitFnPtr = unsafe extern "C" fn(*mut usize, *mut u64) -> u32;

/// Bundle of data necessary to keep the `JitFunction` and `ExecutionEngine`
/// alive as long as any `CompiledFunction` exists.
///
/// This is all an awful hack to use Rust's ownership system to preserve an
/// invariant that it would normally uphold more explicitly. If Inkwell
/// implemented `Send`/`Sync` for `ExecutionEngine`, none of this would be
/// necessary.
#[derive(Debug)]
struct SyncStuff {
    counter: Mutex<usize>,
    condvar: Condvar,
}

/// Compiled user function with allocated space for arguments, return value, and
/// optionally debug values to it.
///
/// This struct can be cloned and sent to different threads to run the same
/// function multiple times simultaneously.
#[derive(Debug)]
pub struct CompiledFunction {
    /// This guarantees that the JitFunction (and therefore the ExecutionEngine)
    /// will last as long as this.
    ///
    /// We still have to be careful not to drop the CompiledJitFunction except
    /// in the thread that created it.
    sync_stuff: Arc<SyncStuff>,
    /// Pointer to the JIT function to run.
    jit_fn_ptr: JitFnPtr,

    /// Pointers to argument values, which are passed to the JIT function.
    arg_ptrs: Vec<usize>,
    /// Arguments.
    args: Vec<Argument>,
    /// Return value.
    ret: ValueConverter,

    /// Rule metadata.
    rule_meta: Arc<RuleMeta>,
    /// List of possible runtime errors.
    error_points: Vec<LangError>,
}
impl Clone for CompiledFunction {
    fn clone(&self) -> Self {
        // Increment the counter in the sync stuff.
        *self.sync_stuff.counter.lock().unwrap() += 1;

        Self {
            sync_stuff: self.sync_stuff.clone(),
            jit_fn_ptr: self.jit_fn_ptr.clone(),

            arg_ptrs: self.arg_ptrs.clone(),
            args: self.args.clone(),
            ret: self.ret.clone(),

            rule_meta: self.rule_meta.clone(),
            error_points: self.error_points.clone(),
        }
    }
}
impl Drop for CompiledFunction {
    fn drop(&mut self) {
        // Decrement the counter in the sync stuff.
        let mut counter = self.sync_stuff.counter.lock().unwrap();
        *counter -= 1;
        if *counter == 0 {
            // Notify the thread that created the JIT function that it is ok to
            // drop it now.
            self.sync_stuff.condvar.notify_all();
        }
    }
}
impl CompiledFunction {
    /// Completes the compilation process and returns a compiled function along
    /// the given MPSC channel.
    ///
    /// **This function blocks** until all instances of the CompiledFunction
    /// have been dropped.
    pub fn send_new(
        mpsc_sender: &mpsc::Sender<LangResult<Self>>,
        rule_meta: Arc<RuleMeta>,
        error_points: Vec<LangError>,
        compiler: Compiler,
    ) -> LangResult<()> {
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
        let jit_fn_ptr = unsafe { jit_fn.raw_fn_ptr() };

        let target_data = compiler.target_data();

        // Make a list of all the arguments and the return value.
        let args = compiler
            .function()
            .arg_names
            .iter()
            .map(|name| {
                let ty = compiler.vars()[&**name].ty.clone();
                Argument {
                    name: name.clone(),
                    converter: ValueConverter::new(ty, target_data),
                }
            })
            .collect_vec();
        let ret = ValueConverter::new(compiler.function().return_type.clone(), target_data);
        let arg_ptrs = vec![0; args.len()];

        // Now we have to do the SYNC STUFF.
        let sync_stuff = Arc::new(SyncStuff {
            counter: Mutex::new(1),
            condvar: Condvar::new(),
        });

        mpsc_sender
            .send(Ok(Self {
                sync_stuff: sync_stuff.clone(),
                jit_fn_ptr,

                arg_ptrs,
                args,
                ret,

                rule_meta,
                error_points,
            }))
            .expect("Failed to send compiled rule.");

        // Drop the JIT function when there are no more CompiledFunction
        // instances.
        let mut counter = sync_stuff.counter.lock().unwrap();
        while *counter != 0 {
            counter = sync_stuff.condvar.wait(counter).unwrap();
        }

        // Now drop the JIT function.
        drop(jit_fn);

        Ok(())
    }

    /// Calls this compiled function and returns its return value.
    pub fn call(&mut self, arg_values: &mut [ConstValue]) -> LangResult<ConstValue> {
        // Set argument values.
        if arg_values.len() != self.args.len() {
            panic!("Wrong number of arguments passed to JIT function",);
        }
        self.arg_ptrs.clear();
        self.arg_ptrs.extend(
            self.args
                .iter_mut()
                .zip(arg_values)
                .map(|(arg, arg_value)| {
                    arg.converter.value_to_bytes(arg_value).as_mut_ptr() as usize
                }),
        );
        let args_pointer = self.arg_ptrs.as_mut_ptr();

        // Make space for the return value.
        let mut ret_value = ConstValue::default(self.ret.ty())?;
        let ret_pointer = self.ret.value_to_bytes(&mut ret_value).as_mut_ptr();

        // Call the function.
        let ret: u32 = unsafe { (self.jit_fn_ptr)(args_pointer, ret_pointer) };

        if ret == u32::MAX {
            // No error occurred; get the return value from self.out_bytes.
            self.ret.bytes_to_value().map_err(|_| {
                internal_error_value!("Unable to get return value of type {}", self.ret.ty())
            })
        } else {
            // An error occurred, and the return value holds the error index.
            Err(self
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
        self.args.len()
    }

    /// Returns the metadata for the rule that this function is a part of.
    pub fn rule_meta(&self) -> &Arc<RuleMeta> {
        &self.rule_meta
    }
}

/// Parameter of a JIT function.
#[derive(Debug, Clone)]
struct Argument {
    /// Name of the variable that holds this value (displayed by interactive
    /// debugger).
    name: Arc<String>,
    /// Converter for this value (which keeps track of the type as well).
    converter: ValueConverter,
}
impl Argument {
    pub fn ty(&self) -> &Type {
        self.converter.ty()
    }
}
