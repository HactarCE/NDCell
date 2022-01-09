//! JIT-compiled function wrapper.

use parking_lot::{Condvar, Mutex};
use std::sync::Arc;

use super::param::Param;
use crate::data::RtVal;
use crate::errors::{Error, Result};
use crate::llvm;

// TODO: add flag to `CompiledFunction` to optimize argument writing/reading for
// the case of a single cell array.

/// Compiled user function with allocated space for arguments, return value, and
/// optionally debug values to it.
///
/// This struct can be cloned and sent to different threads to run the same
/// function multiple times concurrently.
#[derive(Debug)]
pub struct CompiledFunction {
    /// Number of references to the compiled JIT function that exist.
    pub(super) counter: Arc<Mutex<usize>>,
    /// Condition variable to wake the thread that compiled the JIT function
    /// once the last reference is dropped.
    pub(super) condvar: Arc<Condvar>,

    /// Pointer to the JIT function to run.
    jit_fn_ptr: llvm::JitFnPtr,
    /// Parameters to the JIT function.
    params: Arc<Vec<Param>>,
    /// Raw bytes representing arguments passed to JIT function.
    arg_bytes: Vec<u64>,

    /// LLVM source code
    llvm_source: Arc<String>,

    /// List of possible runtime errors.
    error_points: Arc<Vec<Error>>,
}
impl Clone for CompiledFunction {
    fn clone(&self) -> Self {
        // Increment the reference counter.
        *self.counter.lock() += 1;

        Self {
            counter: Arc::clone(&self.counter),
            condvar: Arc::clone(&self.condvar),

            jit_fn_ptr: self.jit_fn_ptr.clone(),
            params: Arc::clone(&self.params),
            arg_bytes: vec![0_u64; self.arg_bytes.len()],

            llvm_source: Arc::clone(&self.llvm_source),

            error_points: Arc::clone(&self.error_points),
        }
    }
}
impl Drop for CompiledFunction {
    fn drop(&mut self) {
        // Decrement the counter in the sync stuff.
        let mut counter = self.counter.lock();
        *counter -= 1;
        if *counter == 0 {
            // Notify the thread that compiled the JIT function that it is ok to
            // drop it now.
            self.condvar.notify_all();
        }
    }
}
impl CompiledFunction {
    pub(super) fn new(
        jit_fn_ptr: llvm::JitFnPtr,
        params: Vec<Param>,
        llvm_source: String,
        error_points: Vec<Error>,
    ) -> Self {
        let arg_bytes_len_u8 = params.last().map(|p| p.offset + p.size).unwrap_or(0);
        let arg_bytes_len_u64 = arg_bytes_len_u8 / std::mem::size_of::<u64>() + 1;

        Self {
            counter: Arc::new(Mutex::new(1)),
            condvar: Arc::new(Condvar::new()),

            jit_fn_ptr,
            params: Arc::new(params),
            arg_bytes: vec![0_u64; arg_bytes_len_u64],

            llvm_source: Arc::new(llvm_source),

            error_points: Arc::new(error_points),
        }
    }

    /// Calls this compiled function and returns its return value.
    pub fn call(&mut self, arg_values: &mut [RtVal]) -> Result<()> {
        let args_pointer = write_args_to_bytes(&mut self.arg_bytes, &self.params, arg_values)?;

        // Call the function.
        let ret: u32 = unsafe { (self.jit_fn_ptr)(args_pointer.as_mut_ptr()) };

        drop(args_pointer);

        // Update argument values.
        read_args_from_bytes(&self.arg_bytes, &self.params, arg_values);

        if ret == u32::MAX {
            // No error occurred.
            Ok(())
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
}

/// Reslice a `&[u64]` as a `&[u8]`. Adapted from
/// https://stackoverflow.com/questions/29037033/.
fn as_u8_slice(v: &mut [u64]) -> &mut [u8] {
    let ptr = v.as_ptr() as *mut u8;
    let len = v.len() * std::mem::size_of::<u64>();
    unsafe { std::slice::from_raw_parts_mut(ptr, len) }
}

fn write_args_to_bytes<'a>(
    bytes_u64: &'a mut [u64],
    params: &[Param],
    values: &'a mut [RtVal],
) -> Result<&'a mut [u64]> {
    // This function aliases a `&mut` and `*mut` to the same memory; once Rust
    // has a formal aliasing model, this may be UB.

    // Reslice as 8-byte-aligned `&mut [u8]`.
    let ptr = bytes_u64.as_ptr() as *mut u8;
    let len = bytes_u64.len() * std::mem::size_of::<u64>();
    let bytes = unsafe { std::slice::from_raw_parts_mut(ptr, len) };

    // Check number of arguments.
    if params.len() != values.len() {
        internal_error!("Wrong number of arguments passed to JIT function");
    }

    // Set argument values.
    for (param, value) in params.iter().zip(values) {
        // SAFETY: The slice is 8-byte aligned and `value` is a valid mutable
        // reference with a lifetime valid for as long as `bytes_u64`.
        param.value_to_bytes(&mut bytes[param.slice_range()], value)?;
    }

    Ok(bytes_u64)
}

fn read_args_from_bytes(bytes_u64: &[u64], params: &[Param], values: &mut [RtVal]) {
    // Reslice as 8-byte-aligned `&[u8]`.
    let ptr = bytes_u64.as_ptr() as *const u8;
    let len = bytes_u64.len() * std::mem::size_of::<u64>();
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };

    // Set argument values.
    for (param, value) in params.iter().zip(values) {
        param.bytes_to_value(&bytes[param.slice_range()], value);
    }
}