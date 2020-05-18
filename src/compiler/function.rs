//! Compiled user function.

use inkwell::execution_engine::JitFunction;
use std::rc::Rc;

use super::super::errors::*;
use super::super::types::LangCellState;
use super::Compiler;
use LangErrorMsg::InternalError;

#[derive(Debug)]
pub struct CompiledFunction {
    source_code: Rc<String>,
    error_points: Vec<LangError>,
    jit_fn: JitFunction<'static, unsafe extern "C" fn() -> u64>,
}
impl CompiledFunction {
    pub fn try_new(
        source_code: Rc<String>,
        error_points: Vec<LangError>,
        compiler: &mut Compiler,
    ) -> LangResult<Self> {
        // Make sure that the LLVM code is valid.
        if !compiler.fn_value().verify(true) {
            eprint!(
                "Error encountered during function compilation; dumping LLVM function to sttderr"
            );
            compiler.fn_value().print_to_stderr();
            Err(InternalError(
                "LLVM function is invalid! This is a big problem".into(),
            ))?;
        }
        let jit_fn = unsafe { compiler.get_jit_function() }?;
        Ok(Self {
            source_code,
            error_points,
            jit_fn,
        })
    }
    pub fn call(&self) -> LangResult<LangCellState> {
        let ret: u64 = unsafe { self.jit_fn.call() };
        if ret & (1 << 63) == 0 {
            Ok(ret as LangCellState)
        } else {
            Err(self
                .error_points
                .get((ret & !(1 << 63)) as usize)
                .ok_or_else(|| InternalError("Got invalid error index".into()).without_span())?
                .clone())
        }
    }
}
