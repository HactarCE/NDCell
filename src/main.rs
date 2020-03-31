use inkwell::context::Context;
use std::error::Error;

mod ast;
mod compiler;
mod errors;
mod interpreter;
mod span;
pub use span::{Span, Spanned};

use codegen::CodeGen;

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let codegen = CodeGen::new(&context)?;

    let sum = codegen
        .jit_compile_sum()
        .ok_or("Unable to JIT compile `sum`")?;

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
        assert_eq!(sum.call(x, y, z), x + y + z);
    }

    Ok(())
}
