use inkwell::context::Context;
use std::error::Error;

mod ast;
mod compiler;
mod errors;
mod interpreter;
mod span;
mod types;

pub use span::{Span, Spanned};
use types::Type;

const CELL_STATE_COUNT: usize = 0;

fn main() -> Result<(), ()> {
    let source_code = "
        @transition {
            become #(2 - 1 + 3)
        }
        ";

    // Interpret transition function.
    let result = interpret(source_code);
    match result {
        Ok(ret) => println!("Interpreted transition function output: {:?}", ret),
        Err(err) => {
            println!("Error while interpreting transition function\n{}", err);
            return Err(());
        }
    }

    // Compile and execute transition function.
    let result = compile_and_run(source_code);
    match result {
        Ok(ret) => println!("JIT-compiled transition function output: {:?}", ret),
        Err(err) => {
            println!("Error in compiled transition function\n{}", err);
            return Err(());
        }
    }

    Ok(())
}

fn interpret(source_code: &str) -> Result<interpreter::Value, Box<dyn Error>> {
    let ast = make_ast(source_code)?;
    let mut interpreter = interpreter::State::new(ast.transition_fn);
    loop {
        if let Some(ret) = interpreter
            .step()
            .map_err(|e| Box::new(e.with_source(source_code)))?
            .return_value()
        {
            return Ok(ret);
        }
    }
}

fn compile_and_run(source_code: &str) -> Result<i64, Box<dyn Error>> {
    let ast = make_ast(source_code)?;
    let context = Context::create();
    let compiler = compiler::Compiler::new(&context)?;
    let transition_fn = compiler
        .jit_compile_transition_fn(&ast.transition_fn)
        .map_err(|e| Box::new(e.with_source(source_code)))?;
    Ok(unsafe { transition_fn.call() })
}

fn make_ast(source_code: &str) -> Result<ast::Program, Box<dyn Error>> {
    ast::make_program(source_code).map_err(|e| e.with_source(source_code).into())
}
