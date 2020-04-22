//! A testing ground for a cellular automaton description language for NDCell.
#![allow(dead_code)]
#![warn(missing_docs)]

use inkwell::context::Context;
use std::fs::File;
use std::io::Read;

#[macro_use]
mod macros;

mod ast;
mod compiler;
mod errors;
mod interpreter;
mod span;
mod types;

pub use errors::{CompleteLangResult, LangResult};
pub use span::{Span, Spanned};

const CELL_STATE_COUNT: usize = 100;

fn main() -> Result<(), ()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!(
            "\
Please specify a file to run. E.g.

{} examples{}life.ndca",
            args[0],
            std::path::MAIN_SEPARATOR,
        );
        return Err(());
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Error opening file");
    let mut source_code = String::new();
    file.read_to_string(&mut source_code)
        .expect("Error reading file");

    let rule = ast::make_ndca_rule(&source_code).map_err(|err| {
        println!(
            "Error while parsing rule and generating AST\n{}",
            err.with_source(&source_code)
        );
        ()
    })?;

    println!();
    // Interpret transition function.
    let result = interpret(rule.clone());
    match result {
        Ok(ret) => println!("Interpreted transition function output: {:?}", ret),
        Err(err) => println!(
            "Error while interpreting transition function\n{}",
            err.with_source(&source_code)
        ),
    }

    println!();
    // Compile and execute transition function.
    let result = compile_and_run(rule);
    match result {
        Ok(ret) => println!("JIT-compiled transition function output: {:?}", ret),
        Err(err) => println!(
            "Error in compiled transition function\n{}",
            err.with_source(&source_code)
        ),
    }

    Ok(())
}

/// Runs the given rule's transition function using the interpreter and returns
/// the result.
fn interpret(rule: ast::Rule) -> LangResult<interpreter::Value> {
    let mut interpreter = interpreter::State::new(rule.transition_fn)?;
    loop {
        if let Some(ret) = interpreter.step()?.return_value() {
            return Ok(ret);
        }
    }
}

/// Runs the given rule's transition function using the compiler and returns the
/// result.
fn compile_and_run(rule: ast::Rule) -> LangResult<types::LangCellState> {
    let context = Context::create();
    let mut compiler = compiler::Compiler::new(&context)?;
    let transition_fn = compiler.jit_compile_fn(&rule.transition_fn)?;
    transition_fn.call()
}

#[cfg(test)]
mod tests;
