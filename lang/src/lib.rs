//! A testing ground for a cellular automaton description language for NDCell.
#![allow(dead_code)]
#![warn(missing_docs)]

#[macro_use]
extern crate lazy_static;

// use std::fs::File;
// use std::io::Read;
use std::rc::Rc;

#[macro_use]
mod utils;
#[macro_use]
mod errors;
#[macro_use]
mod types;
mod ast;
mod compiler;
mod constvalue;
mod functions;
mod lexer;
mod meta;
mod parser;
mod span;

pub use crate::meta::{CellState, RuleMeta};
pub use constvalue::ConstValue;
pub use errors::CompleteLangResult;
pub use span::{Span, Spanned};
pub use types::Type;

use errors::LangResult;

/// Maximum number of dimensions.
pub const MAX_NDIM: usize = 6;
/// Maximum neighborhood radius.
pub const MAX_RADIUS: usize = 32;
/// Maximum number of states.
pub const MAX_STATE_COUNT: usize = 256;
/// Maximum pattern size.
pub const MAX_PATTERN_SIZE: usize = 4096;

/// Number of dimensions to use when the user doesn't specify.
const DEFAULT_NDIM: usize = 2;
/// Neighborhood radius to use when the user doesn't specify.
const DEFAULT_NBHD_RADIUS: usize = 1;
/// Number of states to use when the user doesn't specify.
const DEFAULT_STATE_COUNT: usize = 2;

/// Reserved throwaway variable name.
const THROWAWAY_VARIABLE: &'static str = "_";

// fn main() -> Result<(), ()> {
//     let args: Vec<String> = std::env::args().collect();
//     if args.len() != 2 {
//         eprintln!(
//             "\
// Please specify a file to run. E.g.

// {} examples{}life.ndca",
//             args[0],
//             std::path::MAIN_SEPARATOR,
//         );
//         Err(())?;
//     }

//     let filename = &args[1];
//     let mut file = File::open(filename).expect("Error opening file");
//     let mut source_code = String::new();
//     file.read_to_string(&mut source_code)
//         .expect("Error reading file");
//     let source_code = Rc::new(source_code);

//     let result = compile_and_run(source_code.clone());
//     match result {
//         Ok(ret) => println!("JIT-compiled transition function output: {:?}", ret),
//         Err(err) => {
//             eprintln!("{}", err.with_source(&source_code));
//             Err(())?
//         }
//     }

//     Ok(())
// }

/// Runs the given rule's transition function using the compiler and returns the
/// result.
fn compile_and_run(source_code: Rc<String>) -> LangResult<ConstValue> {
    let rule = ast::make_rule(source_code.clone())?;
    let compiler = compiler::Compiler::new()?;
    let transition_function = rule.transition_function().compile(compiler)?;
    transition_function.call(&mut [])
}

#[cfg(test)]
#[macro_use]
extern crate itertools;

#[cfg(test)]
mod tests;
