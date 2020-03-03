use std::convert::TryFrom;

mod ast;
mod bytecode;
mod optimizer;
mod tokenizer;

use super::*;
use ast::*;
use bytecode::*;
use tokenizer::*;

pub fn compile(source_code: &str) -> LangResult<Program> {
    let tokens = tokenizer::tokenize(source_code)?;
    let ast = ast::TokenFeeder::from(&tokens[..]).program()?;
    let program = bytecode::Program::try_from(&ast[..])?;
    Ok(program)
}
