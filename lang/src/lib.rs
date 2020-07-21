//! A testing ground for a cellular automaton description language for NDCell.
#![allow(dead_code)]
#![warn(missing_docs)]

#[macro_use]
extern crate lazy_static;

use std::sync::{mpsc, Arc};

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
pub const DEFAULT_NDIM: usize = 2;
/// Neighborhood radius to use when the user doesn't specify.
pub const DEFAULT_NBHD_RADIUS: usize = 1;
/// Number of states to use when the user doesn't specify.
pub const DEFAULT_STATE_COUNT: usize = 2;

/// Reserved throwaway variable name.
const THROWAWAY_VARIABLE: &'static str = "_";

fn compile_rule(
    mpsc_sender: &mpsc::Sender<LangResult<compiler::CompiledFunction>>,
    source_code: Arc<String>,
    fn_name: Option<String>,
) -> LangResult<()> {
    let rule = ast::make_rule(source_code.clone())?;
    let compiler = compiler::Compiler::new()?;
    match fn_name {
        Some(s) => &rule.helper_functions()[&s],
        None => rule.transition_function(),
    }
    .compile(mpsc_sender, compiler)?;
    Ok(())
}

/// Compiles the specified function of the given source code. If the function
/// name is None, then the transition function is returned.
pub fn compile_blocking(
    source_code: Arc<String>,
    fn_name: Option<String>,
) -> LangResult<compiler::CompiledFunction> {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        if let Err(e) = compile_rule(&tx, source_code, fn_name) {
            tx.send(Err(e)).expect("MPSC channel error");
        }
    });
    rx.recv()
        .unwrap_or_else(|e| internal_error!("MPSC channel error: {:?}", e))
}

impl ndcell_core::Rule<ndcell_core::Dim2D> for compiler::CompiledFunction {
    fn radius(&self) -> usize {
        let (min, max) = self.rule_meta().nbhd_shape.bounds().min_and_max();
        min.into_iter()
            .zip(max)
            .map(|(lo, hi)| std::cmp::max(lo.abs(), hi.abs()) as usize)
            .max()
            .unwrap()
    }
    fn get_transition_function(&self) -> ndcell_core::TransitionFunction<ndcell_core::Dim2D> {
        let mut compiled_func = self.clone();
        Box::new(move |neighborhood| {
            let nbhd_pattern = types::Pattern {
                cells: compiled_func
                    .rule_meta()
                    .nbhd_shape
                    .bounds()
                    .iter()
                    .map(|pos| neighborhood[&ndcell_core::Vec2D::from_fn(|ax| pos[ax as usize])])
                    .collect(),
                shape: compiled_func.rule_meta().nbhd_shape.clone(),
                lut: None,
            };
            compiled_func.call(&mut [ConstValue::Pattern(nbhd_pattern)]).expect("Runtime error in transition function! And I haven't implemented proper error handling yet! AAAHHH").as_cell_state().unwrap()
        })
    }
}

#[cfg(test)]
#[macro_use]
extern crate itertools;

#[cfg(test)]
mod tests;
