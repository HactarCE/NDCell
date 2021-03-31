//! JIT-compiled cellular automaton description language for NDCell.

#![allow(dead_code)]
#![warn(missing_docs)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate paste;

use std::sync::{mpsc, Arc};

use ndcell_core::prelude::{Dim, NdRule, NdVec, TransitionFunction};

#[macro_use]
mod utils;
#[macro_use]
pub mod errors;
#[macro_use]
mod data;
mod ast;
// mod compiler;
// mod constvalue;
// mod functions;
mod lexer;
// mod meta;
mod parser;
mod regex;
mod runtime;

// pub use crate::meta::{CellState, RuleMeta};
// pub use constvalue::ConstValue;
pub use data::Type;
pub use errors::{Error, Result};

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

// fn compile_rule(
//     mpsc_sender: &mpsc::Sender<LangResult<compiler::CompiledFunction>>,
//     source_code: Arc<String>,
//     fn_name: Option<String>,
// ) -> LangResult<()> {
//     let rule = ast::make_rule(source_code.clone())?;
//     let compiler = compiler::Compiler::new()?;
//     match fn_name {
//         Some(s) => &rule.helper_functions()[&s],
//         None => rule.transition_function(),
//     }
//     .compile(mpsc_sender, compiler)?;
//     Ok(())
// }

// /// Compiles the specified function of the given source code. If the function
// /// name is None, then the transition function is returned.
// pub fn compile_blocking(
//     source_code: Arc<String>,
//     fn_name: Option<String>,
// ) -> LangResult<compiler::CompiledFunction> {
//     let (tx, rx) = mpsc::channel();
//     std::thread::spawn(move || {
//         if let Err(e) = compile_rule(&tx, source_code, fn_name) {
//             tx.send(Err(e)).expect("MPSC channel error");
//         }
//     });
//     rx.recv()
//         .unwrap_or_else(|e| internal_error!("MPSC channel error: {:?}", e))
// }

// impl<D: Dim> NdRule<D> for compiler::CompiledFunction {
//     fn radius(&self) -> usize {
//         let (min, max) = self.rule_meta().nbhd_shape.bounds().min_and_max();
//         min.into_iter()
//             .zip(max)
//             .map(|(lo, hi)| std::cmp::max(lo.abs(), hi.abs()) as usize)
//             .max()
//             .unwrap()
//     }
//     fn transition_function(&self) -> TransitionFunction<D> {
//         let mut compiled_func = self.clone();
//         Box::new(move |nbhd, rect| {
//             ndcell_core::sim::rule::transition_cell_array(rect, |pos| {
//                 let nbhd_pattern = types::Pattern {
//                     cells: compiled_func
//                         .rule_meta()
//                         .nbhd_shape
//                         .bounds()
//                         .iter()
//                         .map(|offset| {
//                             let offset = NdVec::from_fn(|ax| offset[ax as usize]);
//                             nbhd[(pos.to_ivec() + offset).to_uvec()]
//                         })
//                         .collect(),
//                     shape: compiled_func.rule_meta().nbhd_shape.clone(),
//                     lut: None,
//                 };
//                 compiled_func.call(&mut [ConstValue::Pattern(nbhd_pattern)]).expect("Runtime error in transition function! And I haven't implemented proper error handling yet! AAAHHH").as_cell_state().unwrap()
//             })
//         })
//     }
//     fn max_state(&self) -> u8 {
//         (self.rule_meta().states.len() - 1) as u8
//     }
// }

#[cfg(test)]
#[macro_use]
extern crate itertools;

#[cfg(test)]
mod tests;
