use codemap::Span;
use std::collections::HashMap;
use std::sync::Arc;

use super::Var;
use crate::data::{CpVal, Type};
use crate::llvm;

/// Collection of basic blocks that specify a loop. All blocks in this struct
/// must be unique. Their predecessor/successor relationships might not be
/// established while a loop is being compiled.
///
/// See https://llvm.org/docs/LoopTerminology.html for terminology explanation.
///
/// To fill in a [`Loop`]:
/// - Add jump(s) from outside the loop to the preheader.
/// - Add instructions (loop contents) at the header, ending with a branch to
///   either the prelatch or the exit.
/// - Add instructions (before looping again) at the prelatch, ending with a
///   jump to the latch.
/// - Add instructions (after exiting loop) at the exit.
///
/// For values which change during each iteration, add a phi instruction to the
/// header block with incoming values from the preheader and latch.
#[derive(Debug)]
pub struct Loop {
    /// Sole entry point for the loop, executed only once immediately before
    /// entering the loop.
    ///
    /// This block has only one successor: the header block.
    pub preheader: llvm::BasicBlock,
    /// First block visited in each loop iteration.
    ///
    /// This block is dominated by the preheader and has exactly two
    /// predecessors: the preheader and the latch.
    pub header: llvm::BasicBlock,

    /// Sole entry point for the next iteration.
    ///
    /// `continue` statements jump here. This block is dominated by the header.
    pub prelatch: llvm::BasicBlock,
    /// Last block visited in each loop iteration.
    pub latch: llvm::BasicBlock,

    /// Sole exit point for the loop, executed only once immediately after
    /// exiting the loop.
    ///
    /// `break` statements jump here. This block is dominated by the header.
    pub exit: llvm::BasicBlock,

    /// Phi node for each variable that is modified in the loop.
    pub vars_modified: HashMap<Arc<String>, VarInLoop>,
}

/// Variable value modified in a loops, used to reconcile values at the
/// beginning and end of a loop.
#[derive(Debug)]
pub struct VarInLoop {
    /// Value in the variable before the loop begins.
    pub pre_loop_var: Var,
    /// Placeholder value for this variable's initial value at the top of the
    /// loop. Iff the variable has a type with no compile-time represenation,
    /// this is `None`.
    pub placeholder_value: Option<CpVal>,
    /// Span of the first use of the placeholder value.
    pub first_use: Option<Span>,

    /// Inputs to the phi node for this variable at the beginning of the loop.
    pub prelatch_phi_inputs: Vec<(Var, llvm::BasicBlock)>,
    /// Inputs to the phi node for this variable at the end of the loop.
    pub exit_phi_inputs: Vec<(Var, llvm::BasicBlock)>,
}
