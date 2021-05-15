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
#[derive(Debug, Copy, Clone)]
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
}
