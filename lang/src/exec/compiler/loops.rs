use crate::llvm;

#[derive(Debug)]
pub struct Loop {
    /// Jump target for a `continue` statement.
    continue_bb: llvm::BasicBlock,
    /// Jump target for a `break` statement.
    break_bb: llvm::BasicBlock,
}
