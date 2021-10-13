use inkwell::OptimizationLevel;

/// Compiler configuration.
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    /// LLVM optimization level.
    pub optimization_level: OptimizationLevel,
}
impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            optimization_level: OptimizationLevel::Aggressive,
        }
    }
}
