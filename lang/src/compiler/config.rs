use inkwell::OptimizationLevel;

/// Compiler configuration.
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub enable_debug_mode: bool,
    pub optimization_level: OptimizationLevel,
}
impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            enable_debug_mode: false,
            optimization_level: OptimizationLevel::Aggressive,
        }
    }
}
