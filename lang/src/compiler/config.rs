/// Compiler configuration.
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    /// Whether to enable debug mode.
    pub enable_debug_mode: bool,
}
impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            enable_debug_mode: false,
        }
    }
}
