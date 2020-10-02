use ndcell_core::num::BigInt;

#[derive(Debug)]
pub struct SimConfig {
    pub step_size: BigInt,
    pub use_breakpoint: bool,
    pub breakpoint_gen: BigInt,
    pub max_memory: usize,
}
impl Default for SimConfig {
    fn default() -> Self {
        Self {
            step_size: 4.into(),
            use_breakpoint: false,
            breakpoint_gen: 0.into(),
            max_memory: 1024 * 1024 * 1024, // 1 GiB
        }
    }
    // TODO: consider adding setter for step_size
}
