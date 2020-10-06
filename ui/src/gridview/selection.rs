use ndcell_core::prelude::*;

use crate::config::Config;

pub type Selection2D = Selection<Dim2D>;
pub type Selection3D = Selection<Dim3D>;

#[derive(Debug, Clone)]
pub struct Selection<D: Dim> {
    pub rect: BigRect<D>,
    pub cells: Option<NdTree<D>>,
}
impl<D: Dim> Selection<D> {
    pub fn restore_history_entry(
        config: &Config,
        mut current: &mut Option<Self>,
        entry: Option<Self>,
    ) -> Option<Self> {
        let current_has_cells = current.as_ref().and_then(|s| s.cells.as_ref()).is_some();
        let entry_has_cells = entry.as_ref().and_then(|s| s.cells.as_ref()).is_some();

        if config.hist.record_select || entry_has_cells {
            std::mem::replace(&mut current, entry)
        } else if current_has_cells {
            std::mem::replace(&mut current, None)
        } else {
            current.clone()
        }
    }
}
