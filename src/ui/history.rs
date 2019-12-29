use super::*;

#[derive(Default)]
pub struct HistoryStack {
    undo: Vec<HistoryEntry>,
    redo: Vec<HistoryEntry>,
}

pub struct HistoryEntry(GridView);

impl HistoryEntry {
    fn restore(self, current: GridView) -> GridView {
        let mut ret = self.0;
        // Preserve viewport if possible
        match &mut ret {
            GridView::View2D(about_to_restore) => {
                if let GridView::View2D(current_view) = &current {
                    about_to_restore.use_viewport_from(current_view);
                }
            }
            GridView::View3D { .. } => unimplemented!(),
        }
        // Return the GridView.
        ret
    }
}

impl From<GridView> for HistoryEntry {
    fn from(grid_view: GridView) -> Self {
        Self(grid_view)
    }
}
