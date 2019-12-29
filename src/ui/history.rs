use super::*;

#[derive(Default)]
pub struct HistoryStack {
    undo: Vec<HistoryEntry>,
    redo: Vec<HistoryEntry>,
}
impl HistoryStack {
    /// Pushes the given state onto the undo stack and clears the redo stack.
    pub fn record(&mut self, current: GridView) {
        // Erase redo history.
        self.redo.clear();
        // Push new entry.
        self.undo.push(HistoryEntry::from(current));
    }
    /// Returns true if there is something to undo, or false otherwise.
    pub fn has_undo(&self) -> bool {
        !self.undo.is_empty()
    }
    /// Returns true if there is something to redo, or false otherwise.
    pub fn has_redo(&self) -> bool {
        !self.redo.is_empty()
    }
    /// Restores the last state from the undo stack, pushing the current state
    /// onto the redo stack.
    ///
    /// Returns true if the undo was successful, or false if there was nothing
    /// to undo.
    pub fn undo(&mut self, current: &mut GridView) -> bool {
        if let Some(new_state) = self.undo.pop() {
            let redo_state = new_state.restore(current);
            self.redo.push(HistoryEntry::from(redo_state));
            true
        } else {
            false
        }
    }
    /// Restores the next state from the redo stack, pushing the current state
    /// onto the undo stack.
    ///
    /// Returns true if the redo was successful, or false if there was nothing
    /// to redo.
    pub fn redo(&mut self, current: &mut GridView) -> bool {
        if let Some(new_state) = self.redo.pop() {
            let undo_state = new_state.restore(current);
            self.undo.push(HistoryEntry::from(undo_state));
            true
        } else {
            false
        }
    }
}

pub struct HistoryEntry(GridView);
impl From<GridView> for HistoryEntry {
    fn from(grid_view: GridView) -> Self {
        Self(grid_view)
    }
}
impl HistoryEntry {
    /// Replaces the given GridView with the one recorded in this HistoryEntry
    /// and return a new HistoryEntry representing the GridView originally
    /// passed to this method.
    ///
    /// This method is analogous to std::mem::replace().
    pub fn restore(mut self, current: &mut GridView) -> GridView {
        // Preserve viewport if possible.
        match &mut self.0 {
            GridView::View2D(about_to_restore) => {
                if let GridView::View2D(current_view) = &current {
                    about_to_restore.use_viewport_from(current_view);
                }
            }
            GridView::View3D { .. } => unimplemented!(),
        }
        // Perform the replacement.
        std::mem::replace(current, self.0)
    }
}
