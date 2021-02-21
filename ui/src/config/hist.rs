use crate::commands::DragCmd;

#[derive(Debug)]
pub struct HistoryConfig {
    pub record_move_cells: bool,
    pub record_select: bool,
    pub record_view: bool,
    // pub undo_limit: Option<usize>,
    // pub redo_limit: Option<usize>,
}
impl Default for HistoryConfig {
    fn default() -> Self {
        Self {
            record_move_cells: true,
            record_select: false,
            record_view: false,
        }
    }
}
impl HistoryConfig {
    pub fn should_record_history_for_drag_command(&self, command: &DragCmd) -> bool {
        match command {
            DragCmd::View(_) => false,

            DragCmd::DrawFreeform(_) => true,

            DragCmd::SelectNewRect
            | DragCmd::ResizeSelectionToCursor
            | DragCmd::ResizeSelection2D(_)
            | DragCmd::ResizeSelection3D(_)
            | DragCmd::MoveSelection(_) => self.record_select,
            DragCmd::MoveSelectedCells(_) | DragCmd::CopySelectedCells(_) => self.record_move_cells,
        }
    }
}
