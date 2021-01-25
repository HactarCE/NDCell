use crate::commands::*;

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
            record_select: false, // still restored when moving
            record_view: false,
        }
    }
}
impl HistoryConfig {
    pub fn should_record_select_drag_command(&self, command: SelectDragCommand) -> bool {
        use SelectDragCommand::*;
        match command {
            NewRect | Resize2D(_) | Resize3D(_) | ResizeToCell | MoveSelection => {
                self.record_select
            }
            MoveCells | CopyCells => self.record_move_cells,
        }
    }
}
