use ndcell_core::ndvec::FVec2D;

use crate::commands::*;
use crate::config::MouseDisplay;

#[derive(Debug, Clone)]
pub enum MouseDragBinding {
    Draw(DrawDragBinding),
    Select(SelectDragBinding),
    View(ViewDragBinding),
}
impl MouseDragBinding {
    pub fn display(&self) -> MouseDisplay {
        match self {
            Self::Draw(_) => MouseDisplay::Draw,
            Self::Select(_) => MouseDisplay::Select,
            Self::View(_) => MouseDisplay::Pan,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DrawDragBinding {
    pub mode: DrawMode,
    pub shape: DrawShape,
}
impl DrawDragBinding {
    pub fn to_command(self, cursor_pos: FVec2D, selected_cell_state: u8) -> Command {
        DrawCommand::Drag(
            DrawDragCommand {
                mode: self.mode,
                shape: self.shape,
                cell_state: selected_cell_state,
            },
            cursor_pos,
        )
        .into()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SelectDragBinding(pub SelectDragCommand);
impl SelectDragBinding {
    pub fn to_command(self, cursor_pos: FVec2D) -> Command {
        SelectCommand::Drag(self.0, cursor_pos).into()
    }
}
impl From<SelectDragCommand> for SelectDragBinding {
    fn from(c: SelectDragCommand) -> Self {
        Self(c)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ViewDragBinding(pub ViewDragCommand);
impl ViewDragBinding {
    pub fn to_command(self, cursor_pos: FVec2D) -> Command {
        ViewCommand::Drag(self.0, cursor_pos).into()
    }
}
impl From<ViewDragCommand> for ViewDragBinding {
    fn from(c: ViewDragCommand) -> Self {
        Self(c)
    }
}
