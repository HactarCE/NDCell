use ndcell_core::ndvec::FVec2D;

use crate::commands::*;
use crate::config::MouseDisplay;

#[derive(Debug, Copy, Clone)]
pub enum MouseDragBinding {
    Draw(DrawDragBinding),
    Select(SelectDragBinding),
    View(ViewDragBinding),
}
impl MouseDragBinding {
    pub fn display(&self) -> MouseDisplay {
        match self {
            Self::Draw(_) => MouseDisplay::Draw,
            Self::Select(s) => {
                use SelectDragCommand::*;
                match s.0 {
                    NewRect => MouseDisplay::Select,
                    Resize { .. } => MouseDisplay::Normal,
                    ResizeToCell => MouseDisplay::ResizeSelectionAbsolute,
                }
            }
            Self::View(v) => {
                use ViewDragCommand::*;
                match v.0 {
                    Orbit => MouseDisplay::Normal, // TODO: better mouse icon
                    Pan | PanAligned | PanAlignedVertical | PanHorizontal => MouseDisplay::Pan,
                    Scale => MouseDisplay::ResizeNS, // TODO: better mouse icon
                }
            }
        }
    }
    pub fn to_command(self, cursor_pos: FVec2D) -> Command {
        match self {
            Self::Draw(b) => b.to_command(cursor_pos),
            Self::Select(b) => b.to_command(cursor_pos),
            Self::View(b) => b.to_command(cursor_pos),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DrawDragBinding(DrawDragCommand);
impl DrawDragBinding {
    pub fn to_command(self, cursor_pos: FVec2D) -> Command {
        DrawCommand::Drag(self.0, cursor_pos).into()
    }
}
impl From<DrawDragCommand> for DrawDragBinding {
    fn from(c: DrawDragCommand) -> Self {
        Self(c)
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
