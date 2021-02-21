use imgui::MouseCursor;

use ndcell_core::ndvec::FVec2D;

use crate::commands::DrawMode;
use crate::{Direction, Face};

/// The way to display the mouse cursor and the cell it is hovering over.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MouseDisplayMode {
    Normal,

    Orbit,
    Pan,
    Scale,

    Draw(DrawMode),

    Select,
    ResizeSelectionToCursor,
    ResizeSelectionEdge(Direction),
    ResizeSelectionFace(Face),

    Move,
}
impl Default for MouseDisplayMode {
    fn default() -> Self {
        Self::Normal
    }
}
impl MouseDisplayMode {
    pub fn cursor_icon(self) -> Option<MouseCursor> {
        use MouseCursor::*;
        match self {
            Self::Orbit => Some(Arrow),    // TODO: some better icon?
            Self::Pan => Some(Arrow),      // TODO: open palm hand
            Self::Scale => Some(ResizeNS), // TODO: some better icon?
            Self::Draw(_) => Some(Arrow),  // TODO: pencil
            Self::Select => Some(Arrow),   // TODO: crosshairs/plus
            Self::ResizeSelectionEdge(direction) => match direction {
                Direction::N | Direction::S => Some(ResizeNS),
                Direction::NE | Direction::SW => Some(ResizeNESW),
                Direction::E | Direction::W => Some(ResizeEW),
                Direction::SE | Direction::NW => Some(ResizeNWSE),
            },
            Self::ResizeSelectionFace(_) => Some(Arrow),
            Self::Move => Some(ResizeAll),
            _ => Some(Arrow),
        }
    }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct MouseState {
    /// Pixel position of the mouse cursor from the top left of the area where
    /// the gridview is being drawn.
    pub pos: Option<FVec2D>,

    /// Whether a mouse button is being held and dragged.
    pub dragging: bool,

    /// What to display for the mouse cursor.
    ///
    /// This determines the mouse cursor icon and how/whether to indicate the
    /// highlighted cell in the grid.
    pub display_mode: MouseDisplayMode,
}
