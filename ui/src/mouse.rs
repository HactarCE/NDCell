use imgui::MouseCursor;

use ndcell_core::ndvec::FVec2D;

use crate::direction::Direction;

/// What to display for the mouse cursor.
///
/// This determines the mouse cursor icon and how/whether to indicate the
/// highlighted cell in the grid.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MouseDisplay {
    Normal,
    Pan,
    Orbit,
    Scale,
    Draw,
    Select,
    ResizeSelectionEdge(Direction),
    ResizeSelectionAbsolute,
    Move,
}
impl Default for MouseDisplay {
    fn default() -> Self {
        Self::Normal
    }
}
impl MouseDisplay {
    pub fn cursor_icon(self) -> Option<MouseCursor> {
        use MouseCursor::*;
        match self {
            Self::Pan => Some(Arrow),      // TODO: open palm hand
            Self::Orbit => Some(Arrow),    // TODO: some better icon?
            Self::Scale => Some(ResizeNS), // TODO: some better icon?
            Self::Draw => Some(Arrow),     // TODO: pencil
            Self::Select => Some(Arrow),   // TODO: crosshairs/plus
            Self::ResizeSelectionEdge(direction) => match direction {
                Direction::N | Direction::S => Some(ResizeNS),
                Direction::NE | Direction::SW => Some(ResizeNESW),
                Direction::E | Direction::W => Some(ResizeEW),
                Direction::SE | Direction::NW => Some(ResizeNWSE),
            },
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
    pub display: MouseDisplay,
}
