use imgui::MouseCursor;

use ndcell_core::ndvec::FVec2D;

/// What to display for the mouse cursor.
///
/// This determines the mouse cursor icon and how/whether to indicate the
/// highlighted cell in the grid.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MouseDisplay {
    Normal,
    Pan,
    Draw,
    Select,
    ResizeEW,
    ResizeNS,
    ResizeNESW,
    ResizeNWSE,
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
            Self::Pan => Some(Arrow),    // TODO: open palm hand
            Self::Draw => Some(Arrow),   // TODO: pencil
            Self::Select => Some(Arrow), // TODO: crosshairs/plus
            Self::ResizeEW => Some(ResizeEW),
            Self::ResizeNS => Some(ResizeNS),
            Self::ResizeNESW => Some(ResizeNESW),
            Self::ResizeNWSE => Some(ResizeNWSE),
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
