use cgmath::Deg;
use palette::{Srgb, Srgba};

use ndcell_core::prelude::*;

use crate::{Direction, Face, Scale};

macro_rules! impl_command_from {
    ( Command::$command_variant:ident($inner:ty) ) => {
        impl From<$inner> for Command {
            fn from(c: $inner) -> Self {
                Self::$command_variant(c)
            }
        }
    };
}

#[derive(Debug, Clone)]
pub enum Command {
    Sim(SimCommand),
    History(HistoryCommand),
    View(ViewCommand),
    Draw(DrawCommand),
    Select(SelectCommand),
    GarbageCollect,

    ContinueDrag(FVec2D),
    StopDrag,

    Cancel,
}

#[derive(Debug, Clone)]
pub enum SimCommand {
    Step(BigInt),
    StepStepSize,

    StartRunning,
    StopRunning,
    ToggleRunning,

    UpdateStepSize,

    Cancel,
}
impl_command_from!(Command::Sim(SimCommand));

#[derive(Debug, Clone)]
pub enum HistoryCommand {
    Undo,
    Redo,
    UndoTo(BigInt),
}
impl From<HistoryCommand> for Command {
    fn from(c: HistoryCommand) -> Self {
        Self::History(c)
    }
}

#[derive(Debug, Clone)]
pub enum ViewCommand {
    Drag(ViewDragCommand, FVec2D),

    GoTo2D {
        x: Option<FixedPoint>,
        y: Option<FixedPoint>,
        relative: bool,
        scaled: bool,
    },
    GoTo3D {
        x: Option<FixedPoint>,
        y: Option<FixedPoint>,
        z: Option<FixedPoint>,
        yaw: Option<Deg<f32>>,
        pitch: Option<Deg<f32>>,
        relative: bool,
        scaled: bool,
    },
    GoToScale(Scale),

    Scale {
        /// Base-2 logarithm of the relative scale factor.
        log2_factor: f64,
        /// Optional invariant position.
        invariant_pos: Option<FVec2D>,
    },

    /// Snap viewpoint to the nearest integer cell position.
    SnapPos,
    /// Snap viewpoint to the nearest power-of-2 scale factor.
    SnapScale {
        /// Optional invariant position.
        invariant_pos: Option<FVec2D>,
    },

    FitView,

    FocusPixel(FVec2D),
}
impl_command_from!(Command::View(ViewCommand));

#[derive(Debug, Copy, Clone)]
pub enum ViewDragCommand {
    /// Rotates the 3D view around the viewpoint pivot.
    Orbit,
    /// Pans in the plane of the camera.
    Pan,
    /// Pans in the nearest axis-aligned plane.
    PanAligned,
    /// Pans in the nearest axis-aligned plane parallel to the Y axis.
    PanAlignedVertical,
    /// Pans in the XZ plane.
    PanHorizontal,
    /// Adjusts scale.
    Scale,
}

#[derive(Debug, Clone)]
pub enum DrawCommand {
    SetState(u8),
    Drag(DrawDragCommand, FVec2D),

    Confirm,
    Cancel,
}
impl_command_from!(Command::Draw(DrawCommand));

#[derive(Debug, Copy, Clone)]
pub struct DrawDragCommand {
    pub mode: DrawMode,
    pub shape: DrawShape,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DrawMode {
    /// Sets the state of a #0 cell to nonzero.
    Place,
    /// Toggles the first cell clicked between the selected cell state and #0
    /// (preferring the selected cell state) and then sets all subsequently
    /// hovered cells to the same state. This is the default behavior in 2D, and
    /// what Golly does.
    Replace,
    /// Sets the state of a nonzero cell to #0. Ignores the selected cell state.
    Erase,
}
impl DrawMode {
    /// Returns the cell state to use when drawing, given the existing state of
    /// the clicked cell and the cell state that is selected.
    pub fn cell_state(self, existing_cell_state: u8, selected_cell_state: u8) -> u8 {
        match self {
            DrawMode::Place => selected_cell_state,
            DrawMode::Replace => {
                if existing_cell_state == selected_cell_state {
                    0_u8
                } else {
                    selected_cell_state
                }
            }
            DrawMode::Erase => 0_u8,
        }
    }

    pub fn fill_color(self) -> Srgba {
        match self {
            DrawMode::Place => crate::colors::hover::PLACE_FILL,
            DrawMode::Replace => crate::colors::hover::REPLACE_FILL,
            DrawMode::Erase => crate::colors::hover::ERASE_FILL,
        }
    }
    pub fn outline_color(self) -> Srgb {
        match self {
            DrawMode::Place => crate::colors::hover::PLACE_OUTLINE,
            DrawMode::Replace => crate::colors::hover::REPLACE_OUTLINE,
            DrawMode::Erase => crate::colors::hover::ERASE_OUTLINE,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DrawShape {
    /// Modifies cells in a freeform path.
    Freeform,
    /// Modifies cells in a straight line.
    Line,
}

#[derive(Debug, Clone)]
pub enum SelectCommand {
    Drag(SelectDragCommand, FVec2D),
    SelectAll,
    Deselect,

    Copy(CaFormat),
    Paste,
    Delete,

    Cancel,
}
impl_command_from!(Command::Select(SelectCommand));

#[derive(Debug, Copy, Clone)]
pub enum SelectDragCommand {
    NewRect,
    Resize2D(Direction),
    Resize3D(Face),
    ResizeToCell,
    MoveSelection,
    MoveCells,
    CopyCells,
}
impl SelectDragCommand {
    pub fn uses_drag_threshold(self) -> bool {
        match self {
            SelectDragCommand::NewRect => true,
            SelectDragCommand::Resize2D(_) => true,
            SelectDragCommand::Resize3D(_) => true,
            SelectDragCommand::ResizeToCell => false,
            SelectDragCommand::MoveSelection => true,
            SelectDragCommand::MoveCells => true,
            SelectDragCommand::CopyCells => true,
        }
    }
}
