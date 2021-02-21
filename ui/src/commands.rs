use cgmath::Deg;
use log::error;
use palette::{Srgb, Srgba};

use ndcell_core::prelude::*;

use crate::mouse::MouseDisplayMode;
use crate::{Direction, Face};

/// Message sent to a `GridView` to enqueue a command.
#[derive(Debug, Clone)]
pub struct CmdMsg {
    pub command: Cmd,
    pub cursor_pos: Option<FVec2D>,
}
impl<T: Into<Cmd>> From<T> for CmdMsg {
    fn from(command: T) -> Self {
        command.into().to_msg()
    }
}

/// Command issued to a `GridView`.
#[derive(Debug, Clone)]
pub enum Cmd {
    /// Starts a drag command.
    BeginDrag(DragCmd),
    /// Continues a drag command with a new mousue cursor position. (hidden)
    ContinueDrag,
    /// Ends a drag command. (hidden)
    EndDrag,

    /// Cancels any operation.
    Cancel,

    /// Undoes one action.
    Undo,
    /// Redoes one action.
    Redo,
    /// Undoes to generation 0.
    Reset,

    /// Moves the viewpoint in 2D using relative coordinates.
    Move2D(Move2D),
    /// Moves the viewpoint in 3D using relative coordinates.
    Move3D(Move3D),
    /// Zooms in or out by a power of 2. (positive = in, negative = out)
    Scale(f64),
    /// Zooms in or out by a power of 2, keeping the position at the mouse
    /// cursor invariant.
    ScaleToCursor(f64),

    /// Snaps the viewpoint to the nearest cell boundary.
    SnapPos,
    /// Snaps the viewpoint to the nearest power-of-2 scale factor.
    SnapScale,
    /// Snaps the viewpoint to the nearest power-of-2 scale factor, keeping the
    /// position at the mouse cursor invariant.
    SnapScaleToCursor,

    /// Moves the viewpoint to the origin.
    ResetView,
    /// Moves the viewpoint to fit the pattern.
    FitView,
    /// Moves the viewpoint to cell at the mouse cursor.
    FocusCursor,

    /// Sets the cell state for drawing.
    SetDrawState(u8),
    /// Selects the next cell state for drawing.
    NextDrawState { wrap: bool },
    /// Selects the previous cell state for drawing.
    PrevDrawState { wrap: bool },
    /// Completes a drawing operation.
    ConfirmDraw,
    /// Cancels a drawing operation.
    CancelDraw,

    /// Selects the entire pattern.
    SelectAll,
    /// Deselects all cells.
    Deselect,
    /// Copies the selection to the clipboard.
    CopySelection(CaFormat),
    /// Pastes the clipboard contents as a selection.
    PasteSelection,
    /// Deletes the selection contents.
    DeleteSelection,
    /// Drops selected cells onto the pattern or deselects all cells.
    CancelSelection,

    /// Advances the simulation by a specific number of generations.
    Step(usize),
    /// Advances the simulation by the configured step size.
    StepStepSize,
    /// Starts running the simulation continuously.
    StartRunning,
    /// Stops running the simulation continuously.
    StopRunning,
    /// Toggles running the simulation continuously.
    ToggleRunning,
    /// Restarts the continuously-running simulation using the newly configured
    /// step size value, if it was already running. (hidden)
    UpdateStepSize,
    /// Cancels any pending simulation requests.
    CancelSim,

    /// Clears the HashLife cache to reduce memory usage.
    ClearCache,
}
impl Cmd {
    /// Returns the way to display the mouse cursor when this is the main
    /// command for the selected tool.
    pub fn mouse_display_mode(&self) -> MouseDisplayMode {
        match self {
            Self::BeginDrag(cmd) => cmd.mouse_display_mode(),

            _ => MouseDisplayMode::Normal,
        }
    }

    /// Creates a `CmdMsg` for this command including a mouse cursor position.
    pub fn at(self, cursor_pos: FVec2D) -> CmdMsg {
        CmdMsg {
            command: self,
            cursor_pos: Some(cursor_pos),
        }
    }
    /// Creates a `CmdMsg` for this command with no mouse cursor position. This
    /// is invalid for some commands, and will log a warning!
    pub fn to_msg(self) -> CmdMsg {
        match &self {
            Cmd::BeginDrag(_)
            | Cmd::ContinueDrag
            | Cmd::ScaleToCursor(_)
            | Cmd::SnapScaleToCursor
            | Cmd::FocusCursor => {
                error!(
                    ".to_msg() called on {:?}, which requires cursor position; use .at() instead",
                    self,
                );
            }
            _ => (),
        }

        CmdMsg {
            command: self,
            cursor_pos: None,
        }
    }
}

/// Command issued to a `GridView` that starts a mouse drag.
#[derive(Debug, Clone)]
pub enum DragCmd {
    /// Drag the viewpoint/camera.
    View(DragViewCmd),

    /// Draws freeform.
    DrawFreeform(DrawMode),

    /// Selects a new rectangle.
    SelectNewRect,
    /// Resizes the selection to the mouse cursor.
    ResizeSelectionToCursor,
    /// Resizes the selection in 2D along a cardinal or intercardinal direction.
    /// (mouse target)
    ResizeSelection2D(Direction),
    /// Resizes the selection in 3D along a cardinal direction. (mouse target)
    ResizeSelection3D(Face),
    /// Moves the selection. (mouse target)
    MoveSelection(Option<Face>),
    /// Moves the selected cells. (mouse target)
    MoveSelectedCells(Option<Face>),
    /// Moves a copy of the selected cells. (mouse target)
    CopySelectedCells(Option<Face>),
}
impl From<DragCmd> for Cmd {
    fn from(cmd: DragCmd) -> Self {
        Cmd::BeginDrag(cmd)
    }
}
impl DragCmd {
    /// Returns the way to display the mouse cursor when this is the main
    /// command for the selected tool.
    pub fn mouse_display_mode(&self) -> MouseDisplayMode {
        match self {
            Self::View(cmd) => cmd.mouse_display_mode(),

            Self::DrawFreeform(mode) => MouseDisplayMode::Draw(*mode),

            Self::SelectNewRect => MouseDisplayMode::Select,
            Self::ResizeSelectionToCursor => MouseDisplayMode::ResizeSelectionToCursor,
            Self::ResizeSelection2D(direction) => MouseDisplayMode::ResizeSelectionEdge(*direction),
            Self::ResizeSelection3D(face) => MouseDisplayMode::ResizeSelectionFace(*face),
            Self::MoveSelection(_) | Self::MoveSelectedCells(_) | Self::CopySelectedCells(_) => {
                MouseDisplayMode::Move
            }
        }
    }
    /// Returns `true` if this drag command always waits for the cursor to move
    /// a certain threshold distance from the initial click before acting.
    /// Returns `false` if the drag command only waits for that treshold when
    /// another action is bound on click.
    pub fn always_uses_movement_threshold(&self) -> bool {
        match self {
            Self::View(cmd) => cmd.always_uses_movement_threshold(),

            Self::DrawFreeform(_) => false,

            Self::SelectNewRect => true,
            Self::ResizeSelectionToCursor => false,
            Self::ResizeSelection2D(_) => true,
            Self::ResizeSelection3D(_) => true,
            Self::MoveSelection(_) => true,
            Self::MoveSelectedCells(_) => true,
            Self::CopySelectedCells(_) => true,
        }
    }

    pub fn is_view_cmd(&self) -> bool {
        match self {
            DragCmd::View(_) => true,

            DragCmd::DrawFreeform(_) => false,

            DragCmd::SelectNewRect
            | DragCmd::ResizeSelectionToCursor
            | DragCmd::ResizeSelection2D(_)
            | DragCmd::ResizeSelection3D(_)
            | DragCmd::MoveSelection(_)
            | DragCmd::MoveSelectedCells(_)
            | DragCmd::CopySelectedCells(_) => false,
        }
    }
    pub fn is_draw_cmd(&self) -> bool {
        match self {
            DragCmd::View(_) => false,

            DragCmd::DrawFreeform(_) => true,

            DragCmd::SelectNewRect
            | DragCmd::ResizeSelectionToCursor
            | DragCmd::ResizeSelection2D(_)
            | DragCmd::ResizeSelection3D(_)
            | DragCmd::MoveSelection(_)
            | DragCmd::MoveSelectedCells(_)
            | DragCmd::CopySelectedCells(_) => false,
        }
    }
    pub fn is_select_cmd(&self) -> bool {
        match self {
            DragCmd::View(_) => false,

            DragCmd::DrawFreeform(_) => false,

            DragCmd::SelectNewRect
            | DragCmd::ResizeSelectionToCursor
            | DragCmd::ResizeSelection2D(_)
            | DragCmd::ResizeSelection3D(_)
            | DragCmd::MoveSelection(_)
            | DragCmd::MoveSelectedCells(_)
            | DragCmd::CopySelectedCells(_) => true,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DragViewCmd {
    /// Rotates the 3D camera around the viewpoint pivot.
    Orbit3D,
    /// Pans in the plane of the camera.
    Pan,
    /// Pans in the XZ plane.
    PanHorizontal3D,
    /// Zooms in or out.
    Scale,
}
impl From<DragViewCmd> for DragCmd {
    fn from(cmd: DragViewCmd) -> Self {
        DragCmd::View(cmd)
    }
}
impl From<DragViewCmd> for Cmd {
    fn from(cmd: DragViewCmd) -> Self {
        Cmd::BeginDrag(DragCmd::View(cmd))
    }
}
impl DragViewCmd {
    /// Returns the way to display the mouse cursor when this is the main
    /// command for the selected tool.
    pub fn mouse_display_mode(&self) -> MouseDisplayMode {
        match self {
            Self::Orbit3D => MouseDisplayMode::Orbit,
            Self::Pan | Self::PanHorizontal3D => MouseDisplayMode::Pan,
            Self::Scale => MouseDisplayMode::Scale,
        }
    }
    pub fn always_uses_movement_threshold(&self) -> bool {
        false
    }
}

/// 2D movement relative to the camera.
#[derive(Debug, Default, Copy, Clone)]
pub struct Move2D {
    pub dx: f64,
    pub dy: f64,
}
impl Move2D {
    pub fn right(n: f64) -> Self {
        Self { dx: n, dy: 0.0 }
    }
    pub fn left(n: f64) -> Self {
        Self { dx: -n, dy: 0.0 }
    }
    pub fn up(n: f64) -> Self {
        Self { dx: 0.0, dy: n }
    }
    pub fn down(n: f64) -> Self {
        Self { dx: 0.0, dy: -n }
    }
}
impl From<Move2D> for Cmd {
    fn from(move2d: Move2D) -> Self {
        Cmd::Move2D(move2d)
    }
}

/// 3D movement relative to the camera.
#[derive(Debug, Copy, Clone)]
pub struct Move3D {
    pub dx: f64,
    pub dy: f64,
    pub dz: f64,
    pub dyaw: Deg<f32>,
    pub dpitch: Deg<f32>,
}
impl Default for Move3D {
    fn default() -> Self {
        Self {
            dx: 0.0,
            dy: 0.0,
            dz: 0.0,
            dyaw: Deg(0.0),
            dpitch: Deg(0.0),
        }
    }
}
impl From<Move3D> for Cmd {
    fn from(move3d: Move3D) -> Self {
        Cmd::Move3D(move3d)
    }
}
impl Move3D {
    pub fn east(n: f64) -> Self {
        Self {
            dx: n,
            ..Default::default()
        }
    }
    pub fn west(n: f64) -> Self {
        Self {
            dx: -n,
            ..Default::default()
        }
    }
    pub fn up(n: f64) -> Self {
        Self {
            dy: n,
            ..Default::default()
        }
    }
    pub fn down(n: f64) -> Self {
        Self {
            dy: -n,
            ..Default::default()
        }
    }
    pub fn north(n: f64) -> Self {
        Self {
            dz: n,
            ..Default::default()
        }
    }
    pub fn south(n: f64) -> Self {
        Self {
            dz: -n,
            ..Default::default()
        }
    }
}

/// How drawing should affect individual cells, depending on their prior state.
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
