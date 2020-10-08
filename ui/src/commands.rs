use cgmath::Deg;

use ndcell_core::prelude::*;

use crate::Scale;

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
    Move(ViewCommand),
    Draw(DrawCommand),
    Select(DragCommand<()>),
    Clipboard(ClipboardCommand),
    GarbageCollect,
}

#[derive(Debug, Clone)]
pub enum SimCommand {
    Step(BigInt),
    StepStepSize,
    StartRunning,
    StopRunning,
    ToggleRunning,
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
pub enum DragCommand<A> {
    Start { action: A, cursor_start: FVec2D },
    Continue { cursor_pos: FVec2D },
    Stop,
}

#[derive(Debug, Clone)]
pub enum ViewCommand {
    Drag(DragCommand<ViewDragAction>),

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

    /// Snap camera to the nearest integer cell position.
    SnapPos,
    /// Snap camera to the nearest power-of-2 scale factor.
    SnapScale {
        /// Optional invariant position.
        invariant_pos: Option<FVec2D>,
    },
}
impl_command_from!(Command::Move(ViewCommand));

#[derive(Debug, Copy, Clone)]
pub enum ViewDragAction {
    /// Rotates the 3D view around the pivot.
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
pub struct DrawCommand(pub DragCommand<(DrawDragAction, u8)>);
impl_command_from!(Command::Draw(DrawCommand));

#[derive(Debug, Copy, Clone)]
pub struct DrawDragAction {
    pub mode: DrawMode,
    pub shape: DrawShape,
}

#[derive(Debug, Copy, Clone)]
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

#[derive(Debug, Copy, Clone)]
pub enum DrawShape {
    /// Modifies cells in a freeform path.
    Freeform,
    /// Modifies cells in a straight line.
    Line,
}

#[derive(Debug, Clone)]
pub enum ClipboardCommand {
    CopyRle,
    CopyCxrle,
    Paste,
}
impl_command_from!(Command::Clipboard(ClipboardCommand));
