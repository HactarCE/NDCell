use cgmath::Rad;

use ndcell_core::prelude::*;

use super::camera::Scale;

#[derive(Debug, Clone)]
pub enum Command {
    Sim(SimCommand),
    History(HistoryCommand),
    Move(MoveCommand),
    Draw(DrawCommand),
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
impl From<SimCommand> for Command {
    fn from(c: SimCommand) -> Self {
        Self::Sim(c)
    }
}

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
pub enum MoveCommand {
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
        yaw: Option<Rad<f32>>,
        pitch: Option<Rad<f32>>,
        relative: bool,
        scaled: bool,
    },
    GoToScale(Scale),

    Pan {
        /// Cursor pixel position before event.
        start: FVec2D,
        /// Cursor pixel position after event.
        end: FVec2D,
    },
    Orbit {
        /// Cursor pixel position before event.
        start: FVec2D,
        /// Cursor pixel position after event.
        end: FVec2D,
    },
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
impl From<MoveCommand> for Command {
    fn from(c: MoveCommand) -> Self {
        Self::Move(c)
    }
}
impl MoveCommand {
    /// Returns whether to interpolate the camera as it undergoes this movement.
    pub fn is_interpolating(&self) -> bool {
        match self {
            Self::GoTo2D { .. } => true,
            Self::GoTo3D { .. } => true,
            Self::GoToScale(_) => true,
            Self::Pan { .. } => false,
            Self::Orbit { .. } => false,
            Self::Scale { .. } => true,
            Self::SnapPos => true,
            Self::SnapScale { .. } => true,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Start,
    End,
    Draw2D(DrawCommand2D),
    Draw3D(DrawCommand3D),
}
impl From<DrawCommand> for Command {
    fn from(c: DrawCommand) -> Self {
        Self::Draw(c)
    }
}

#[derive(Debug, Clone)]
pub enum DrawCommand2D {
    Cell(BigVec2D, u8),
    Line(BigVec2D, BigVec2D, u8),
}
impl From<DrawCommand2D> for Command {
    fn from(c: DrawCommand2D) -> Self {
        DrawCommand::Draw2D(c).into()
    }
}

#[derive(Debug, Clone)]
pub enum DrawCommand3D {
    Cell(BigVec3D, u8),
    Line(BigVec3D, BigVec3D, u8),
}
impl From<DrawCommand3D> for Command {
    fn from(c: DrawCommand3D) -> Self {
        DrawCommand::Draw3D(c).into()
    }
}

#[derive(Debug, Clone)]
pub enum ClipboardCommand {
    CopyRle,
    CopyCxrle,
    Paste,
}
impl From<ClipboardCommand> for Command {
    fn from(c: ClipboardCommand) -> Self {
        Self::Clipboard(c)
    }
}

#[derive(Debug, Clone)]
pub enum Interpolation {
    Direct,
    Decay,
}
