use ndcell_core::prelude::*;

use super::view2d::Zoom2D;

// TODO: Document all these commands!

#[derive(Debug, Clone)]
pub enum Command {
    Sim(SimCommand),
    History(HistoryCommand),
    Move2D(MoveCommand2D, Interpolation),
    StartDraw,
    EndDraw,
    Draw2D(DrawCommand2D),
    Clipboard(ClipboardCommand),
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
pub enum MoveCommand2D {
    PanPixels(FVec2D),
    Zoom {
        power: f64,
        invariant_pos: Option<FixedVec2D>,
    },
    SetPos(FixedVec2D),
    SetZoom {
        zoom: Zoom2D,
        invariant_pos: Option<FixedVec2D>,
    },
    SnapPos,
    SnapZoom(Option<FixedVec2D>),
}
impl MoveCommand2D {
    pub fn direct(self) -> Command {
        Command::Move2D(self, Interpolation::Direct)
    }
    pub fn decay(self) -> Command {
        Command::Move2D(self, Interpolation::Decay)
    }
}

#[derive(Debug, Clone)]
pub enum DrawCommand2D {
    Cell(BigVec2D, u8),
    Line(BigVec2D, BigVec2D, u8),
}
impl From<DrawCommand2D> for Command {
    fn from(c: DrawCommand2D) -> Self {
        Self::Draw2D(c)
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
