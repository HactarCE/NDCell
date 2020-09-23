use ndcell_core::prelude::*;

use super::camera::Scale;

#[derive(Debug, Clone)]
pub enum Command {
    Sim(SimCommand),
    History(HistoryCommand),
    Move(MoveCommand, Interpolation),
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
    SnapPos,

    Scale { log2_factor: R64 },
    SetScale { scale: Scale },
    SnapScale,

    Move2D(MoveCommand2D),
    Move3D(MoveCommand3D),
}
impl MoveCommand {
    pub fn direct(self) -> Command {
        Command::Move(self, Interpolation::Direct)
    }
    pub fn decay(self) -> Command {
        Command::Move(self, Interpolation::Decay)
    }
}

#[derive(Debug, Clone)]
pub enum MoveCommand2D {
    PanPixels(FixedVec2D),
    SetPos(FixedVec2D),

    Scale {
        log2_factor: R64,
        invariant_pos: Option<FixedVec2D>,
    },
    SetScale {
        scale: Scale,
        invariant_pos: Option<FixedVec2D>,
    },
    SnapScale {
        invariant_pos: Option<FixedVec2D>,
    },
}
impl MoveCommand2D {
    pub fn direct(self) -> Command {
        MoveCommand::Move2D(self).direct()
    }
    pub fn decay(self) -> Command {
        MoveCommand::Move2D(self).decay()
    }
}

#[derive(Debug, Clone)]
pub enum MoveCommand3D {
    PanPixels {
        start: FVec2D,
        end: FVec2D,
    },
    MovePixels(FixedVec3D),
    SetPos(FixedVec3D),

    RotPixels(FixedVec2D),
    SetPitch(f64),
    SetYaw(f64),

    Scale {
        log2_factor: R64,
        invariant_pos: Option<FixedVec3D>,
    },
    SetScale {
        scale: Scale,
        invariant_pos: Option<FixedVec3D>,
    },
    SnapScale {
        invariant_pos: Option<FixedVec3D>,
    },
}
impl MoveCommand3D {
    pub fn direct(self) -> Command {
        MoveCommand::Move3D(self).direct()
    }
    pub fn decay(self) -> Command {
        MoveCommand::Move3D(self).decay()
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
