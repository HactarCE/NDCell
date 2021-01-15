/// Color of the gridlines. This will be configurable in the future.
pub const GRIDLINES: [f32; 4] = [0.25, 0.25, 0.25, 1.0];

/// Color given to the hovered cell when drawing. This will be configurable in
/// the future.
pub const HOVERED_DRAW: [f32; 4] = [0.0, 0.5, 1.0, 1.0];

/// Color given to the hovered cell when selecting. This will be configurable in
/// the future.
pub const HOVERED_SELECT: [f32; 4] = [1.0, 1.0, 1.0, 1.0];

/// Color given to the selection resize preview. This will be configurable in
/// the future.
pub const SELECTION_RESIZE: [f32; 4] = [0.0, 0.5, 1.0, 0.4];

/// Color given to the selection. This will be configurable in the future.
pub const SELECTION: [f32; 4] = [0.6, 0.6, 0.6, 0.8];

/// 2D cell background color. This will be configurable in the future.
pub const BACKGROUND_2D: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

/// 3D cell background color. This will be configurable in the future.
pub const BACKGROUND_3D: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

/// Color for dead cells. This will be configurable in the future.
pub const DEAD: [u8; 4] = [0, 0, 0, 0];

/// Color for live cells. This will be configurable in the future.
pub const LIVE: [u8; 4] = [255, 255, 255, 255];
