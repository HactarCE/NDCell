//! Color constants. All of these will be configurable in the future.

use palette::{Srgb, Srgba};

/// Defines an `Rgb` value at compile time.
macro_rules! rgb {
    ($r:expr, $g:expr, $b:expr) => {
        palette::rgb::Rgb {
            red: $r,
            green: $g,
            blue: $b,
            standard: std::marker::PhantomData,
        }
    };
}
/// Defines an `Rgba` value at compile time.
macro_rules! rgba {
    ($r:expr, $g:expr, $b:expr, $a:expr) => {
        palette::Alpha {
            color: rgb!($r, $g, $b),
            alpha: $a,
        }
    };
}

/// 2D background color.
pub const BACKGROUND_2D: Srgb = rgb!(0.0, 0.0, 0.0);
/// 3D background color.
pub const BACKGROUND_3D: Srgb = rgb!(0.0, 0.0, 0.0);

/// Color of the gridlines.
pub const GRIDLINES: Srgba = rgba!(0.25, 0.25, 0.25, 1.0);

/// Crosshair opacity change.
pub const CROSSHAIR_OPACITY: f32 = 0.2;

pub mod debug {
    use super::*;

    /// Gradient color 1 of ND-tree visualization.
    pub const NDTREE_COLOR_1: Srgb = rgb!(1.0, 0.0, 0.0);
    /// Gradient color 2 of ND-tree visualization.
    pub const NDTREE_COLOR_2: Srgb = rgb!(1.0, 1.0, 0.0);
}

pub mod hover {
    use super::*;

    /// Color of the hovered cell when drawing in `PLACE` mode.
    pub const PLACE_FILL: Srgba = rgba!(0.2, 0.5, 0.2, 0.75);
    /// Color of the outline around the hovered cell when drawing in `PLACE`
    /// mode.
    pub const PLACE_OUTLINE: Srgb = rgb!(0.4, 1.0, 0.4);

    /// Color of the hovered cell when drawing in `REPLACE` mode.
    pub const REPLACE_FILL: Srgba = rgba!(0.0, 0.2, 0.5, 0.75);
    /// Color of the outline around the hovered cell when drawing in `REPLACE`
    /// mode.
    pub const REPLACE_OUTLINE: Srgb = rgb!(0.0, 0.4, 1.0);

    /// Color of the hovered cell when drawing in `ERASE` mode.
    pub const ERASE_FILL: Srgba = rgba!(0.4, 0.1, 0.1, 0.75);
    /// Color of the outline around the hovered cell when drawing in `ERASE`
    /// mode.
    pub const ERASE_OUTLINE: Srgb = rgb!(0.8, 0.2, 0.2);

    /// Color of the hovered cell when selecting.
    pub const SELECT_FILL: Srgba = rgba!(0.6, 0.6, 0.6, 0.75);
    /// Color of the outline around the hovered cell when selecting.
    pub const SELECT_OUTLINE: Srgb = rgb!(0.8, 0.8, 0.8);
}

pub mod selection {
    use super::*;

    /// Color given to the region selection.
    pub const REGION_FILL: Srgba = rgba!(0.4, 0.6, 0.8, 0.25);
    /// Color of the outline around the region selection.
    pub const REGION_OUTLINE: Srgb = rgb!(0.4, 0.6, 0.8);

    /// Color of the cell selection.
    pub const CELLS_FILL: Srgba = rgba!(0.4, 0.6, 0.8, 0.0);
    /// Color of the outline around the cell selection.
    pub const CELLS_OUTLINE: Srgb = rgb!(0.4, 0.6, 0.8);

    /// Color of the selection resize preview.
    pub const RESIZE_FILL: Srgba = rgba!(0.8, 0.4, 0.0, 0.125);
    /// Color of the outline around the selection resize preview.
    pub const RESIZE_OUTLINE: Srgb = rgb!(0.8, 0.4, 0.0);
}

pub mod cells {
    use super::*;

    /// Color for dead cells.
    pub const DEAD: Srgba = rgba!(0.0, 0.0, 0.0, 0.0);
    /// Color for live cells.
    pub const LIVE: Srgba = rgba!(1.0, 1.0, 1.0, 1.0);
}
