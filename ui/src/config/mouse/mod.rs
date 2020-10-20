use glium::glutin::event::{ModifiersState, MouseButton};
use imgui::MouseCursor;
use std::ops::{Index, IndexMut};

mod click;
mod drag;

pub use click::*;
pub use drag::*;

use crate::commands::{DrawDragCommand, DrawMode, DrawShape, SelectDragCommand, ViewDragCommand};

#[derive(Debug)]
pub struct MouseConfig {
    pub click_bindings_2d: MouseBindings<Option<MouseClickBinding>>,
    pub click_bindings_3d: MouseBindings<Option<MouseClickBinding>>,
    pub drag_bindings_2d: MouseBindings<Option<MouseDragBinding>>,
    pub drag_bindings_3d: MouseBindings<Option<MouseDragBinding>>,
}
impl Default for MouseConfig {
    fn default() -> Self {
        const SHIFT: ModifiersState = ModifiersState::SHIFT;
        const CTRL: ModifiersState = ModifiersState::CTRL;
        const ALT: ModifiersState = ModifiersState::ALT;
        const NONE: ModifiersState = ModifiersState::empty();

        use MouseButton::{Left, Middle, Right};
        use MouseDragBinding::{Draw, Select, View};

        let freeform_2d: DrawDragBinding = DrawDragCommand {
            mode: DrawMode::Replace,
            shape: DrawShape::Freeform,
        }
        .into();
        let line_2d: DrawDragBinding = DrawDragCommand {
            mode: DrawMode::Replace,
            shape: DrawShape::Line,
        }
        .into();

        Self {
            click_bindings_2d: vec![].into_iter().collect(),
            click_bindings_3d: vec![].into_iter().collect(),
            drag_bindings_2d: vec![
                (NONE, Left, Draw(freeform_2d)),
                (SHIFT, Left, Draw(line_2d)),
                (CTRL, Left, Select(SelectDragCommand::NewRect.into())),
                (
                    CTRL | SHIFT,
                    Left,
                    Select(SelectDragCommand::RESIZE_2D_DIAG.into()),
                ),
                (NONE, Right, View(ViewDragCommand::Pan.into())),
                (CTRL, Right, View(ViewDragCommand::Scale.into())),
                (NONE, Middle, View(ViewDragCommand::Pan.into())),
            ]
            .into_iter()
            .collect(),
            drag_bindings_3d: vec![
                (CTRL, Left, Select(SelectDragCommand::NewRect.into())),
                (NONE, Right, View(ViewDragCommand::Orbit.into())),
                (CTRL, Right, View(ViewDragCommand::Scale.into())),
                (NONE, Middle, View(ViewDragCommand::Pan.into())),
                (SHIFT, Middle, View(ViewDragCommand::PanHorizontal.into())),
            ]
            .into_iter()
            .collect(),
        }
    }
}
impl MouseConfig {
    /// Returns a tuple `(click_binding, drag_binding)`.
    pub fn get_bindings(
        &self,
        ndim: usize,
        mods: ModifiersState,
        button: MouseButton,
    ) -> (&Option<MouseClickBinding>, &Option<MouseDragBinding>) {
        match ndim {
            2 => (
                &self.click_bindings_2d[(mods, button)],
                &self.drag_bindings_2d[(mods, button)],
            ),
            3 => (
                &self.click_bindings_3d[(mods, button)],
                &self.drag_bindings_3d[(mods, button)],
            ),
            _ => (&None, &None),
        }
    }
}

#[derive(Debug, Default)]
pub struct MouseBindings<T>([(T, T, T); 8]);

impl<T> Index<ModifiersState> for MouseBindings<T> {
    type Output = (T, T, T);

    fn index(&self, mods: ModifiersState) -> &(T, T, T) {
        &self.0[((mods.shift() as usize) << 0)
            + ((mods.ctrl() as usize) << 1)
            + ((mods.alt() as usize) << 2)]
    }
}
impl<T> IndexMut<ModifiersState> for MouseBindings<T> {
    fn index_mut(&mut self, mods: ModifiersState) -> &mut (T, T, T) {
        &mut self.0[((mods.shift() as usize) << 0)
            + ((mods.ctrl() as usize) << 1)
            + ((mods.alt() as usize) << 2)]
    }
}

impl<T> Index<(ModifiersState, MouseButton)> for MouseBindings<T> {
    type Output = T;

    fn index(&self, (mods, button): (ModifiersState, MouseButton)) -> &T {
        match button {
            MouseButton::Left => &self[mods].0,
            MouseButton::Right => &self[mods].1,
            MouseButton::Middle => &self[mods].2,
            _ => panic!("Cannot assign binding to non-standard mouse button"),
        }
    }
}
impl<T> IndexMut<(ModifiersState, MouseButton)> for MouseBindings<T> {
    fn index_mut(&mut self, (mods, button): (ModifiersState, MouseButton)) -> &mut T {
        match button {
            MouseButton::Left => &mut self[mods].0,
            MouseButton::Right => &mut self[mods].1,
            MouseButton::Middle => &mut self[mods].2,
            _ => panic!("Cannot assign binding to non-standard mouse button"),
        }
    }
}

impl<T> std::iter::FromIterator<(ModifiersState, MouseButton, T)> for MouseBindings<Option<T>> {
    fn from_iter<I: IntoIterator<Item = (ModifiersState, MouseButton, T)>>(iter: I) -> Self {
        let mut ret = Self::default();
        for (mods, button, action) in iter {
            ret[(mods, button)] = Some(action);
        }
        ret
    }
}

/// What to display for the mouse cursor.
///
/// This determines the mouse cursor icon and how/whether to indicate the
/// highlighted cell in the grid.
#[derive(Debug, Copy, Clone)]
pub enum MouseDisplay {
    Normal,
    Pan,
    Draw,
    Select,
    ResizeEW,
    ResizeNS,
    ResizeNESW,
    ResizeNWSE,
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
impl From<&MouseDragBinding> for MouseDisplay {
    fn from(b: &MouseDragBinding) -> Self {
        match b {
            MouseDragBinding::Draw(_) => Self::Draw,
            MouseDragBinding::Select(_) => Self::Select,
            MouseDragBinding::View(v) => {
                use ViewDragCommand::*;
                match v.0 {
                    Orbit => Self::Normal, // TODO: better mouse icon
                    Pan | PanAligned | PanAlignedVertical | PanHorizontal => Self::Pan,
                    Scale => Self::ResizeNS, // TODO: better mouse icon
                }
            }
        }
    }
}
