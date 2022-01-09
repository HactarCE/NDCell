use glium::glutin::event::{ModifiersState, MouseButton as GlutinMouseButton};
use std::convert::{TryFrom, TryInto};
use std::ops::{Index, IndexMut};

use crate::commands::{Cmd, DragCmd, DragViewCmd, DrawMode};

/// Left, right, or middle mouse button.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum MouseButton {
    Left,
    Right,
    Middle,
}
impl Default for MouseButton {
    fn default() -> Self {
        Self::Left
    }
}
impl TryFrom<GlutinMouseButton> for MouseButton {
    type Error = OtherMouseButton;

    fn try_from(button: GlutinMouseButton) -> Result<Self, Self::Error> {
        match button {
            GlutinMouseButton::Left => Ok(Self::Left),
            GlutinMouseButton::Right => Ok(Self::Right),
            GlutinMouseButton::Middle => Ok(Self::Middle),
            GlutinMouseButton::Other(n) => Err(OtherMouseButton(n)),
        }
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct OtherMouseButton(u16);

#[derive(Debug)]
pub struct MouseConfig {
    pub click_bindings_2d: MouseBindings<Option<Cmd>>,
    pub click_bindings_3d: MouseBindings<Option<Cmd>>,
    pub drag_bindings_2d: MouseBindings<Option<DragCmd>>,
    pub drag_bindings_3d: MouseBindings<Option<DragCmd>>,
    pub drag_threshold: f64,
}
impl Default for MouseConfig {
    fn default() -> Self {
        const SHIFT: ModifiersState = ModifiersState::SHIFT;
        const CTRL: ModifiersState = ModifiersState::CTRL;
        const ALT: ModifiersState = ModifiersState::ALT;
        const NONE: ModifiersState = ModifiersState::empty();

        use MouseButton::{Left, Middle, Right};

        Self {
            click_bindings_2d: vec![].into_iter().collect(),
            click_bindings_3d: vec![].into_iter().collect(),
            drag_bindings_2d: vec![
                (NONE, Left, DragCmd::DrawFreeform(DrawMode::Replace)),
                (CTRL, Left, DragCmd::SelectNewRect),
                (CTRL | SHIFT, Left, DragCmd::ResizeSelectionToCursor),
                (NONE, Right, DragViewCmd::Pan.into()),
                (CTRL, Right, DragViewCmd::Scale.into()),
                (NONE, Middle, DragViewCmd::Pan.into()),
            ]
            .into_iter()
            .collect(),
            drag_bindings_3d: vec![
                (NONE, Left, DragCmd::DrawFreeform(DrawMode::Place)),
                (SHIFT, Left, DragCmd::DrawFreeform(DrawMode::Erase)),
                (CTRL, Left, DragCmd::SelectNewRect),
                (CTRL | SHIFT, Left, DragCmd::ResizeSelectionToCursor),
                (NONE, Right, DragViewCmd::Orbit3D.into()),
                (CTRL, Right, DragViewCmd::Scale.into()),
                (NONE, Middle, DragViewCmd::Pan.into()),
                (SHIFT, Middle, DragViewCmd::PanHorizontal3D.into()),
            ]
            .into_iter()
            .collect(),
            drag_threshold: 2.0,
        }
    }
}
impl MouseConfig {
    /// Returns a tuple `(click_binding, drag_binding)`.
    pub fn get_bindings(
        &self,
        ndim: usize,
        mods: ModifiersState,
        button: GlutinMouseButton,
    ) -> (&Option<Cmd>, &Option<DragCmd>) {
        let button = match button.try_into() {
            Ok(b) => b,
            Err(_) => return (&None, &None),
        };

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

    #[allow(clippy::identity_op)]
    fn index(&self, mods: ModifiersState) -> &(T, T, T) {
        &self.0[((mods.shift() as usize) << 0)
            + ((mods.ctrl() as usize) << 1)
            + ((mods.alt() as usize) << 2)]
    }
}
impl<T> IndexMut<ModifiersState> for MouseBindings<T> {
    #[allow(clippy::identity_op)]
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
        }
    }
}
impl<T> IndexMut<(ModifiersState, MouseButton)> for MouseBindings<T> {
    fn index_mut(&mut self, (mods, button): (ModifiersState, MouseButton)) -> &mut T {
        match button {
            MouseButton::Left => &mut self[mods].0,
            MouseButton::Right => &mut self[mods].1,
            MouseButton::Middle => &mut self[mods].2,
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
