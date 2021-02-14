use glium::glutin::event::{ModifiersState, MouseButton};
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
    pub drag_threshold: f64,
}
impl Default for MouseConfig {
    fn default() -> Self {
        const SHIFT: ModifiersState = ModifiersState::SHIFT;
        const CTRL: ModifiersState = ModifiersState::CTRL;
        const ALT: ModifiersState = ModifiersState::ALT;
        const NONE: ModifiersState = ModifiersState::empty();

        use MouseButton::{Left, Middle, Right};
        use MouseDragBinding::{Draw, Select, View};

        let replace_freeform: DrawDragBinding = DrawDragCommand {
            mode: DrawMode::Replace,
            shape: DrawShape::Freeform,
        }
        .into();
        let place_line: DrawDragBinding = DrawDragCommand {
            mode: DrawMode::Place,
            shape: DrawShape::Line,
        }
        .into();
        let place_freeform: DrawDragBinding = DrawDragCommand {
            mode: DrawMode::Place,
            shape: DrawShape::Freeform,
        }
        .into();
        let erase_freeform: DrawDragBinding = DrawDragCommand {
            mode: DrawMode::Erase,
            shape: DrawShape::Freeform,
        }
        .into();

        Self {
            click_bindings_2d: vec![].into_iter().collect(),
            click_bindings_3d: vec![].into_iter().collect(),
            drag_bindings_2d: vec![
                (NONE, Left, Draw(replace_freeform)),
                (SHIFT, Left, Draw(place_line)),
                (CTRL, Left, Select(SelectDragCommand::NewRect.into())),
                (
                    CTRL | SHIFT,
                    Left,
                    Select(SelectDragCommand::ResizeToCell.into()),
                ),
                (NONE, Right, View(ViewDragCommand::Pan.into())),
                (CTRL, Right, View(ViewDragCommand::Scale.into())),
                (NONE, Middle, View(ViewDragCommand::Pan.into())),
            ]
            .into_iter()
            .collect(),
            drag_bindings_3d: vec![
                (NONE, Left, Draw(place_freeform)),
                (SHIFT, Left, Draw(erase_freeform)),
                (CTRL, Left, Select(SelectDragCommand::NewRect.into())),
                (
                    CTRL | SHIFT,
                    Left,
                    Select(SelectDragCommand::ResizeToCell.into()),
                ),
                (NONE, Right, View(ViewDragCommand::Orbit.into())),
                (CTRL, Right, View(ViewDragCommand::Scale.into())),
                (NONE, Middle, View(ViewDragCommand::Pan.into())),
                (SHIFT, Middle, View(ViewDragCommand::PanHorizontal.into())),
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
