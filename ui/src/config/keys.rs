use glium::glutin::event::{ModifiersState, MouseButton};

use crate::commands::{DrawDragAction, DrawMode, DrawShape, SelectDragAction, ViewDragAction};

#[derive(Debug)]
pub struct KeyConfig {
    pub mouse_drag_bindings_2d: MouseBindings<MouseDragBinding>,
    pub mouse_drag_bindings_3d: MouseBindings<MouseDragBinding>,
    // pub mouse_click_bindings_2d: MouseBindings<MouseClickBinding>,
    // pub mouse_click_bindings_3d: MouseBindings<MouseClickBinding>,
}
impl Default for KeyConfig {
    fn default() -> Self {
        const SHIFT: ModifiersState = ModifiersState::SHIFT;
        const CTRL: ModifiersState = ModifiersState::CTRL;
        const ALT: ModifiersState = ModifiersState::ALT;
        const NONE: ModifiersState = ModifiersState::empty();

        use MouseButton::{Left, Middle, Right};
        use MouseDragBinding::{Draw, Select, View};

        const FREEFORM_2D: DrawDragAction = DrawDragAction {
            mode: DrawMode::Replace,
            shape: DrawShape::Freeform,
        };
        const LINE_2D: DrawDragAction = DrawDragAction {
            mode: DrawMode::Replace,
            shape: DrawShape::Line,
        };

        Self {
            mouse_drag_bindings_2d: vec![
                (NONE, Left, Draw(FREEFORM_2D)),
                (SHIFT, Left, Draw(LINE_2D)),
                (CTRL, Left, Select(SelectDragAction::NewRect)),
                (NONE, Right, View(ViewDragAction::Pan)),
                (CTRL, Right, View(ViewDragAction::Scale)),
                (NONE, Middle, View(ViewDragAction::Pan)),
            ]
            .into_iter()
            .collect(),
            mouse_drag_bindings_3d: vec![
                (CTRL, Left, Select(SelectDragAction::NewRect)),
                (NONE, Right, View(ViewDragAction::Orbit)),
                (CTRL, Right, View(ViewDragAction::Scale)),
                (NONE, Middle, View(ViewDragAction::Pan)),
                (SHIFT, Middle, View(ViewDragAction::PanHorizontal)),
            ]
            .into_iter()
            .collect(),
            // mouse_click_bindings_2d: vec![].into_iter().collect(),
            // mouse_click_bindings_3d: vec![].into_iter().collect(),
        }
    }
}
impl KeyConfig {
    /// Returns a tuple `(click_binding, drag_binding)`.
    pub fn get_mouse_bindings(
        &self,
        ndim: usize,
        mods: ModifiersState,
        button: MouseButton,
    ) -> (Option<()>, Option<MouseDragBinding>) {
        match ndim {
            2 => (None, self.mouse_drag_bindings_2d.get(mods, button)),
            3 => (None, self.mouse_drag_bindings_3d.get(mods, button)),
            _ => (None, None),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MouseBindings<T>([Option<T>; 8 * 3]);
impl<T> Default for MouseBindings<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<T: Copy> MouseBindings<T> {
    fn index(&self, mods: ModifiersState, button: MouseButton) -> Option<usize> {
        if mods.logo() {
            None
        } else {
            let button_offset = match button {
                MouseButton::Left => 0,
                MouseButton::Right => 8,
                MouseButton::Middle => 16,
                _ => return None,
            };
            Some(
                button_offset
                    + ((mods.shift() as usize) << 1)
                    + ((mods.ctrl() as usize) << 2)
                    + ((mods.alt() as usize) << 3),
            )
        }
    }
    pub fn get(&self, mods: ModifiersState, button: MouseButton) -> Option<T> {
        let index = self.index(mods, button)?;
        *self.0.get(index)?
    }
    pub fn get_mut(&mut self, mods: ModifiersState, button: MouseButton) -> Option<&mut Option<T>> {
        let index = self.index(mods, button)?;
        self.0.get_mut(index)
    }
}
impl<T: Copy> std::iter::FromIterator<(ModifiersState, MouseButton, T)> for MouseBindings<T> {
    fn from_iter<I: IntoIterator<Item = (ModifiersState, MouseButton, T)>>(iter: I) -> Self {
        let mut ret = Self::default();
        for (mods, button, action) in iter {
            *ret.get_mut(mods, button)
                .expect("Error creating MouseBindings from iterator") = Some(action);
        }
        ret
    }
}

#[derive(Debug, Copy, Clone)]
pub enum MouseDragBinding {
    View(ViewDragAction),
    Draw(DrawDragAction),
    Select(SelectDragAction),
}
