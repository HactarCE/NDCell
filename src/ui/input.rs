use glium::glutin::*;
use log::warn;
use noisy_float::prelude::r64;
use num::Signed;
use std::collections::HashSet;
use std::ops::Index;

use super::gridview;
use crate::automaton::{AnyDimBigVec, NdSimulate, NdVec};

const FALSE_REF: &bool = &false;
const TRUE_REF: &bool = &true;

// OSX scancodes are from https://eastmanreference.com/complete-list-of-applescript-key-codes
#[cfg(any(target_os = "macos"))]
const SC_W: u32 = 13;
#[cfg(any(target_os = "macos"))]
const SC_A: u32 = 0;
#[cfg(any(target_os = "macos"))]
const SC_S: u32 = 1;
#[cfg(any(target_os = "macos"))]
const SC_D: u32 = 2;
#[cfg(any(target_os = "macos"))]
const SC_Q: u32 = 12;
#[cfg(any(target_os = "macos"))]
const SC_Z: u32 = 6;

#[cfg(not(any(target_os = "macos")))]
const SC_W: u32 = 17;
#[cfg(not(any(target_os = "macos")))]
const SC_A: u32 = 30;
#[cfg(not(any(target_os = "macos")))]
const SC_S: u32 = 31;
#[cfg(not(any(target_os = "macos")))]
const SC_D: u32 = 32;
#[cfg(not(any(target_os = "macos")))]
const SC_Q: u32 = 16;
#[cfg(not(any(target_os = "macos")))]
const SC_Z: u32 = 44;

/// A struct tracking miscellaneous stateful things relating input, such as
/// whether any given key is pressed.
#[derive(Default)]
pub struct InputState {
    scancodes: HashSet<u32>,
    virtual_keycodes: HashSet<VirtualKeyCode>,
    /// Whether the simulation is running.
    pub is_running: bool,
    /// Whether user input directed the viewport to move this frame. This is
    /// reset at the beginning of each frame.
    pub moving: bool,
    /// Whether user input directed the viewport to zoom in or out this frame.
    /// This is reset at the beginning of each frame.
    pub zooming: bool,
    /// Whether to ignore mouse inputs.
    pub ignore_mouse: bool,
    /// Whether to ignore keyboard inputs.
    pub ignore_keyboard: bool,
    /// The pixel position of the cursor.
    pub cursor_position: Option<(i32, i32)>,
    /// The cell state to draw with.
    pub draw_state: Option<u8>,
    /// The cell that the mouse is currently hovering over.
    pub hovered_cell: Option<AnyDimBigVec>,
}
impl Index<u32> for InputState {
    type Output = bool;
    fn index(&self, scancode: u32) -> &bool {
        &self[&scancode]
    }
}
impl Index<VirtualKeyCode> for InputState {
    type Output = bool;
    fn index(&self, virtual_keycode: VirtualKeyCode) -> &bool {
        &self[&virtual_keycode]
    }
}
impl Index<&u32> for InputState {
    type Output = bool;
    fn index(&self, scancode: &u32) -> &bool {
        if self.scancodes.contains(scancode) {
            TRUE_REF
        } else {
            FALSE_REF
        }
    }
}
impl Index<&VirtualKeyCode> for InputState {
    type Output = bool;
    fn index(&self, virtual_keycode: &VirtualKeyCode) -> &bool {
        if self.virtual_keycodes.contains(virtual_keycode) {
            TRUE_REF
        } else {
            FALSE_REF
        }
    }
}
impl InputState {
    /// Update internal key state based on a KeyboardInput event.
    pub fn update(&mut self, input: &KeyboardInput) {
        match input.state {
            ElementState::Pressed => {
                self.scancodes.insert(input.scancode);
                if let Some(virtual_keycode) = input.virtual_keycode {
                    self.virtual_keycodes.insert(virtual_keycode);
                }
            }
            ElementState::Released => {
                self.scancodes.remove(&input.scancode);
                if let Some(virtual_keycode) = input.virtual_keycode {
                    self.virtual_keycodes.remove(&virtual_keycode);
                }
            }
        }
    }
}

pub fn start_frame(state: &mut super::State) {
    state.input_state.moving = false;
    state.input_state.zooming = false;
}

pub fn handle_event(state: &mut super::State, ev: &Event) {
    if state.input_state.ignore_mouse {
        state.input_state.cursor_position = None;
    }
    match ev {
        // Handle WindowEvents.
        Event::WindowEvent { event, .. } => {
            match event {
                WindowEvent::KeyboardInput { input, .. } => {
                    state.input_state.update(input);
                    if !state.input_state.ignore_keyboard {
                        handle_key(state, input);
                    }
                }
                WindowEvent::CursorLeft { .. } => {
                    state.input_state.cursor_position = None;
                }
                WindowEvent::CursorMoved {
                    position: dpi::LogicalPosition { x, y },
                    ..
                } if !state.input_state.ignore_mouse => {
                    state.input_state.cursor_position =
                        Some(((*x * state.dpi) as i32, (*y * state.dpi) as i32));
                    if let Some(draw_state) = state.input_state.draw_state {
                        if let gridview::GridView::View2D(grid_view) = &mut state.grid_view {
                            if let Some(AnyDimBigVec::Vec2D(hover_pos)) =
                                &state.input_state.hovered_cell
                            {
                                grid_view.set_cell(hover_pos, draw_state);
                            }
                        }
                    }
                }
                WindowEvent::MouseWheel { delta, .. } if !state.input_state.ignore_mouse => {
                    // Pan 100x.
                    let (dx, dy) = match delta {
                        MouseScrollDelta::LineDelta(x, y) => (*x as f64, *y as f64),
                        MouseScrollDelta::PixelDelta(dpi::LogicalPosition { x, y }) => (*x, *y),
                    };
                    match &mut state.grid_view {
                        gridview::GridView::View2D(view2d) => {
                            view2d
                                .viewport
                                .pan_pixels(NdVec([r64(dx * 100.0), r64(dy * 100.0)]));
                        }
                        _ => (),
                    }
                }
                WindowEvent::MouseInput {
                    button: MouseButton::Left,
                    state: element_state,
                    ..
                } if !state.input_state.ignore_mouse => match element_state {
                    ElementState::Pressed => {
                        if let gridview::GridView::View2D(grid_view) = &mut state.grid_view {
                            if let Some(AnyDimBigVec::Vec2D(hover_pos)) =
                                &state.input_state.hovered_cell
                            {
                                if grid_view.get_cell(hover_pos) == 1 {
                                    state.input_state.draw_state = Some(0);
                                } else {
                                    state.input_state.draw_state = Some(1);
                                }
                                grid_view
                                    .set_cell(hover_pos, state.input_state.draw_state.unwrap());
                            }
                        }
                    }
                    ElementState::Released => {
                        state.input_state.draw_state = None;
                    }
                },
                // WindowEvent::MouseInput {

                // }
                _ => (),
            }
        }
        // Ignore non-WindowEvents.
        _ => (),
    }
}

pub fn handle_key(state: &mut super::State, input: &KeyboardInput) {
    match input {
        // Handle key press.
        KeyboardInput {
            state: ElementState::Pressed,
            virtual_keycode,
            modifiers,
            ..
        } => {
            match modifiers {
                // No modifiers
                ModifiersState {
                    shift: false,
                    ctrl: false,
                    alt: false,
                    logo: false,
                } => match virtual_keycode {
                    Some(VirtualKeyCode::Space) => state.step_no_cache_clear(&1.into(), true),
                    Some(VirtualKeyCode::Tab) => state.step_step_size(true),
                    Some(VirtualKeyCode::Return) => {
                        state.toggle_running();
                    }
                    _ => (),
                },

                // CTRL
                ModifiersState {
                    shift: false,
                    ctrl: true,
                    alt: false,
                    logo: false,
                } => match virtual_keycode {
                    // Undo.
                    Some(VirtualKeyCode::Z) => {
                        state.stop_running();
                        state.history.undo(&mut state.grid_view);
                    }
                    // Redo.
                    Some(VirtualKeyCode::Y) => {
                        state.stop_running();
                        state.history.redo(&mut state.grid_view);
                    }
                    // Reset.
                    Some(VirtualKeyCode::R) => {
                        state.stop_running();
                        while state.grid_view.get_generation_count().is_positive()
                            && state.history.undo(&mut state.grid_view)
                        {}
                    }
                    // Copy (Golly-compatible).
                    Some(VirtualKeyCode::C) => {
                        state.stop_running();
                        if state.copy_rle_to_clipboard().is_err() {
                            warn!("Failed to save RLE to clipboard");
                        }
                    }
                    // Paste.
                    Some(VirtualKeyCode::V) => {
                        state.stop_running();
                        if state.load_rle_from_clipboard().is_err() {
                            warn!("Failed to load RLE from clipboard");
                        };
                    }
                    _ => (),
                },
                // SHIFT + CTRL
                ModifiersState {
                    shift: true,
                    ctrl: true,
                    alt: false,
                    logo: false,
                } => match virtual_keycode {
                    // Redo.
                    Some(VirtualKeyCode::Z) => {
                        state.stop_running();
                        state.history.redo(&mut state.grid_view);
                    }
                    // Copy with extra info.
                    Some(VirtualKeyCode::C) => {
                        state.stop_running();
                        if state.copy_cxrle_to_clipboard().is_err() {
                            warn!("Failed to save CXRLE to clipboard");
                        }
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        // Ignore key release.
        _ => (),
    }
}

pub fn do_frame(state: &mut super::State) {
    if state.input_state.is_running {
        state.step_step_size(false);
    }

    let input_state = &mut state.input_state;
    let no_modifiers_pressed = !(input_state[VirtualKeyCode::LAlt]
        || input_state[VirtualKeyCode::LControl]
        || input_state[VirtualKeyCode::LWin]
        || input_state[VirtualKeyCode::RAlt]
        || input_state[VirtualKeyCode::RControl]
        || input_state[VirtualKeyCode::RWin]);
    let shift_pressed = input_state[VirtualKeyCode::LShift] || input_state[VirtualKeyCode::RShift];

    let speed = if shift_pressed { 2.0 } else { 1.0 };
    let move_speed = 25.0 * speed * state.dpi;
    let zoom_speed = 0.1 * speed;
    match &mut state.grid_view {
        gridview::GridView::View2D(view2d) => {
            if no_modifiers_pressed && !input_state.ignore_keyboard {
                let mut pan_x = 0.0;
                let mut pan_y = 0.0;
                // 'A' or left arrow => pan west.
                if input_state[SC_A] || input_state[VirtualKeyCode::Left] {
                    pan_x -= move_speed;
                }
                // 'D' or right arrow => pan west.
                if input_state[SC_D] || input_state[VirtualKeyCode::Right] {
                    pan_x += move_speed;
                }
                // 'W' or up arrow => pan north.
                if input_state[SC_W] || input_state[VirtualKeyCode::Up] {
                    pan_y += move_speed;
                }
                // 'S' or down arrow => pan down.
                if input_state[SC_S] || input_state[VirtualKeyCode::Down] {
                    pan_y -= move_speed;
                }
                if pan_x != 0.0 || pan_y != 0.0 {
                    view2d.viewport.pan_pixels(NdVec([r64(pan_x), r64(pan_y)]));
                    input_state.moving = true;
                }
                // 'Q' or page up => zoom in.
                if input_state[SC_Q] || input_state[VirtualKeyCode::PageUp] {
                    view2d.viewport.zoom_by(2.0f64.powf(zoom_speed));
                    input_state.zooming = true;
                }
                // 'Z' or page down => zoom out.
                if input_state[SC_Z] || input_state[VirtualKeyCode::PageDown] {
                    view2d.viewport.zoom_by(2.0f64.powf(-zoom_speed));
                    input_state.zooming = true;
                }
            }
            if !input_state.moving {
                // Snap to nearest position and zoom level.
                view2d.viewport.snap_pos();
            }
            if !input_state.zooming {
                view2d.viewport.zoom = view2d.viewport.zoom.round();
            }
        }
        gridview::GridView::View3D(_) => (),
    }
}
