use glium::glutin::dpi::LogicalPosition;
use glium::glutin::*;
use log::warn;
use noisy_float::prelude::r64;
use std::collections::HashSet;
use std::ops::Index;
use std::time::{Duration, Instant};

use super::gridview::*;
use crate::automaton::{AsFVec, IVec2D, NdVec, X};
use crate::ui::{gridview_mut, History};

const FALSE_REF: &bool = &false;
const TRUE_REF: &bool = &true;

// Define keyboard scancodes. OSX scancodes are from
// https://eastmanreference.com/complete-list-of-applescript-key-codes
#[cfg(any(target_os = "macos"))]
mod sc {
    pub const W: u32 = 13;
    pub const A: u32 = 0;
    pub const S: u32 = 1;
    pub const D: u32 = 2;
    pub const Q: u32 = 12;
    pub const Z: u32 = 6;
}
#[cfg(not(any(target_os = "macos")))]
mod sc {
    pub const W: u32 = 17;
    pub const A: u32 = 30;
    pub const S: u32 = 31;
    pub const D: u32 = 32;
    pub const Q: u32 = 16;
    pub const Z: u32 = 44;
}

#[derive(Debug, Default)]
pub struct State {
    /// A record of which keys are pressed.
    keys: KeysPressed,
    /// Whether the right mouse button is held.
    rmb_held: bool,
    /// The pixel position of the mouse cursor from the top left of the window.
    cursor_pos: Option<IVec2D>,
    /// The cursor position on the last frame.
    last_cursor_pos: Option<IVec2D>,
    /// The next time that the zoom should snap to the nearest power of 2.
    time_to_snap_zoom: Option<Instant>,
    /// The cell state being used in the current drawing operation.
    draw_cell_state: Option<u8>,
}

impl State {
    pub fn get_cursor_pos(&self) -> Option<IVec2D> {
        self.cursor_pos
    }

    fn start_drawing(&mut self, draw_cell_state: u8) {
        self.draw_cell_state = Some(draw_cell_state);
    }
    fn stop_drawing(&mut self) {
        self.draw_cell_state = None;
    }

    pub fn handle_event(&mut self, ev: &Event, has_keyboard: bool, has_mouse: bool) {
        if !has_mouse {
            self.cursor_pos = None;
        }
        match ev {
            // Handle WindowEvents.
            Event::WindowEvent { event, .. } => {
                match event {
                    WindowEvent::KeyboardInput { input, .. } => {
                        self.keys.update(input);
                        if has_keyboard {
                            self.handle_key(input);
                        }
                    }
                    WindowEvent::CursorLeft { .. } => {
                        self.cursor_pos = None;
                    }
                    WindowEvent::CursorMoved {
                        position: LogicalPosition { x, y },
                        ..
                    } if has_mouse => {
                        let dpi = crate::ui::get_dpi();
                        let new_pos = NdVec([(*x * dpi) as isize, (*y * dpi) as isize]);

                        if let (true, Some(old_pos)) = (self.rmb_held, self.cursor_pos) {
                            let mut delta = new_pos - old_pos;
                            delta[X] = -delta[X]; // TODO: why invert X? explain.
                            match &mut *gridview_mut() {
                                GridView::View2D(view2d) => {
                                    // Pan both viewports so that the viewport stays matched with the cursor.
                                    view2d.viewport.pan_pixels(delta.as_fvec());
                                    view2d.interpolating_viewport.pan_pixels(delta.as_fvec());
                                }
                                _ => (),
                            }
                            // TODO: implement velocity when letting go
                            // TODO: zoom in/out relative to cursor position when holding RMB
                        }
                        self.cursor_pos = Some(new_pos);
                    }
                    WindowEvent::MouseWheel { delta, .. } if has_mouse => {
                        let (_dx, dy) = match delta {
                            MouseScrollDelta::LineDelta(x, y) => (*x as f64, *y as f64),
                            MouseScrollDelta::PixelDelta(LogicalPosition { x, y }) => (*x, *y),
                        };
                        match &mut *gridview_mut() {
                            GridView::View2D(view2d) => {
                                view2d.viewport.zoom_by(2.0f64.powf(dy));
                                // TODO magic numbers ick
                                self.time_to_snap_zoom =
                                    Some(Instant::now() + Duration::from_millis(200));
                            }
                            _ => (),
                        }
                    }
                    WindowEvent::MouseInput {
                        button: MouseButton::Left,
                        state,
                        ..
                    } if has_mouse => match state {
                        ElementState::Pressed => {
                            let mut gridview = gridview_mut();
                            match &*gridview {
                                GridView::View2D(view2d) => {
                                    let pos = &view2d.get_render_result().hover_pos.clone();
                                    if let Some(draw_pos) = &pos {
                                        // Start drawing.
                                        self.start_drawing(if view2d.get_cell(draw_pos) == 1 {
                                            0
                                        } else {
                                            1
                                        });
                                    }
                                }
                                _ => (),
                            }
                            if self.draw_cell_state.is_some() {
                                // Save to undo history before starting to draw.
                                gridview.record()
                            }
                        }
                        ElementState::Released => {
                            self.stop_drawing();
                        }
                    },
                    WindowEvent::MouseInput {
                        button: MouseButton::Right,
                        state,
                        ..
                    } if has_mouse => {
                        self.rmb_held = *state == ElementState::Pressed;
                    }
                    _ => (),
                }
            }
            // Ignore non-WindowEvents.
            _ => (),
        }
    }

    fn handle_key(&mut self, input: &KeyboardInput) {
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
                        Some(VirtualKeyCode::Space) => {
                            let mut gridview = gridview_mut();
                            if !gridview.stop_running() {
                                gridview.step_n(&1.into())
                            }
                        }
                        Some(VirtualKeyCode::Tab) => {
                            let mut gridview = gridview_mut();
                            if !gridview.stop_running() {
                                gridview_mut().step()
                            }
                        }
                        Some(VirtualKeyCode::Return) => {
                            gridview_mut().toggle_running();
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
                            gridview_mut().undo();
                        }
                        // Redo.
                        Some(VirtualKeyCode::Y) => {
                            gridview_mut().redo();
                        }
                        // Reset.
                        Some(VirtualKeyCode::R) => {
                            gridview_mut().undo_to_gen(&0.into());
                        }
                        // Copy (Golly-compatible).
                        Some(VirtualKeyCode::C) => {
                            if crate::ui::copy_rle_to_clipboard().is_err() {
                                warn!("Failed to save RLE to clipboard");
                            }
                        }
                        // Paste.
                        Some(VirtualKeyCode::V) => {
                            if crate::ui::load_rle_from_clipboard().is_err() {
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
                            gridview_mut().redo();
                        }
                        // Copy with extra info.
                        Some(VirtualKeyCode::C) => {
                            if crate::ui::copy_cxrle_to_clipboard().is_err() {
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

    pub fn do_frame(&mut self, has_keyboard: bool, _has_mouse: bool) {
        let no_modifiers_pressed = !(self.keys[VirtualKeyCode::LAlt]
            || self.keys[VirtualKeyCode::LControl]
            || self.keys[VirtualKeyCode::LWin]
            || self.keys[VirtualKeyCode::RAlt]
            || self.keys[VirtualKeyCode::RControl]
            || self.keys[VirtualKeyCode::RWin]);
        let shift_pressed = self.keys[VirtualKeyCode::LShift] || self.keys[VirtualKeyCode::RShift];

        if let Some(draw_state) = self.draw_cell_state {
            let mut gridview = gridview_mut();
            match &mut *gridview {
                GridView::View2D(view2d) => {
                    if let Some(pos1) = view2d.get_render_result().hover_pos.clone() {
                        if let Some(pos2) = view2d.get_prior_render_result().hover_pos.clone() {
                            for draw_pos in crate::math::bresenham(pos1, pos2) {
                                view2d.set_cell(&draw_pos, draw_state);
                            }
                        } else {
                            view2d.set_cell(&pos1, draw_state);
                        }
                    }
                }
                _ => (),
            };
        }

        let mut moved = false;
        let mut zoomed = false;

        let speed = if shift_pressed { 2.0 } else { 1.0 };
        let move_speed = 25.0 * speed * crate::ui::get_dpi();
        let zoom_speed = 0.1 * speed;
        match &mut *gridview_mut() {
            GridView::View2D(view2d) => {
                if has_keyboard && no_modifiers_pressed {
                    let mut pan_x = 0.0;
                    let mut pan_y = 0.0;
                    // 'A' or left arrow => pan west.
                    if self.keys[sc::A] || self.keys[VirtualKeyCode::Left] {
                        pan_x -= move_speed;
                    }
                    // 'D' or right arrow => pan west.
                    if self.keys[sc::D] || self.keys[VirtualKeyCode::Right] {
                        pan_x += move_speed;
                    }
                    // 'W' or up arrow => pan north.
                    if self.keys[sc::W] || self.keys[VirtualKeyCode::Up] {
                        pan_y += move_speed;
                    }
                    // 'S' or down arrow => pan down.
                    if self.keys[sc::S] || self.keys[VirtualKeyCode::Down] {
                        pan_y -= move_speed;
                    }
                    if pan_x != 0.0 || pan_y != 0.0 {
                        view2d.viewport.pan_pixels(NdVec([r64(pan_x), r64(pan_y)]));
                        moved = true;
                    }
                    // 'Q' or page up => zoom in.
                    if self.keys[sc::Q] || self.keys[VirtualKeyCode::PageUp] {
                        view2d.viewport.zoom_by(2.0f64.powf(zoom_speed));
                        zoomed = true;
                    }
                    // 'Z' or page down => zoom out.
                    if self.keys[sc::Z] || self.keys[VirtualKeyCode::PageDown] {
                        view2d.viewport.zoom_by(2.0f64.powf(-zoom_speed));
                        zoomed = true;
                    }
                }
                if !moved && !self.rmb_held {
                    // Snap to the nearest position.
                    view2d.viewport.snap_pos();
                }
                if zoomed {
                    self.time_to_snap_zoom = Some(Instant::now() + Duration::from_millis(10));
                }
                if self.time_to_snap_zoom.is_none()
                    || (Instant::now() >= self.time_to_snap_zoom.unwrap())
                {
                    // Snap to the nearest zoom level.
                    view2d.viewport.zoom = view2d.viewport.zoom.round();
                }
            }
            GridView::View3D(_) => (),
        }
        self.last_cursor_pos = self.cursor_pos;
    }
}

// TODO: document this
#[derive(Debug, Default)]
struct KeysPressed {
    /// The set of scancodes for keys that are held.
    scancodes: HashSet<u32>,
    /// The set of virtual keycodes for keys that are held.
    virtual_keycodes: HashSet<VirtualKeyCode>,
}
impl KeysPressed {
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
impl Index<u32> for KeysPressed {
    type Output = bool;
    fn index(&self, scancode: u32) -> &bool {
        if self.scancodes.contains(&scancode) {
            TRUE_REF
        } else {
            FALSE_REF
        }
    }
}
impl Index<VirtualKeyCode> for KeysPressed {
    type Output = bool;
    fn index(&self, virtual_keycode: VirtualKeyCode) -> &bool {
        if self.virtual_keycodes.contains(&virtual_keycode) {
            TRUE_REF
        } else {
            FALSE_REF
        }
    }
}
