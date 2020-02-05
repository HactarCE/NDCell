use glium::glutin::*;
use log::warn;
use noisy_float::prelude::r64;
use ref_thread_local::RefThreadLocal;
use std::collections::HashSet;
use std::ops::Index;

use super::gridview::*;
use crate::automaton::{IVec2D, NdVec};
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

ref_thread_local! {
    /// A record of which keys are pressed.
    static managed KEYS: KeysPressed = KeysPressed::default();
    /// The pixel position of the cursor from the top left of the window.
    static managed CURSOR_POS: Option<IVec2D> = None;
    /// The cell state being used in the current drawing operation.
    static managed DRAW_CELL_STATE: Option<u8> = None;
}

pub fn get_cursor_pos() -> Option<IVec2D> {
    *CURSOR_POS.borrow()
}

fn get_draw_cell_state() -> Option<u8> {
    *DRAW_CELL_STATE.borrow()
}
fn start_drawing(new_state: u8) {
    *DRAW_CELL_STATE.borrow_mut() = Some(new_state);
}
fn stop_drawing() {
    *DRAW_CELL_STATE.borrow_mut() = None;
}

// TODO: document this
#[derive(Default)]
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

pub fn handle_event(ev: &Event, has_keyboard: bool, has_mouse: bool) {
    let mut cursor_pos = CURSOR_POS.borrow_mut();
    if !has_mouse {
        *cursor_pos = None;
    }
    match ev {
        // Handle WindowEvents.
        Event::WindowEvent { event, .. } => {
            match event {
                WindowEvent::KeyboardInput { input, .. } => {
                    KEYS.borrow_mut().update(input);
                    if has_keyboard {
                        handle_key(input);
                    }
                }
                WindowEvent::CursorLeft { .. } => {
                    *cursor_pos = None;
                }
                WindowEvent::CursorMoved {
                    position: dpi::LogicalPosition { x, y },
                    ..
                } if has_mouse => {
                    let dpi = crate::ui::get_dpi();
                    *cursor_pos = Some(NdVec([(*x * dpi) as isize, (*y * dpi) as isize]));
                }
                WindowEvent::MouseWheel { delta, .. } if has_mouse => {
                    // Pan 100x.
                    let (dx, dy) = match delta {
                        MouseScrollDelta::LineDelta(x, y) => (*x as f64, *y as f64),
                        MouseScrollDelta::PixelDelta(dpi::LogicalPosition { x, y }) => (*x, *y),
                    };
                    // TODO: zoom on mousewheel instead of pan
                    match &mut *gridview_mut() {
                        GridView::View2D(view2d) => {
                            view2d
                                .viewport
                                .pan_pixels(NdVec([r64(dx * 100.0), r64(dy * 100.0)]));
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
                                let pos = view2d.get_hover_pos().clone();
                                if let Some(draw_pos) = pos {
                                    // Start drawing.
                                    start_drawing(if view2d.get_cell(draw_pos) == 1 {
                                        0
                                    } else {
                                        1
                                    });
                                }
                            }
                            _ => (),
                        }
                        if get_draw_cell_state().is_some() {
                            // Save to undo history before starting to draw.
                            gridview.record()
                        }
                    }
                    ElementState::Released => {
                        stop_drawing();
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

fn handle_key(input: &KeyboardInput) {
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
                    Some(VirtualKeyCode::Space) => gridview_mut().step_n(&1.into()),
                    Some(VirtualKeyCode::Tab) => gridview_mut().step(),
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

pub fn do_frame(has_keyboard: bool, _has_mouse: bool) {
    let keys = KEYS.borrow();
    let no_modifiers_pressed = !(keys[VirtualKeyCode::LAlt]
        || keys[VirtualKeyCode::LControl]
        || keys[VirtualKeyCode::LWin]
        || keys[VirtualKeyCode::RAlt]
        || keys[VirtualKeyCode::RControl]
        || keys[VirtualKeyCode::RWin]);
    let shift_pressed = keys[VirtualKeyCode::LShift] || keys[VirtualKeyCode::RShift];

    if let Some(draw_state) = get_draw_cell_state() {
        let mut gridview = gridview_mut();
        match &mut *gridview {
            GridView::View2D(view2d) => {
                let pos = view2d.get_hover_pos().map(NdVec::clone);
                if let Some(draw_pos) = pos {
                    view2d.set_cell(&draw_pos, draw_state);
                }
            }
            _ => (),
        };
    }

    let mut moved = false;
    let mut zoomed = false;

    let speed = if shift_pressed { 2.0 } else { 1.0 };
    let move_speed = 25.0 * speed * *crate::ui::DPI.borrow();
    let zoom_speed = 0.1 * speed;
    match &mut *gridview_mut() {
        GridView::View2D(view2d) => {
            if has_keyboard && no_modifiers_pressed {
                let mut pan_x = 0.0;
                let mut pan_y = 0.0;
                // 'A' or left arrow => pan west.
                if keys[sc::A] || keys[VirtualKeyCode::Left] {
                    pan_x -= move_speed;
                }
                // 'D' or right arrow => pan west.
                if keys[sc::D] || keys[VirtualKeyCode::Right] {
                    pan_x += move_speed;
                }
                // 'W' or up arrow => pan north.
                if keys[sc::W] || keys[VirtualKeyCode::Up] {
                    pan_y += move_speed;
                }
                // 'S' or down arrow => pan down.
                if keys[sc::S] || keys[VirtualKeyCode::Down] {
                    pan_y -= move_speed;
                }
                if pan_x != 0.0 || pan_y != 0.0 {
                    view2d.viewport.pan_pixels(NdVec([r64(pan_x), r64(pan_y)]));
                    moved = true;
                }
                // 'Q' or page up => zoom in.
                if keys[sc::Q] || keys[VirtualKeyCode::PageUp] {
                    view2d.viewport.zoom_by(2.0f64.powf(zoom_speed));
                    zoomed = true;
                }
                // 'Z' or page down => zoom out.
                if keys[sc::Z] || keys[VirtualKeyCode::PageDown] {
                    view2d.viewport.zoom_by(2.0f64.powf(-zoom_speed));
                    zoomed = true;
                }
            }
            if !moved {
                // Snap to nearest position.
                view2d.viewport.snap_pos();
            }
            if !zoomed {
                // Snap to the nearest zoom level.
                view2d.viewport.zoom = view2d.viewport.zoom.round();
            }
        }
        GridView::View3D(_) => (),
    }
}
