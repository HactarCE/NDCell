use glium::glutin::dpi::PhysicalPosition;
use glium::glutin::event::*;
use noisy_float::prelude::r64;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut, Index};
use std::time::{Duration, Instant};

use ndcell_core::{FVec2D, IVec2D, NdVec, X, Y};

use crate::config::Config;
use crate::gridview::{control::*, GridView, GridViewTrait, RenderGridView, Zoom2D};

const FALSE_REF: &bool = &false;
const TRUE_REF: &bool = &true;

const SHIFT: ModifiersState = ModifiersState::SHIFT;
const CTRL: ModifiersState = ModifiersState::CTRL;
const ALT: ModifiersState = ModifiersState::ALT;
const LOGO: ModifiersState = ModifiersState::LOGO;

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
    /// A record of which modifiers are pressed.
    modifiers: ModifiersState,
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
    pub fn cursor_pos(&self) -> Option<IVec2D> {
        self.cursor_pos
    }
    pub fn frame<'a>(
        &'a mut self,
        config: &'a mut Config,
        gridview: &'a GridView,
        imgui_io: &imgui::Io,
    ) -> FrameInProgress<'a> {
        FrameInProgress {
            state: self,
            config,
            gridview,
            has_keyboard: !imgui_io.want_capture_keyboard,
            has_mouse: !imgui_io.want_capture_mouse,
        }
    }
}

// TODO: document this
#[must_use = "call finish()"]
pub struct FrameInProgress<'a> {
    state: &'a mut State,
    config: &'a mut Config,
    gridview: &'a GridView,
    /// Whether to handle keyboard input (false if it is captured by imgui).
    has_keyboard: bool,
    /// Whether to handle mouse input (false if it is captured by imgui).
    has_mouse: bool,
}
impl<'a> Deref for FrameInProgress<'a> {
    type Target = State;
    fn deref(&self) -> &State {
        &self.state
    }
}
impl<'a> DerefMut for FrameInProgress<'a> {
    fn deref_mut(&mut self) -> &mut State {
        &mut self.state
    }
}
impl<'a> FrameInProgress<'a> {
    fn start_drawing(&mut self, draw_cell_state: u8) {
        self.gridview.enqueue(Command::StartDraw);
        self.draw_cell_state = Some(draw_cell_state);
    }
    fn stop_drawing(&mut self) {
        self.gridview.enqueue(Command::EndDraw);
        self.draw_cell_state = None;
    }

    pub fn handle_event(&mut self, ev: &Event<()>) {
        if !self.has_mouse {
            self.cursor_pos = None;
        }
        match ev {
            // Handle WindowEvents.
            Event::WindowEvent { event, .. } => {
                match event {
                    WindowEvent::KeyboardInput { input, .. } => {
                        self.keys.update(input);
                        if self.has_keyboard {
                            self.handle_key(input);
                        }
                    }
                    WindowEvent::ModifiersChanged(new_modifiers) => {
                        self.modifiers = *new_modifiers;
                    }
                    WindowEvent::CursorLeft { .. } => {
                        self.cursor_pos = None;
                    }
                    WindowEvent::CursorMoved {
                        position: PhysicalPosition { x, y },
                        ..
                    } if self.has_mouse => {
                        let new_pos = NdVec([x.round() as isize, y.round() as isize]);

                        if let (true, Some(old_pos)) = (self.rmb_held, self.cursor_pos) {
                            let mut delta = new_pos - old_pos;
                            delta[X] = -delta[X]; // TODO: why invert X? explain.
                            match self.gridview {
                                GridView::View2D(view2d) => {
                                    // Pan both viewports so that the viewport stays matched with the cursor.
                                    view2d.enqueue(
                                        MoveCommand2D::PanPixels(delta.to_fvec()).direct(),
                                    );
                                }
                                _ => (),
                            }
                            // TODO: implement velocity when letting go
                            // TODO: zoom in/out relative to cursor position when holding RMB
                        }
                        self.cursor_pos = Some(new_pos);
                    }
                    WindowEvent::MouseWheel { delta, .. } if self.has_mouse => {
                        let (_dx, dy) = match delta {
                            MouseScrollDelta::LineDelta(x, y) => (*x as f64, *y as f64),
                            MouseScrollDelta::PixelDelta(logical_position) => {
                                let PhysicalPosition { x, y } =
                                    logical_position.to_physical(self.config.gfx.dpi);
                                (x, y)
                            }
                        };
                        match self.gridview {
                            GridView::View2D(view2d) => {
                                view2d.enqueue(
                                    MoveCommand2D::Zoom {
                                        power: dy,
                                        fixed_point: view2d.nth_render_result(0).hover_pos.clone(),
                                    }
                                    .decay(),
                                );
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
                    } if self.has_mouse => match state {
                        ElementState::Pressed => {
                            match self.gridview {
                                GridView::View2D(view2d) => {
                                    if let Some(pos) = view2d.nth_render_result(0).draw_pos.as_ref()
                                    {
                                        // Start drawing.
                                        self.start_drawing(if view2d.get_cell(&pos.int) == 1 {
                                            0
                                        } else {
                                            1
                                        });
                                    }
                                }
                                _ => (),
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
                    } if self.has_mouse => {
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
                ..
            } => {
                // We don't care about left vs. right modifiers, so just extract
                // the bits that don't specify left vs. right.
                let modifiers = self.state.modifiers & (SHIFT | CTRL | ALT | LOGO);

                if modifiers.is_empty() {
                    match virtual_keycode {
                        Some(VirtualKeyCode::Space) => {
                            self.gridview.enqueue(SimCommand::Step(1.into()))
                        }
                        Some(VirtualKeyCode::Tab) => {
                            self.gridview.enqueue(SimCommand::StepStepSize)
                        }
                        Some(VirtualKeyCode::Return) => {
                            self.gridview.enqueue(SimCommand::ToggleRunning)
                        }
                        Some(VirtualKeyCode::Escape) => self.gridview.enqueue(SimCommand::Cancel),
                        _ => (),
                    }
                }

                if modifiers == CTRL {
                    match virtual_keycode {
                        // Undo.
                        Some(VirtualKeyCode::Z) => self.gridview.enqueue(HistoryCommand::Undo),
                        // Redo.
                        Some(VirtualKeyCode::Y) => self.gridview.enqueue(HistoryCommand::Redo),
                        // Reset.
                        Some(VirtualKeyCode::R) => {
                            self.gridview.enqueue(HistoryCommand::UndoTo(0.into()))
                        }
                        // Copy (Golly-compatible).
                        Some(VirtualKeyCode::C) => self.gridview.enqueue(ClipboardCommand::CopyRle),
                        // Paste.
                        Some(VirtualKeyCode::V) => self.gridview.enqueue(ClipboardCommand::Paste),
                        // Center pattern.
                        Some(VirtualKeyCode::M) => {
                            self.gridview
                                .enqueue(MoveCommand2D::SetPos(FVec2D::origin().into()).decay());
                            self.gridview.enqueue(
                                MoveCommand2D::SetZoom {
                                    zoom: Zoom2D::default(),
                                    fixed_point: None,
                                }
                                .decay(),
                            );
                        }
                        _ => (),
                    }
                }

                if modifiers == SHIFT | CTRL {
                    match virtual_keycode {
                        // Redo.
                        Some(VirtualKeyCode::Z) => self.gridview.enqueue(HistoryCommand::Redo),
                        // Copy with extra info.
                        Some(VirtualKeyCode::C) => {
                            self.gridview.enqueue(ClipboardCommand::CopyCxrle)
                        }
                        _ => (),
                    }
                }
            }
            // Ignore key release.
            _ => (),
        }
    }

    pub fn finish(mut self) {
        if let Some(draw_state) = self.draw_cell_state {
            match self.gridview {
                GridView::View2D(view2d) => {
                    if let Some(pos1) = view2d.nth_render_result(0).draw_pos.clone() {
                        if let Some(pos2) = view2d.nth_render_result(1).draw_pos.clone() {
                            view2d.enqueue(DrawCommand2D::Line(pos1.int, pos2.int, draw_state))
                        } else {
                            view2d.enqueue(DrawCommand2D::Cell(pos1.int, draw_state));
                        }
                    } else {
                        self.stop_drawing();
                    }
                }
                _ => (),
            };
        }

        let mut moved = false;
        let mut zoomed = false;

        let speed_per_second = if self.modifiers.shift() {
            self.config.ctrl.base_speed_2
        } else {
            self.config.ctrl.base_speed_1
        };
        let speed = speed_per_second / self.gridview.fps(&self.config);
        let move_speed = r64(self.config.ctrl.move_speed * speed * self.config.gfx.dpi);
        let zoom_speed = self.config.ctrl.zoom_speed * speed;
        match self.gridview {
            GridView::View2D(view2d) => {
                if self.has_keyboard && !self.modifiers.intersects(CTRL | ALT | LOGO) {
                    let mut pan = FVec2D::origin();
                    // 'A' or left arrow => pan west.
                    if self.keys[sc::A] || self.keys[VirtualKeyCode::Left] {
                        pan[X] -= move_speed;
                    }
                    // 'D' or right arrow => pan west.
                    if self.keys[sc::D] || self.keys[VirtualKeyCode::Right] {
                        pan[X] += move_speed;
                    }
                    // 'W' or up arrow => pan north.
                    if self.keys[sc::W] || self.keys[VirtualKeyCode::Up] {
                        pan[Y] += move_speed;
                    }
                    // 'S' or down arrow => pan down.
                    if self.keys[sc::S] || self.keys[VirtualKeyCode::Down] {
                        pan[Y] -= move_speed;
                    }
                    if !pan.is_zero() {
                        view2d.enqueue(MoveCommand2D::PanPixels(pan).decay());
                        moved = true;
                    }
                    // 'Q' or page up => zoom in.
                    if self.keys[sc::Q] || self.keys[VirtualKeyCode::PageUp] {
                        view2d.enqueue(
                            MoveCommand2D::Zoom {
                                power: zoom_speed,
                                fixed_point: None,
                            }
                            .decay(),
                        );
                        zoomed = true;
                    }
                    // 'Z' or page down => zoom out.
                    if self.keys[sc::Z] || self.keys[VirtualKeyCode::PageDown] {
                        view2d.enqueue(
                            MoveCommand2D::Zoom {
                                power: -zoom_speed,
                                fixed_point: None,
                            }
                            .decay(),
                        );
                        zoomed = true;
                    }
                }
                if !moved && !self.rmb_held {
                    // Snap to the nearest position.
                    view2d.enqueue(MoveCommand2D::SnapPos.decay());
                }
                if zoomed {
                    self.time_to_snap_zoom = Some(Instant::now() + Duration::from_millis(10));
                }
                if self.time_to_snap_zoom.is_none()
                    || (Instant::now() >= self.time_to_snap_zoom.unwrap())
                {
                    // Snap to the nearest zoom level.
                    view2d.enqueue(MoveCommand2D::SnapZoom(None).decay());
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