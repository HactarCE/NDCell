use glium::glutin::dpi::{PhysicalPosition, PhysicalSize};
use glium::glutin::event::*;
use imgui_winit_support::WinitPlatform;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut, Index};
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use crate::config::Config;
use crate::gridview::commands::*;
use crate::gridview::{GridView, GridViewTrait, Scale};

const FALSE_REF: &bool = &false;
const TRUE_REF: &bool = &true;

const SHIFT: ModifiersState = ModifiersState::SHIFT;
const CTRL: ModifiersState = ModifiersState::CTRL;
const ALT: ModifiersState = ModifiersState::ALT;
const LOGO: ModifiersState = ModifiersState::LOGO;

/// Cooldown time between the user changing the scale factor and the scale
/// factor snapping to the nearest power-of-2 factor.
const SCALE_SNAP_COOLDOWN: Duration = Duration::from_millis(200);

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
    /// Set of pressed keys.
    keys: KeysPressed,
    /// Set of pressed modifiers.
    modifiers: ModifiersState,

    /// Whether the left mouse button is pressed.
    lmb_held: bool,
    /// Whether the right mouse button is pressed.
    rmb_held: bool,
    /// Whether the middle mouse button is pressed.
    mmb_held: bool,

    /// Current pixel position of the mouse cursor (relative to top left).
    cursor_pos: Option<FVec2D>,
    /// Pixel position of the mouse cursor on the last frame (relative to top left).
    last_cursor_pos: Option<FVec2D>,

    /// Cooldown until scale snap.
    scale_snap_cooldown: Option<Instant>,
    /// Cooldown until position snap.
    move_snap_cooldown: Option<Instant>,

    /// The cell state being used in the current drawing operation.
    draw_cell_state: Option<u8>,

    /// Window size.
    window_size: Option<PhysicalSize<u32>>,
    /// Window position.
    window_position: Option<PhysicalPosition<i32>>,
}
impl State {
    pub fn cursor_pos(&self) -> Option<FVec2D> {
        self.cursor_pos
    }
    pub fn mouse_buttton_held(&self) -> Option<MouseButton> {
        match (self.lmb_held, self.rmb_held, self.mmb_held) {
            (true, false, false) => Some(MouseButton::Left),
            (false, true, false) => Some(MouseButton::Right),
            (false, false, true) => Some(MouseButton::Middle),
            _ => None,
        }
    }
    pub fn frame<'a>(
        &'a mut self,
        config: &'a mut Config,
        gridview: &'a GridView,
        imgui_io: &imgui::Io,
        platform: &WinitPlatform,
    ) -> FrameInProgress<'a> {
        FrameInProgress {
            state: self,
            config,
            gridview,
            has_keyboard: !imgui_io.want_capture_keyboard,
            has_mouse: !imgui_io.want_capture_mouse,
            dpi: platform.hidpi_factor(),
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
    /// HiDPI factor.
    dpi: f64,
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
        self.gridview.enqueue(DrawCommand::Start);
        self.draw_cell_state = Some(draw_cell_state);
    }
    fn stop_drawing(&mut self) {
        self.gridview.enqueue(DrawCommand::End);
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
                    } => {
                        if self.has_mouse {
                            if let (Some(x), Some(y)) = (R64::try_new(*x), R64::try_new(*y)) {
                                self.state.cursor_pos = Some(NdVec([x, y]));
                            }
                        }
                    }
                    WindowEvent::MouseWheel { delta, .. } if self.has_mouse => {
                        let (_dx, dy) = match delta {
                            MouseScrollDelta::LineDelta(x, y) => (*x as f64, *y as f64),
                            MouseScrollDelta::PixelDelta(logical_position) => {
                                let PhysicalPosition { x, y } =
                                    logical_position.to_physical(self.dpi);
                                (x, y)
                            }
                        };
                        let speed = match (&self.gridview, delta) {
                            (GridView::View2D(_), MouseScrollDelta::LineDelta(_, _)) => {
                                self.config.ctrl.discrete_scale_speed_2d
                            }
                            (GridView::View2D(_), MouseScrollDelta::PixelDelta(_)) => {
                                self.config.ctrl.smooth_scroll_speed_2d
                            }
                            (GridView::View3D(_), MouseScrollDelta::LineDelta(_, _)) => {
                                self.config.ctrl.discrete_scale_speed_3d
                            }
                            (GridView::View3D(_), MouseScrollDelta::PixelDelta(_)) => {
                                self.config.ctrl.smooth_scroll_speed_3d
                            }
                        };
                        self.gridview.enqueue(MoveCommand::Scale {
                            log2_factor: dy * speed,
                            invariant_pos: self.cursor_pos,
                        });
                        self.scale_snap_cooldown = Some(Instant::now() + SCALE_SNAP_COOLDOWN);
                    }
                    WindowEvent::MouseInput {
                        button: MouseButton::Left,
                        state,
                        ..
                    } if self.has_mouse => match state {
                        ElementState::Pressed => {
                            match self.gridview {
                                GridView::View2D(view2d) => {
                                    if let Some(AnyDimVec::Vec2D(pos)) =
                                        view2d.nth_render_result(0).draw_pos.as_ref()
                                    {
                                        // Start drawing.
                                        let old_cell = view2d.get_cell(
                                            &view2d.cache().read_recursive(),
                                            &pos.floor().0,
                                        );
                                        self.start_drawing(if old_cell == 1 { 0 } else { 1 });
                                    }
                                }
                                _ => (),
                            }
                        }
                        ElementState::Released => {
                            self.stop_drawing();
                        }
                    },
                    WindowEvent::MouseInput { button, state, .. } if self.has_mouse => {
                        let pressed = *state == ElementState::Pressed;
                        match button {
                            MouseButton::Left => self.lmb_held = pressed,
                            MouseButton::Right => self.rmb_held = pressed,
                            MouseButton::Middle => self.mmb_held = pressed,
                            MouseButton::Other(_) => (),
                        };
                    }

                    // Ignore other `WindowEvent`s.
                    _ => (),
                }
            }

            // Ignore non-`WindowEvent`s.
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
                        Some(VirtualKeyCode::Equals) | Some(VirtualKeyCode::Add) => {
                            self.config.sim.step_size *= 2;
                        }
                        Some(VirtualKeyCode::Minus) | Some(VirtualKeyCode::Subtract) => {
                            self.config.sim.step_size /= 2;
                            if self.config.sim.step_size < 1.into() {
                                self.config.sim.step_size = 1.into();
                            }
                        }
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
                            self.gridview.enqueue(match self.gridview {
                                GridView::View2D(_) => MoveCommand::GoTo2D {
                                    x: Some(r64(0.0).into()),
                                    y: Some(r64(0.0).into()),
                                    relative: false,
                                    scaled: false,
                                },
                                GridView::View3D(_) => MoveCommand::GoTo3D {
                                    x: Some(r64(0.0).into()),
                                    y: Some(r64(0.0).into()),
                                    z: Some(r64(0.0).into()),
                                    yaw: Some(crate::gridview::Camera3D::DEFAULT_YAW.into()),
                                    pitch: Some(crate::gridview::Camera3D::DEFAULT_PITCH.into()),
                                    relative: false,
                                    scaled: false,
                                },
                            });
                            self.gridview
                                .enqueue(MoveCommand::GoToScale(Scale::default()));
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
                    if let Some(AnyDimVec::Vec2D(pos1)) =
                        view2d.nth_render_result(0).draw_pos.as_ref()
                    {
                        if let Some(AnyDimVec::Vec2D(pos2)) =
                            view2d.nth_render_result(1).draw_pos.as_ref()
                        {
                            view2d.enqueue(DrawCommand2D::Line(
                                pos1.floor().0,
                                pos2.floor().0,
                                draw_state,
                            ))
                        } else {
                            view2d.enqueue(DrawCommand2D::Cell(pos1.floor().0, draw_state));
                        }
                    } else {
                        self.stop_drawing();
                    }
                }
                _ => (),
            };
        }

        let mut moved = false;
        let mut scaled = false;

        if let (Some(start), Some(end)) = (self.state.last_cursor_pos, self.state.cursor_pos) {
            match self.state.mouse_buttton_held() {
                Some(MouseButton::Left) => {}
                Some(MouseButton::Right) => {
                    moved = true;
                    if self.gridview.is_2d() {
                        self.gridview.enqueue(MoveCommand::Pan { start, end });
                    } else if self.modifiers.shift() {
                        self.gridview.enqueue(MoveCommand::Pan { start, end });
                    } else {
                        self.gridview.enqueue(MoveCommand::Orbit { start, end });
                    };
                }
                Some(MouseButton::Middle) => {
                    if self.gridview.is_3d() {
                        self.gridview.enqueue(MoveCommand::Pan { start, end });
                    }
                }
                _ => {}
            }
        }

        let speed_per_second = if self.modifiers.shift() {
            self.config.ctrl.speed_modifier
        } else {
            1.0
        };
        let speed = speed_per_second / self.gridview.fps(&self.config);

        // 'A' or left arrow => pan west.
        let pan_left = self.keys[sc::A] || self.keys[VirtualKeyCode::Left];
        // 'D' or right arrow => pan east.
        let pan_right = self.keys[sc::D] || self.keys[VirtualKeyCode::Right];
        // 'W' or up arrow => pan north (2D) / forward (3D).
        let pan_north = self.keys[sc::W] || self.keys[VirtualKeyCode::Up];
        // 'S' or down arrow => pan south (2D) / backward (3D).
        let pan_south = self.keys[sc::S] || self.keys[VirtualKeyCode::Down];

        // 'Q' or page up => zoom in (2D).
        let zoom_in =
            self.gridview.is_2d() && (self.keys[sc::Q] || self.keys[VirtualKeyCode::PageUp]);
        // 'Z' or page down => zoom out (2D).
        let zoom_out =
            self.gridview.is_2d() && (self.keys[sc::Z] || self.keys[VirtualKeyCode::PageDown]);

        let move_left = pan_left;
        let move_right = pan_right;
        let move_fwd = pan_north;
        let move_back = pan_south;
        // 'Q' or page up => move up (3D).
        let move_up =
            self.gridview.is_3d() && (self.keys[sc::Q] || self.keys[VirtualKeyCode::PageUp]);
        // 'Z' or page down => move down (3D).
        let move_down =
            self.gridview.is_3d() && (self.keys[sc::Z] || self.keys[VirtualKeyCode::PageDown]);

        if self.has_keyboard && !self.modifiers.intersects(CTRL | ALT | LOGO) {
            match self.gridview {
                GridView::View2D(view2d) => {
                    let move_speed = self.config.ctrl.keybd_move_speed_2d * speed * self.dpi;
                    let pan_x = if pan_left { -move_speed } else { 0.0 }
                        + if pan_right { move_speed } else { 0.0 };
                    let pan_y = if pan_south { -move_speed } else { 0.0 }
                        + if pan_north { move_speed } else { 0.0 };
                    if (pan_x, pan_y) != (0.0, 0.0) {
                        view2d.enqueue(MoveCommand::GoTo2D {
                            x: Some(r64(pan_x).into()),
                            y: Some(r64(pan_y).into()),
                            relative: true,
                            scaled: true,
                        });
                        moved = true;
                    }

                    let scale_speed = self.config.ctrl.keybd_scale_speed_2d * speed;
                    let log2_factor = if zoom_in { scale_speed } else { 0.0 }
                        + if zoom_out { -scale_speed } else { 0.0 };
                    if log2_factor != 0.0 {
                        let invariant_pos = None;
                        view2d.enqueue(MoveCommand::Scale {
                            log2_factor,
                            invariant_pos,
                        });
                        scaled = true;
                    }
                }

                GridView::View3D(view3d) => {
                    let move_speed = self.config.ctrl.keybd_move_speed_3d * speed * self.dpi;
                    let x = if move_left { -move_speed } else { 0.0 }
                        + if move_right { move_speed } else { 0.0 };
                    let y = if move_down { -move_speed } else { 0.0 }
                        + if move_up { move_speed } else { 0.0 };
                    let z = if move_back { -move_speed } else { 0.0 }
                        + if move_fwd { move_speed } else { 0.0 };
                    if (x, y, z) != (0.0, 0.0, 0.0) {
                        view3d.enqueue(MoveCommand::GoTo3D {
                            x: Some(r64(x).into()),
                            y: Some(r64(y).into()),
                            z: Some(r64(z).into()),
                            yaw: None,
                            pitch: None,
                            relative: true,
                            scaled: true,
                        });
                        moved = true;
                    }
                }
            }
        }
        if !moved {
            // Snap to the nearest position.
            self.gridview.enqueue(MoveCommand::SnapPos);
        }
        if scaled {
            self.scale_snap_cooldown = Some(Instant::now() + Duration::from_millis(10));
        }
        if self.scale_snap_cooldown.is_none()
            || (Instant::now() >= self.scale_snap_cooldown.unwrap())
        {
            // Snap to the nearest power-of-2 scale.
            self.gridview.enqueue(MoveCommand::SnapScale {
                invariant_pos: None,
            });
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
