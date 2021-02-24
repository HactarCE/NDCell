use glium::glutin::dpi::{PhysicalPosition, PhysicalSize};
use glium::glutin::event::*;
use imgui_winit_support::WinitPlatform;
use std::collections::HashSet;
use std::ops::Index;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use crate::commands::*;
use crate::gridview::GridView;
use crate::mouse::{MouseDisplayMode, MouseState};
use crate::CONFIG;

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
    pub const E: u32 = 14;
    pub const Z: u32 = 6;
}
#[cfg(not(any(target_os = "macos")))]
mod sc {
    pub const W: u32 = 17;
    pub const A: u32 = 30;
    pub const S: u32 = 31;
    pub const D: u32 = 32;
    pub const Q: u32 = 16;
    pub const E: u32 = 18;
    pub const Z: u32 = 44;
}

#[derive(Debug, Default)]
pub struct State {
    /// Set of pressed keys.
    keys: KeysPressed,
    /// Set of pressed modifiers.
    modifiers: ModifiersState,

    /// Mouse button being pressed and dragged, if any.
    dragging_button: Option<MouseButton>,

    /// State of the mouse cursor.
    mouse: MouseState,

    /// Cooldown until scale snap.
    scale_snap_cooldown: Option<Instant>,
    /// Cooldown until position snap.
    move_snap_cooldown: Option<Instant>,

    /// Window size.
    window_size: Option<PhysicalSize<u32>>,
    /// Window position.
    window_position: Option<PhysicalPosition<i32>>,
}
impl State {
    pub fn modifiers(&self) -> ModifiersState {
        self.modifiers
    }
    pub fn mouse(&self) -> MouseState {
        self.mouse
    }

    pub fn frame<'a>(
        &'a mut self,
        gridview: &'a GridView,
        imgui_io: &imgui::Io,
        platform: &WinitPlatform,
    ) -> FrameInProgress<'a> {
        let has_keyboard = !imgui_io.want_capture_keyboard;
        let has_mouse = !imgui_io.want_capture_mouse;
        if !has_mouse {
            self.mouse.pos = None;
        }
        FrameInProgress {
            state: self,
            gridview,

            has_keyboard,
            has_mouse,
            dpi: platform.hidpi_factor(),
        }
    }
}

// TODO: document this
#[must_use = "call finish()"]
pub struct FrameInProgress<'a> {
    state: &'a mut State,
    gridview: &'a GridView,

    /// Whether to handle keyboard input (false if it is captured by imgui).
    has_keyboard: bool,
    /// Whether to handle mouse input (false if it is captured by imgui).
    has_mouse: bool,
    /// HiDPI factor.
    dpi: f64,
}
impl FrameInProgress<'_> {
    pub fn handle_event(&mut self, ev: &Event<'_, ()>) {
        match ev {
            // Handle WindowEvents.
            Event::WindowEvent { event, .. } => {
                match event {
                    WindowEvent::KeyboardInput { input, .. } => {
                        self.state.keys.update(input);
                        if self.has_keyboard {
                            self.handle_key(input);
                        }
                    }
                    WindowEvent::ModifiersChanged(new_modifiers) => {
                        self.state.modifiers = *new_modifiers;
                    }
                    WindowEvent::CursorLeft { .. } => {
                        self.state.mouse.pos = None;
                    }
                    WindowEvent::CursorMoved {
                        position: PhysicalPosition { x, y },
                        ..
                    } => {
                        self.state.mouse.pos = None;
                        if self.has_mouse {
                            if let (Some(x), Some(y)) = (R64::try_new(*x), R64::try_new(*y)) {
                                self.state.mouse.pos = Some(NdVec([x, y]));
                            }
                        }
                    }
                    WindowEvent::MouseWheel { delta, .. } if self.has_mouse => {
                        let (_dx, dy) = match *delta {
                            MouseScrollDelta::LineDelta(x, y) => (x as f64, y as f64),
                            MouseScrollDelta::PixelDelta(PhysicalPosition { x, y }) => (x, y),
                        };
                        let config = CONFIG.lock();
                        let speed = match (&self.gridview, delta) {
                            (GridView::View2D(_), MouseScrollDelta::LineDelta(_, _)) => {
                                config.ctrl.discrete_scale_speed_2d
                            }
                            (GridView::View2D(_), MouseScrollDelta::PixelDelta(_)) => {
                                1.0 / config.ctrl.pixels_per_2x_scale_2d
                            }
                            (GridView::View3D(_), MouseScrollDelta::LineDelta(_, _)) => {
                                config.ctrl.discrete_scale_speed_3d
                            }
                            (GridView::View3D(_), MouseScrollDelta::PixelDelta(_)) => {
                                1.0 / config.ctrl.pixels_per_2x_scale_3d
                            }
                        };
                        let log2_scale_factor = dy * speed;
                        if let Some(mouse_pos) = self.state.mouse.pos {
                            self.gridview
                                .enqueue(Cmd::ScaleToCursor(log2_scale_factor).at(mouse_pos));
                        } else {
                            self.gridview.enqueue(Cmd::Scale(log2_scale_factor));
                        }
                        self.state.scale_snap_cooldown = Some(Instant::now() + SCALE_SNAP_COOLDOWN);
                    }
                    WindowEvent::MouseInput { button, state, .. } => match state {
                        ElementState::Pressed => self.handle_mouse_press(*button),
                        ElementState::Released => self.handle_mouse_release(*button),
                    },

                    // Ignore other `WindowEvent`s.
                    _ => (),
                }
            }

            // Ignore non-`WindowEvent`s.
            _ => (),
        }
    }

    fn handle_key(&mut self, input: &KeyboardInput) {
        let mut config = CONFIG.lock();

        match input {
            // Handle key press.
            KeyboardInput {
                state: ElementState::Pressed,
                virtual_keycode,
                scancode,
                ..
            } => {
                // We don't care about left vs. right modifiers, so just extract
                // the bits that don't specify left vs. right.
                let modifiers = self.state.modifiers & (SHIFT | CTRL | ALT | LOGO);

                if modifiers.is_empty() {
                    match virtual_keycode {
                        Some(VirtualKeyCode::Space) => self.gridview.enqueue(Cmd::Step(1)),
                        Some(VirtualKeyCode::Tab) => self.gridview.enqueue(Cmd::StepStepSize),
                        Some(VirtualKeyCode::Return) => self.gridview.enqueue(Cmd::ToggleRunning),
                        Some(VirtualKeyCode::Escape) => self.gridview.enqueue(Cmd::Cancel),
                        Some(VirtualKeyCode::Equals) | Some(VirtualKeyCode::NumpadAdd) => {
                            config.sim.step_size *= 2;
                            self.gridview.enqueue(Cmd::UpdateStepSize);
                        }
                        Some(VirtualKeyCode::Minus) | Some(VirtualKeyCode::NumpadSubtract) => {
                            config.sim.step_size /= 2;
                            if config.sim.step_size < 1.into() {
                                config.sim.step_size = 1.into();
                            }
                            self.gridview.enqueue(Cmd::UpdateStepSize);
                        }
                        Some(VirtualKeyCode::Delete) => self.gridview.enqueue(Cmd::DeleteSelection),
                        Some(k @ VirtualKeyCode::LBracket)
                        | Some(k @ VirtualKeyCode::RBracket)
                        | Some(k @ VirtualKeyCode::Key0)
                        | Some(k @ VirtualKeyCode::Key1)
                        | Some(k @ VirtualKeyCode::Key2)
                        | Some(k @ VirtualKeyCode::Key3)
                        | Some(k @ VirtualKeyCode::Key4)
                        | Some(k @ VirtualKeyCode::Key5)
                        | Some(k @ VirtualKeyCode::Key6)
                        | Some(k @ VirtualKeyCode::Key7)
                        | Some(k @ VirtualKeyCode::Key8)
                        | Some(k @ VirtualKeyCode::Key9) => {
                            self.gridview.enqueue(Cmd::SetDrawState(match k {
                                // Select the previous cell state.
                                VirtualKeyCode::LBracket => {
                                    self.gridview.selected_cell_state().wrapping_sub(1)
                                }
                                // Select the next cell state.
                                VirtualKeyCode::RBracket => {
                                    self.gridview.selected_cell_state().wrapping_add(1)
                                }
                                VirtualKeyCode::Key0 => 0_u8,
                                VirtualKeyCode::Key1 => 1_u8,
                                VirtualKeyCode::Key2 => 2_u8,
                                VirtualKeyCode::Key3 => 3_u8,
                                VirtualKeyCode::Key4 => 4_u8,
                                VirtualKeyCode::Key5 => 5_u8,
                                VirtualKeyCode::Key6 => 6_u8,
                                VirtualKeyCode::Key7 => 7_u8,
                                VirtualKeyCode::Key8 => 8_u8,
                                VirtualKeyCode::Key9 => 9_u8,
                                _ => unreachable!(),
                            }));
                        }
                        _ => match *scancode {
                            sc::E => {
                                if let Some(mouse_pos) = self.state.mouse.pos {
                                    self.gridview.enqueue(Cmd::FocusCursor.at(mouse_pos));
                                }
                            }
                            _ => (),
                        },
                    }
                }

                if modifiers == CTRL {
                    match virtual_keycode {
                        // Select all.
                        Some(VirtualKeyCode::A) => self.gridview.enqueue(Cmd::SelectAll),
                        // Undo.
                        Some(VirtualKeyCode::Z) => self.gridview.enqueue(Cmd::Undo),
                        // Redo.
                        Some(VirtualKeyCode::Y) => self.gridview.enqueue(Cmd::Redo),
                        // Reset.
                        Some(VirtualKeyCode::R) => self.gridview.enqueue(Cmd::Reset),
                        // Cut RLE.
                        Some(VirtualKeyCode::X) => {
                            self.gridview.enqueue(Cmd::CopySelection(CaFormat::Rle));
                            self.gridview.enqueue(Cmd::DeleteSelection);
                        }
                        // Copy RLE.
                        Some(VirtualKeyCode::C) => {
                            self.gridview.enqueue(Cmd::CopySelection(CaFormat::Rle));
                        }
                        // Paste.
                        Some(VirtualKeyCode::V) => self.gridview.enqueue(Cmd::PasteSelection),
                        // Center view.
                        Some(VirtualKeyCode::M) => self.gridview.enqueue(Cmd::ResetView),
                        // Fit view to pattern.
                        Some(VirtualKeyCode::F) => self.gridview.enqueue(Cmd::FitView),
                        _ => (),
                    }
                }

                if modifiers == SHIFT | CTRL {
                    match virtual_keycode {
                        // Redo.
                        Some(VirtualKeyCode::Z) => self.gridview.enqueue(Cmd::Redo),
                        // Cut Macrocell.
                        Some(VirtualKeyCode::X) => {
                            self.gridview
                                .enqueue(Cmd::CopySelection(CaFormat::Macrocell));
                            self.gridview.enqueue(Cmd::DeleteSelection);
                        }
                        // Copy Macrocell.
                        Some(VirtualKeyCode::C) => {
                            self.gridview
                                .enqueue(Cmd::CopySelection(CaFormat::Macrocell));
                        }

                        // Reload shaders (debug build only).
                        // #[cfg(debug_assertions)]
                        Some(VirtualKeyCode::R) => crate::gridview::render::hot_reload_shaders(),

                        _ => (),
                    }
                }
            }
            // Ignore key release.
            _ => (),
        }
    }

    fn handle_mouse_press(&mut self, button: MouseButton) {
        let config = CONFIG.lock();

        // Ignore mouse press if we are already dragging.
        if self.state.dragging_button.is_some() {
            return;
        }

        // Ignore mouse press if we don't own the mouse cursor.
        let mouse_pos = match self.state.mouse.pos {
            Some(mouse_pos) => mouse_pos,
            None => return,
        };

        let ndim = self.gridview.ndim();
        let mods = self.state.modifiers;
        let (click_binding, drag_binding) = config.mouse.get_bindings(ndim, mods, button);

        let maybe_mouse_target = self.gridview.last_render_result().mouse_target.as_ref();
        if button == MouseButton::Left && maybe_mouse_target.is_some() {
            // Possibility #1: Drag mouse target (left mouse button only)
            let mouse_target_data = maybe_mouse_target.unwrap();
            let binding = &mouse_target_data.binding;
            self.state.dragging_button = Some(button);
            self.state.mouse.display_mode = binding.mouse_display_mode();
            self.gridview
                .enqueue(Cmd::BeginDrag(binding.clone()).at(mouse_pos));
        } else if let Some(b) = click_binding {
            // Possibility #2: Click
            match b {
                // If mouse click bindings are ever implemented, they will
                // need to be handled here.
                _ => unimplemented!("Mouse click bindings are not implemented"),
            }
        } else if let Some(b) = drag_binding {
            // Possibility #3: Drag
            self.state.dragging_button = Some(button);
            self.state.mouse.display_mode = b.mouse_display_mode();
            self.gridview
                .enqueue(Cmd::BeginDrag(b.clone()).at(mouse_pos));
        }
    }
    fn handle_mouse_release(&mut self, button: MouseButton) {
        if self.state.dragging_button == Some(button) {
            self.gridview.enqueue(Cmd::EndDrag);
            self.state.dragging_button = None;
        }
    }

    fn update_mouse_state(&mut self) {
        let config = CONFIG.lock();

        self.state.mouse.dragging = self.state.dragging_button.is_some();

        if !self.state.mouse.dragging {
            let (click_binding, drag_binding) = config.mouse.get_bindings(
                self.gridview.ndim(),
                self.state.modifiers,
                MouseButton::Left,
            );
            if let Some(mouse_target_data) = &self.gridview.last_render_result().mouse_target {
                self.state.mouse.display_mode = mouse_target_data.binding.mouse_display_mode();
            } else {
                self.state.mouse.display_mode = None
                    .or(click_binding.as_ref().map(|b| b.mouse_display_mode()))
                    .or(drag_binding.as_ref().map(|b| b.mouse_display_mode()))
                    .unwrap_or(MouseDisplayMode::Normal);
            }
        }
    }

    pub fn finish(mut self) {
        self.update_mouse_state();

        let mut config = CONFIG.lock();

        // Update DPI in config.

        config.gfx.dpi = self.dpi;

        // Move the viewpoint in one step.

        let mut moved = false;
        let mut scaled = false;

        if let Some(drag_cmd) = self.gridview.drag_cmd() {
            if drag_cmd.is_view_cmd() {
                moved = true;
                scaled = true;
            }
        }

        let distance_per_second = if self.state.modifiers.shift() {
            config.ctrl.speed_modifier
        } else {
            1.0
        };
        let frame_duration = self
            .gridview
            .frame_duration()
            .unwrap_or(Duration::default()); // .unwrap_or(Duration::zero());
        let speed = distance_per_second * frame_duration.as_secs_f64();

        let keys = &self.state.keys;

        // 'A' or left arrow => pan west.
        let pan_left = keys[sc::A] || keys[VirtualKeyCode::Left];
        // 'D' or right arrow => pan east.
        let pan_right = keys[sc::D] || keys[VirtualKeyCode::Right];
        // 'W' or up arrow => pan north (2D) / forward (3D).
        let pan_north = keys[sc::W] || keys[VirtualKeyCode::Up];
        // 'S' or down arrow => pan south (2D) / backward (3D).
        let pan_south = keys[sc::S] || keys[VirtualKeyCode::Down];

        // 'Q' or page up => zoom in (2D).
        let zoom_in = self.gridview.is_2d() && (keys[sc::Q] || keys[VirtualKeyCode::PageUp]);
        // 'Z' or page down => zoom out (2D).
        let zoom_out = self.gridview.is_2d() && (keys[sc::Z] || keys[VirtualKeyCode::PageDown]);

        let move_left = pan_left;
        let move_right = pan_right;
        let move_fwd = pan_north;
        let move_back = pan_south;
        // 'Q' or page up => move up (3D).
        let move_up = self.gridview.is_3d() && (keys[sc::Q] || keys[VirtualKeyCode::PageUp]);
        // 'Z' or page down => move down (3D).
        let move_down = self.gridview.is_3d() && (keys[sc::Z] || keys[VirtualKeyCode::PageDown]);

        if self.has_keyboard && !self.state.modifiers.intersects(CTRL | ALT | LOGO) {
            match self.gridview {
                GridView::View2D(view2d) => {
                    let move_speed = config.ctrl.keybd_move_speed_2d * speed * self.dpi;
                    let dx = if pan_left { -move_speed } else { 0.0 }
                        + if pan_right { move_speed } else { 0.0 };
                    let dy = if pan_south { -move_speed } else { 0.0 }
                        + if pan_north { move_speed } else { 0.0 };
                    if (dx, dy) != (0.0, 0.0) {
                        view2d.enqueue(Move2D { dx, dy });
                        moved = true;
                    }

                    let scale_speed = config.ctrl.keybd_scale_speed_2d * speed;
                    let log2_factor = if zoom_in { scale_speed } else { 0.0 }
                        + if zoom_out { -scale_speed } else { 0.0 };
                    if log2_factor != 0.0 {
                        view2d.enqueue(Cmd::Scale(log2_factor));
                        scaled = true;
                    }
                }

                GridView::View3D(view3d) => {
                    let move_speed = config.ctrl.keybd_move_speed_3d * speed * self.dpi;
                    let dx = if move_left { -move_speed } else { 0.0 }
                        + if move_right { move_speed } else { 0.0 };
                    let dy = if move_down { -move_speed } else { 0.0 }
                        + if move_up { move_speed } else { 0.0 };
                    let dz = if move_fwd { -move_speed } else { 0.0 }
                        + if move_back { move_speed } else { 0.0 };
                    if (dx, dy, dz) != (0.0, 0.0, 0.0) {
                        view3d.enqueue(Move3D {
                            dx,
                            dy,
                            dz,
                            ..Default::default()
                        });
                        moved = true;
                    }
                }
            }
        }
        if !moved
            && ((self.gridview.is_2d() && config.ctrl.snap_pos_2d)
                || (self.gridview.is_3d() && config.ctrl.snap_pos_3d))
        {
            // Snap to the nearest position.
            self.gridview.enqueue(Cmd::SnapPos);
        }
        if scaled {
            self.state.scale_snap_cooldown = Some(Instant::now() + Duration::from_millis(10));
        }

        let time_to_snap_scale = self.state.scale_snap_cooldown.is_none()
            || (Instant::now() >= self.state.scale_snap_cooldown.unwrap());
        if time_to_snap_scale
            && ((self.gridview.is_2d() && config.ctrl.snap_scale_2d)
                || (self.gridview.is_3d() && config.ctrl.snap_scale_3d))
        {
            // Snap to the nearest power-of-2 scale.
            self.gridview.enqueue(Cmd::SnapScale);
        }

        // Send mouse drag command, if dragging.

        if self.state.dragging_button.is_some() {
            if let Some(mouse_pos) = self.state.mouse.pos {
                self.gridview.enqueue(Cmd::ContinueDrag.at(mouse_pos));
            } else {
                self.gridview.enqueue(Cmd::EndDrag);
                self.state.dragging_button = None;
            }
        }
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
    /// Updates internal key state based on a KeyboardInput event.
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
            &true
        } else {
            &false
        }
    }
}
impl Index<VirtualKeyCode> for KeysPressed {
    type Output = bool;
    fn index(&self, virtual_keycode: VirtualKeyCode) -> &bool {
        if self.virtual_keycodes.contains(&virtual_keycode) {
            &true
        } else {
            &false
        }
    }
}
