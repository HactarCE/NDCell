use glium::glutin::dpi::{PhysicalPosition, PhysicalSize};
use glium::glutin::event::*;
use imgui_winit_support::WinitPlatform;
use std::collections::HashSet;
use std::fmt;
use std::ops::Index;
use std::time::{Duration, Instant};

use ndcell_core::prelude::*;

use crate::commands::*;
use crate::config::{Config, MouseDragBinding};
use crate::gridview::{GridView, GridViewTrait};
use crate::Scale;

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

    /// Drag handler for the left mouse button, if it is pressed and being
    /// dragged.
    lmb_drag_handler: Option<DragHandler>,
    /// Drag handler for the right mouse button, if it is pressed and being
    /// dragged.
    rmb_drag_handler: Option<DragHandler>,
    /// Drag handler for the middle mouse button, if it is pressed and being
    /// dragged.
    mmb_drag_handler: Option<DragHandler>,

    /// Current pixel position of the mouse cursor (relative to top left).
    cursor_pos: Option<FVec2D>,

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
    fn drag_handler(&mut self, button: MouseButton) -> Option<&mut Option<DragHandler>> {
        match button {
            MouseButton::Left => Some(&mut self.lmb_drag_handler),
            MouseButton::Right => Some(&mut self.rmb_drag_handler),
            MouseButton::Middle => Some(&mut self.mmb_drag_handler),
            MouseButton::Other(_) => None,
        }
    }

    pub fn cursor_pos(&self) -> Option<FVec2D> {
        self.cursor_pos
    }

    pub fn frame<'a>(
        &'a mut self,
        config: &'a mut Config,
        gridview: &'a GridView,
        imgui_io: &imgui::Io,
        platform: &WinitPlatform,
    ) -> FrameInProgress<'a> {
        let has_keyboard = !imgui_io.want_capture_keyboard;
        let has_mouse = !imgui_io.want_capture_mouse;
        if !has_mouse {
            self.cursor_pos = None;
        }
        FrameInProgress {
            state: self,
            config,
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
    config: &'a mut Config,
    gridview: &'a GridView,

    /// Whether to handle keyboard input (false if it is captured by imgui).
    has_keyboard: bool,
    /// Whether to handle mouse input (false if it is captured by imgui).
    has_mouse: bool,
    /// HiDPI factor.
    dpi: f64,
}
impl FrameInProgress<'_> {
    #[must_use = "DragHandler must be used"]
    fn start_drag<A: 'static, C: 'static + Into<Command>>(
        gridview: &GridView,
        command_constructor: fn(Drag<A>) -> C,
        action: A,
        cursor_start: FVec2D,
    ) -> DragHandler {
        gridview.enqueue(command_constructor(Drag::Start {
            action,
            cursor_start,
        }));
        DragHandler {
            continue_command: Box::new(move |cursor_pos| {
                command_constructor(Drag::Continue { cursor_pos }).into()
            }),
            stop_command: command_constructor(Drag::Stop).into(),
        }
    }
    fn update_drag(
        gridview: &GridView,
        drag_handler: &mut Option<DragHandler>,
        cursor_pos: Option<FVec2D>,
    ) {
        if let Some(h) = drag_handler {
            if let Some(pos) = cursor_pos {
                gridview.enqueue((h.continue_command)(pos));
            } else {
                gridview.enqueue(h.stop_command.clone());
                *drag_handler = None;
            }
        }
    }
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
                        self.state.cursor_pos = None;
                    }
                    WindowEvent::CursorMoved {
                        position: PhysicalPosition { x, y },
                        ..
                    } => {
                        self.state.cursor_pos = None;
                        if self.has_mouse {
                            if let (Some(x), Some(y)) = (R64::try_new(*x), R64::try_new(*y)) {
                                self.state.cursor_pos = Some(NdVec([x, y]));
                            }
                        }

                        let gv = self.gridview;
                        let pos = self.state.cursor_pos;
                        // Update in this order so that panning happens before
                        // drawing; panning is generally assigned to middle or
                        // right mouse buttons, while drawing is generally
                        // assigned to the left mouse button. This is obviously
                        // a hack, but it's an easy workaround for a
                        // low-priority bug (panning quickly while drawing).
                        Self::update_drag(gv, &mut self.state.mmb_drag_handler, pos);
                        Self::update_drag(gv, &mut self.state.rmb_drag_handler, pos);
                        Self::update_drag(gv, &mut self.state.lmb_drag_handler, pos);
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
                        self.gridview.enqueue(ViewCommand::Scale {
                            log2_factor: dy * speed,
                            invariant_pos: self.state.cursor_pos,
                        });
                        self.state.scale_snap_cooldown = Some(Instant::now() + SCALE_SNAP_COOLDOWN);
                    }
                    WindowEvent::MouseInput { button, state, .. } => {
                        let ndim = self.gridview.ndim();
                        let mods = self.state.modifiers;
                        let cursor_pos = self.state.cursor_pos();

                        if let Some(drag_handler) = self.state.drag_handler(*button) {
                            match state {
                                ElementState::Pressed => {
                                    if let Some(cursor_start) = cursor_pos {
                                        let (_click_action, drag_action) = self
                                            .config
                                            .keys
                                            .get_mouse_bindings(ndim, mods, *button);

                                        if let Some(a) = drag_action {
                                            *drag_handler = Some(match a {
                                                MouseDragBinding::View(action) => Self::start_drag(
                                                    self.gridview,
                                                    ViewCommand::Drag,
                                                    action,
                                                    cursor_start,
                                                ),
                                                MouseDragBinding::Draw(action) => Self::start_drag(
                                                    self.gridview,
                                                    DrawCommand,
                                                    (action, 1_u8),
                                                    cursor_start,
                                                ),
                                                MouseDragBinding::Select(action) => {
                                                    Self::start_drag(
                                                        self.gridview,
                                                        SelectCommand,
                                                        action,
                                                        cursor_start,
                                                    )
                                                }
                                            });
                                        }
                                    }
                                }
                                ElementState::Released => {
                                    if let Some(h) = drag_handler.take() {
                                        self.gridview.enqueue(h.stop_command);
                                    }
                                }
                            }
                        }
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
                                GridView::View2D(_) => ViewCommand::GoTo2D {
                                    x: Some(r64(0.0).into()),
                                    y: Some(r64(0.0).into()),
                                    relative: false,
                                    scaled: false,
                                },
                                GridView::View3D(_) => ViewCommand::GoTo3D {
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
                                .enqueue(ViewCommand::GoToScale(Scale::default()));
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
        self.config.gfx.dpi = self.dpi;

        let mut moved = false;
        let mut scaled = false;

        if self.gridview.is_dragging_view() {
            moved = true;
            scaled = true;
        }

        let speed_per_second = if self.state.modifiers.shift() {
            self.config.ctrl.speed_modifier
        } else {
            1.0
        };
        let speed = speed_per_second / self.gridview.fps(&self.config);

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
                    let move_speed = self.config.ctrl.keybd_move_speed_2d * speed * self.dpi;
                    let pan_x = if pan_left { -move_speed } else { 0.0 }
                        + if pan_right { move_speed } else { 0.0 };
                    let pan_y = if pan_south { -move_speed } else { 0.0 }
                        + if pan_north { move_speed } else { 0.0 };
                    if (pan_x, pan_y) != (0.0, 0.0) {
                        view2d.enqueue(ViewCommand::GoTo2D {
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
                        view2d.enqueue(ViewCommand::Scale {
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
                        view3d.enqueue(ViewCommand::GoTo3D {
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
        if !moved
            && ((self.gridview.is_2d() && self.config.ctrl.snap_pos_2d)
                || (self.gridview.is_3d() && self.config.ctrl.snap_pos_3d))
        {
            // Snap to the nearest position.
            self.gridview.enqueue(ViewCommand::SnapPos);
        }
        if scaled {
            self.state.scale_snap_cooldown = Some(Instant::now() + Duration::from_millis(10));
        }

        let time_to_snap_scale = self.state.scale_snap_cooldown.is_none()
            || (Instant::now() >= self.state.scale_snap_cooldown.unwrap());
        if time_to_snap_scale
            && ((self.gridview.is_2d() && self.config.ctrl.snap_scale_2d)
                || (self.gridview.is_3d() && self.config.ctrl.snap_scale_3d))
        {
            // Snap to the nearest power-of-2 scale.
            self.gridview.enqueue(ViewCommand::SnapScale {
                invariant_pos: None,
            });
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

struct DragHandler {
    pub continue_command: Box<dyn Fn(FVec2D) -> Command>,
    pub stop_command: Command,
}
impl fmt::Debug for DragHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DragHandler  {{ stop: {:?} }}", self.stop_command)
    }
}
