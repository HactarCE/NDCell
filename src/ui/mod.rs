//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

use glium::glutin;
use imgui::{Context, FontSource};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use log::warn;
use num::{BigInt, Signed};
use std::rc::Rc;
use std::time::Instant;

mod clipboard_compat;
mod gridview;
mod gui;
mod history;
mod input;

use crate::automaton::*;
use clipboard_compat::*;
use gridview::{GridView, GridViewTrait};
use history::*;

const FPS: f32 = 60.0;

/// The title of the window (both the OS window, and the main imgui window).
pub const TITLE: &str = "NDCell";

/// The state of the program, including the automaton and any settings or
/// configuration.
pub struct State {
    pub display: Rc<glium::Display>,
    pub grid_view: GridView,
    pub history: HistoryStack,
    pub input_state: input::InputState,
    pub step_size: BigInt,
    pub gui: gui::GuiWindows,
    pub dpi: f64,
}

const GOSPER_GLIDER_GUN_SYNTH_RLE: &str = "
#CXRLE Gen=-31
x = 47, y = 14, rule = B3/S23
16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
$26bobo!
";

/// Display the main application window.
pub fn show_gui() {
    // Initialize all glium/glutin stuff.
    let mut events_loop = glutin::EventsLoop::new();
    let wb = glutin::WindowBuilder::new().with_title(TITLE.to_owned());
    let cb = glutin::ContextBuilder::new().with_vsync(true);
    let display =
        Rc::new(glium::Display::new(wb, cb, &events_loop).expect("Failed to initialize display"));

    // Initialize imgui things.
    let mut imgui = Context::create();
    imgui.set_clipboard_backend(Box::new(ClipboardCompat));
    imgui.set_ini_filename(None);
    let mut platform = WinitPlatform::init(&mut imgui);
    let gl_window = display.gl_window();
    let window = gl_window.window();
    // Using any other HiDpiMode::Default or HiDpiMode::Rounded makes window
    // borders and other things fuzzy for me (DPI = 1.5 in my setup) and
    // scaling can otherwise be accomplished by changing the font size, with
    // a MUCH crisper result.
    platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Default);
    let hidpi_factor = platform.hidpi_factor(); // Fetch the DPI we want.
    platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Locked(1.0));

    // Initialize imgui fonts.
    let font_size = 16.0 * hidpi_factor as f32;
    imgui.fonts().add_font(&[FontSource::TtfData {
        data: include_bytes!("../../resources/font/NotoSans-Regular.ttf"),
        size_pixels: font_size,
        config: None,
    }]);

    // Initialize imgui renderer.
    let mut renderer =
        Renderer::init(&mut imgui, &*display).expect("Failed to initialize renderer");

    // Initialize cellular automaton stuff.
    let mut automaton: Automaton2D = rle::RleEncode::from_rle(GOSPER_GLIDER_GUN_SYNTH_RLE)
        .unwrap_or_else(|_| {
            warn!("Unable to parse default pattern; using empty pattern instead");
            Default::default()
        });
    automaton.sim = Simulation::new(Rc::new(rule::LIFE));
    let mut state = State {
        display: display.clone(),
        grid_view: GridView::new_2d(display.clone(), automaton),
        history: Default::default(),
        input_state: Default::default(),
        step_size: BigInt::from(4),
        gui: Default::default(),
        dpi: hidpi_factor,
    };

    // Main loop
    let mut last_frame_time = Instant::now();
    let mut closed = false;
    while !closed {
        state.grid_view.do_frame();

        let io = imgui.io_mut();
        platform
            .prepare_frame(io, &window)
            .expect("Failed to start frame");
        last_frame_time = io.update_delta_time(last_frame_time);

        input::start_frame(&mut state);

        state.input_state.ignore_keyboard = io.want_capture_keyboard;
        state.input_state.ignore_mouse = io.want_capture_mouse;

        events_loop.poll_events(|ev| {
            // Let imgui handle events.
            platform.handle_event(imgui.io_mut(), &window, &ev);
            // Handle events for the grid view.
            input::handle_event(&mut state, &ev);
            // Handle events ourself.
            match ev {
                glutin::Event::WindowEvent { event, .. } => match event {
                    // Handle window close event.
                    glutin::WindowEvent::CloseRequested => closed = true,
                    _ => (),
                },
                _ => (),
            }
        });

        input::do_frame(&mut state);

        let ui = imgui.frame();
        gui::build_windows(&mut state, &ui);

        let mut target = display.draw();
        state.grid_view.draw(&mut target);

        platform.prepare_render(&ui, &window);
        let draw_data = ui.render();
        renderer
            .render(&mut target, draw_data)
            .expect("Rendering failed");
        target.finish().expect("Failed to swap buffers");
    }
}

impl State {
    pub fn undo(&mut self) -> bool {
        self.stop_running();
        self.history.undo(&mut self.grid_view)
    }
    pub fn redo(&mut self) -> bool {
        self.stop_running();
        self.history.redo(&mut self.grid_view)
    }
    pub fn reset(&mut self) -> usize {
        self.stop_running();
        let mut i = 0;
        while self.grid_view.get_generation_count().is_positive() && self.undo() {
            i += 1;
        }
        i
    }
    pub fn step_step_size(&mut self, record_history: bool) {
        if record_history {
            if self.stop_running() {
                return;
            }
            self.record_state();
        }
        self.grid_view.step(&self.step_size);
    }
    /// Steps forward in the simulation by the given number of generations.
    pub fn step(&mut self, step_size: &BigInt, record_history: bool) {
        if record_history {
            if self.stop_running() {
                return;
            }
            self.record_state();
        }
        self.grid_view.step(step_size);
    }
    /// Steps forward in the simulation by the given number of generations
    /// without clearing the results cache.
    pub fn step_no_cache_clear(&mut self, step_size: &BigInt, record_history: bool) {
        if record_history {
            if self.stop_running() {
                return;
            }
            self.record_state();
        }
        self.grid_view.step_no_cache_clear(&step_size);
    }
    pub fn toggle_running(&mut self) -> bool {
        if self.input_state.is_running {
            self.stop_running();
            false
        } else {
            self.start_running();
            true
        }
    }
    pub fn start_running(&mut self) {
        self.record_state();
        self.input_state.is_running = true;
    }
    pub fn stop_running(&mut self) -> bool {
        std::mem::replace(&mut self.input_state.is_running, false)
    }
    /// Record the current state in the undo history.
    fn record_state(&mut self) {
        self.history.record(self.grid_view.clone());
    }
    pub fn load_rle_from_clipboard(&mut self) -> Result<(), String> {
        self.record_state();
        let mut automaton: Automaton2D = rle::RleEncode::from_rle(
            &clipboard_get().map_err(|_| "Unable to access clipboard contents")?,
        )?;
        automaton.sim = Simulation::new(Rc::new(rule::LIFE));
        self.grid_view = GridView::new_2d(self.display.clone(), automaton);
        Ok(())
    }
}
