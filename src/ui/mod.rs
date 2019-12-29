//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

use glium::glutin;
use imgui::{Context, FontSource};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use std::rc::Rc;
use std::time::Instant;

mod gridview;
mod gui;
mod history;
mod input;

use crate::automaton::*;
use gridview::{GridView, GridViewTrait};
use history::*;

/// The title of the window (both the OS window, and the main imgui window).
pub const TITLE: &str = "NDCell";

/// The state of the program, including the automaton and any settings or
/// configuration.
pub struct State {
    pub grid_view: GridView,
    pub history: Vec<HistoryStack>,
    pub gui: gui::GuiWindows,
}

const GOSPER_GLIDER_GUN: [Dim2D; 36] = [
    [24, 0],
    [22, 1],
    [24, 1],
    [12, 2],
    [13, 2],
    [20, 2],
    [21, 2],
    [34, 2],
    [35, 2],
    [11, 3],
    [15, 3],
    [20, 3],
    [21, 3],
    [34, 3],
    [35, 3],
    [0, 4],
    [1, 4],
    [10, 4],
    [16, 4],
    [20, 4],
    [21, 4],
    [0, 5],
    [1, 5],
    [10, 5],
    [14, 5],
    [16, 5],
    [17, 5],
    [22, 5],
    [24, 5],
    [10, 6],
    [16, 6],
    [24, 6],
    [11, 7],
    [15, 7],
    [12, 8],
    [13, 8],
];

/// Display the main application window.
pub fn show_gui() {
    // Initialize all glium/glutin stuff.
    let mut events_loop = glutin::EventsLoop::new();
    let wb = glutin::WindowBuilder::new().with_title(TITLE.to_owned());
    let cb = glutin::ContextBuilder::new();
    let display =
        Rc::new(glium::Display::new(wb, cb, &events_loop).expect("Failed to initialize display"));

    // Initialize imgui things.
    let mut imgui = Context::create();
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
    let mut grid = NdTree::new();
    for &pos in GOSPER_GLIDER_GUN.into_iter() {
        grid.set_cell(pos.into(), 1);
    }
    // grid.set_cell([3, 3].into(), 1);
    // grid.set_cell([4, 3].into(), 1);
    // grid.set_cell([5, 3].into(), 1);
    // grid.set_cell([5, 2].into(), 1);
    // grid.set_cell([4, 1].into(), 1);
    let mut automaton = NdAutomaton::default();
    automaton.tree = grid;
    automaton.sim = Simulation::new(Rc::new(rule::LIFE), 1024);
    let mut state = State {
        grid_view: GridView::new_2d(display.clone(), automaton),
        history: Default::default(),
        gui: Default::default(),
    };

    // Main loop
    let mut last_frame_time = Instant::now();
    let mut closed = false;
    while !closed {
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

        state.grid_view.do_frame();

        let io = imgui.io_mut();
        // TODO you'll probably want these
        // io.want_capture_keyboard = true;
        // io.want_capture_mouse = true;
        platform
            .prepare_frame(io, &window)
            .expect("Failed to start frame");
        last_frame_time = io.update_delta_time(last_frame_time);

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
