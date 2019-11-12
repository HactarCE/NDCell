//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

use glium::glutin;
use imgui::{Context, FontSource};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use std::time::Instant;

mod gui;
mod render;

use crate::automaton::*;

/// The title of the program window (both the OS window, and the main imgui
/// window).
pub const TITLE: &str = "NDCell";

/// The state of the program, including the automaton and any
/// settings/configuration.
pub struct State {
    pub grid_view: render::GridView,
}

/// Display the main application window.
pub fn show_gui() {
    // Initialize all glium/glutin stuff.
    let mut events_loop = glutin::EventsLoop::new();
    let wb = glutin::WindowBuilder::new().with_title(TITLE.to_owned());
    let cb = glutin::ContextBuilder::new();
    let display = glium::Display::new(wb, cb, &events_loop).expect("Failed to initialize display");

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
    let mut renderer = Renderer::init(&mut imgui, &display).expect("Failed to initialize renderer");

    // Initialize cellular automaton stuff.
    let mut grid = Grid::new();
    grid[CellCoords([3, 3])] = true;
    grid[CellCoords([4, 3])] = true;
    grid[CellCoords([5, 3])] = true;
    grid[CellCoords([5, 2])] = true;
    grid[CellCoords([4, 1])] = true;
    let mut state = State {
        grid_view: render::GridView::Grid2D(render::Grid2D::new(Box::new(Automaton::new(
            algorithm::LIFE,
            grid,
        )))),
    };

    // Main loop
    let mut last_frame_time = Instant::now();
    let mut closed = false;
    while !closed {
        events_loop.poll_events(|ev| {
            // Let imgui handle events.
            platform.handle_event(imgui.io_mut(), &window, &ev);
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

        let io = imgui.io_mut();
        platform
            .prepare_frame(io, &window)
            .expect("Failed to start frame");
        last_frame_time = io.update_delta_time(last_frame_time);

        let ui = imgui.frame();
        gui::build_windows(&mut state, &ui);

        let mut target = display.draw();
        render::draw_editor(&mut state, &display, &mut target);

        platform.prepare_render(&ui, &window);
        let draw_data = ui.render();
        renderer
            .render(&mut target, draw_data)
            .expect("Rendering failed");
        target.finish().expect("Failed to swap buffers");
    }
}
