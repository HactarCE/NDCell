//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

use glium::glutin;
use imgui::{Context, FontSource};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use log::warn;
use ref_thread_local::RefThreadLocal;
use send_wrapper::SendWrapper;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::RwLock;
use std::time::Instant;

mod clipboard_compat;
mod gridview;
mod gui;
mod history;
mod input;

use crate::automaton::*;
use clipboard_compat::*;
use gridview::*;
use history::History;
use rle::RleEncode;

/// The title of the window (both the OS window, and the main imgui window).
pub const TITLE: &str = "NDCell";

ref_thread_local! {
    static managed CURRENT_GRIDVIEW: GridView = get_default_gridview();
}

lazy_static! {
    static ref EVENTS_LOOP: SendWrapper<RefCell<glutin::EventsLoop>> =
        SendWrapper::new(RefCell::new(glutin::EventsLoop::new()));
    pub static ref DISPLAY: SendWrapper<glium::Display> = SendWrapper::new({
        let wb = glutin::WindowBuilder::new().with_title(TITLE.to_owned());
        let cb = glutin::ContextBuilder::new().with_vsync(true);
        glium::Display::new(wb, cb, &EVENTS_LOOP.borrow()).expect("Failed to initialize display")
    });
    static ref DPI: RwLock<f64> = RwLock::new(1.0);
}

pub fn get_dpi() -> f64 {
    *DPI.read().unwrap()
}
pub fn set_dpi(dpi: f64) {
    *DPI.write().unwrap() = dpi;
}

/// Returns an immutable reference to the active gridview.
pub fn gridview<'a>() -> ref_thread_local::Ref<'a, GridView> {
    CURRENT_GRIDVIEW.borrow()
}
/// Returns a mutable reference to the active gridview.
pub fn gridview_mut<'a>() -> ref_thread_local::RefMut<'a, GridView> {
    CURRENT_GRIDVIEW.borrow_mut()
}

const GOSPER_GLIDER_GUN_SYNTH_RLE: &str = "
#CXRLE Gen=-31
x = 47, y = 14, rule = Life
16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
$26bobo!
";

fn get_default_gridview() -> GridView {
    let mut automaton = Automaton2D::from_rle(GOSPER_GLIDER_GUN_SYNTH_RLE).unwrap_or_else(|_| {
        warn!("Unable to parse default pattern; using empty pattern instead");
        Default::default()
    });
    automaton.sim = Simulation::new(Rc::new(rule::LIFE));
    GridView::from(automaton)
}

/// Display the main application window.
pub fn show_gui() {
    let display = &**DISPLAY;
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
    set_dpi(platform.hidpi_factor()); // Fetch the DPI we want.
    platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Locked(1.0));

    // Initialize imgui fonts.
    let font_size = 16.0 * get_dpi() as f32;
    imgui.fonts().add_font(&[FontSource::TtfData {
        data: include_bytes!("../../resources/font/NotoSans-Regular.ttf"),
        size_pixels: font_size,
        config: None,
    }]);

    // Initialize imgui renderer.
    let mut renderer = Renderer::init(&mut imgui, display).expect("Failed to initialize renderer");

    let mut input_state = input::State::default();

    // Main loop
    let mut last_frame_time = Instant::now();
    let mut closed = false;
    while !closed {
        gridview_mut().do_frame();

        let io = imgui.io_mut();
        platform
            .prepare_frame(io, &window)
            .expect("Failed to start frame");
        last_frame_time = io.update_delta_time(last_frame_time);

        let has_keyboard = !io.want_capture_keyboard;
        let has_mouse = !io.want_capture_mouse;

        EVENTS_LOOP.borrow_mut().poll_events(|ev| {
            // Let imgui handle events.
            platform.handle_event(imgui.io_mut(), &window, &ev);
            // Handle events for the grid view.
            input_state.handle_event(&ev, has_keyboard, has_mouse);
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

        input_state.do_frame(has_keyboard, has_mouse);

        let ui = imgui.frame();
        gui::build_windows(&ui);

        let mut target = display.draw();

        match &mut *gridview_mut() {
            GridView::View2D(view2d) => {
                let gridview_render_params = View2DRenderParams {
                    cursor_pos: input_state.get_cursor_pos(),
                };
                view2d.render(&mut target, gridview_render_params);
            }
            GridView::View3D(_view3d) => (),
        };

        platform.prepare_render(&ui, &window);
        let draw_data = ui.render();
        renderer
            .render(&mut target, draw_data)
            .expect("Rendering failed");

        target.finish().expect("Failed to swap buffers");
    }
}

pub fn copy_rle_to_clipboard() -> Result<(), String> {
    gridview_mut().stop_running();
    match gridview().get_automaton() {
        Automaton::Automaton2D(automaton) => clipboard_set(rle::RleEncode::to_rle(automaton))
            .map_err(|_| "Unable to set clipboard contents")?,
        _ => Err("Unable to convert non-2D patterns to RLE")?,
    }
    Ok(())
}
pub fn copy_cxrle_to_clipboard() -> Result<(), String> {
    gridview_mut().stop_running();
    match gridview().get_automaton() {
        Automaton::Automaton2D(automaton) => clipboard_set(rle::RleEncode::to_cxrle(automaton))
            .map_err(|_| "Unable to set clipboard contents")?,
        _ => Err("Unable to convert non-2D patterns to RLE")?,
    }
    Ok(())
}
pub fn load_rle_from_clipboard() -> Result<(), String> {
    let mut gridview = gridview_mut();
    gridview.stop_running();
    gridview.record();
    let mut automaton: Automaton2D = rle::RleEncode::from_rle(
        &clipboard_get().map_err(|_| "Unable to access clipboard contents")?,
    )?;
    automaton.sim = Simulation::new(Rc::new(rule::LIFE));
    *gridview = GridView::from(automaton);
    Ok(())
}
