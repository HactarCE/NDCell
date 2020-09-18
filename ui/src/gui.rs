use glium::glutin::event::{Event, StartCause, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::{window::WindowBuilder, ContextBuilder};
use glium::Surface;
use imgui::{Context, FontSource};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use log::warn;
use send_wrapper::SendWrapper;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

use ndcell_core::automaton::Automaton2D;
use ndcell_core::traits::RleEncode;

use super::clipboard_compat::*;
use super::gridview::*;

lazy_static! {
    static ref EVENT_LOOP: SendWrapper<RefCell<Option<EventLoop<()>>>> =
        SendWrapper::new(RefCell::new(Some(EventLoop::new())));
    pub static ref DISPLAY: SendWrapper<glium::Display> = SendWrapper::new({
        let wb = WindowBuilder::new().with_title(super::TITLE.to_owned());
        let cb = ContextBuilder::new().with_vsync(true);
        glium::Display::new(wb, cb, EVENT_LOOP.borrow().as_ref().unwrap())
            .expect("Failed to initialize display")
    });
}

const GOSPER_GLIDER_GUN_SYNTH_RLE: &str = "
#CXRLE Gen=-31
x = 47, y = 14, rule = Life
16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
$26bobo!
";

fn make_default_gridview() -> GridView {
    let mut automaton = Automaton2D::from_rle(GOSPER_GLIDER_GUN_SYNTH_RLE).unwrap_or_else(|_| {
        warn!("Unable to parse default pattern; using empty pattern instead");
        Default::default()
    });

    automaton.rule = crate::load_custom_rule();

    GridView::from(automaton)
}

/// Display the main application window.
pub fn show_gui() -> ! {
    let display = &**DISPLAY;

    // Initialize runtime data.
    let mut config = super::config::Config::default();
    let mut gridview = make_default_gridview();
    let mut main_window = super::windows::MainWindow::default();
    let mut input_state = super::input::State::default();
    let mut events_buffer = VecDeque::new();

    // Initialize imgui.
    let mut imgui = Context::create();
    imgui.set_clipboard_backend(Box::new(ClipboardCompat));
    imgui.set_ini_filename(None);
    let mut platform = WinitPlatform::init(&mut imgui);
    let gl_window = display.gl_window();
    let window = gl_window.window();
    // Imgui can't handle mixed DPI, so let's just scale using font size which
    // gives a better and more reliable result. TODO: make this
    // user-configurable. TODO: also scale all buttons etc
    platform.attach_window(imgui.io_mut(), window, HiDpiMode::Default);
    config.gfx.dpi = platform.hidpi_factor(); // Fetch the DPI we want.
    platform.attach_window(imgui.io_mut(), window, HiDpiMode::Locked(1.0));

    // Initialize imgui fonts.
    let font_size = 16.0 * config.gfx.dpi as f32;
    imgui.fonts().add_font(&[FontSource::TtfData {
        data: include_bytes!("../resources/font/NotoSans-Regular.ttf"),
        size_pixels: font_size,
        config: None,
    }]);

    // Initialize imgui renderer.
    let mut renderer = Renderer::init(&mut imgui, display).expect("Failed to initialize renderer");

    // Main loop
    let mut last_frame_time = Instant::now();
    EVENT_LOOP
        .borrow_mut()
        .take()
        .unwrap()
        .run(move |event, _ev_loop, control_flow| {
            // Decide whether to handle events and render everything.
            let do_frame = match event.to_static() {
                Some(Event::NewEvents(cause)) => match cause {
                    StartCause::ResumeTimeReached { .. } | StartCause::Init => true,
                    _ => false,
                },
                Some(Event::LoopDestroyed) => {
                    // The program is about to exit.
                    false
                }
                Some(ev) => {
                    // Queue the event to be handled next time we render
                    // everything.
                    events_buffer.push_back(ev);
                    false
                }
                None => {
                    // Ignore this event.
                    false
                }
            };

            if do_frame {
                let current_time = Instant::now();
                let next_frame_time = current_time + Duration::from_secs_f64(1.0 / config.gfx.fps);
                *control_flow = ControlFlow::WaitUntil(next_frame_time);

                // Prep imgui for event handling.
                let imgui_io = imgui.io_mut();
                platform
                    .prepare_frame(imgui_io, gl_window.window())
                    .expect("Failed to start frame");
                last_frame_time = imgui_io.update_delta_time(last_frame_time);

                // Prep the gridview for event handling.
                let mut input_frame = input_state.frame(&mut config, &gridview, &imgui_io);

                for ev in events_buffer.drain(..) {
                    // Let imgui handle events.
                    platform.handle_event(imgui_io, gl_window.window(), &ev);
                    // Handle events for the gridview.
                    input_frame.handle_event(&ev);
                    // Handle events ourself.
                    match ev {
                        Event::WindowEvent { event, .. } => match event {
                            // Handle window close event.
                            WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                            _ => (),
                        },
                        _ => (),
                    }
                }

                // Finish handling events for the gridview.
                input_frame.finish();

                // Prep imgui for rendering.
                let ui = imgui.frame();
                main_window.build(&ui, &mut config, &gridview);

                // Update the viewport and run the simulation if necessary.
                gridview.do_frame(&config);

                let mut target = display.draw();
                if target.get_dimensions() != (0, 0) {
                    // Render the gridview.
                    match &mut gridview {
                        GridView::View2D(view2d) => {
                            view2d.render(
                                &config,
                                &mut target,
                                View2DRenderParams {
                                    cursor_pos: input_state.cursor_pos(),
                                },
                            );
                        }
                        GridView::View3D(_view3d) => (),
                    };

                    // Render imgui.
                    platform.prepare_render(&ui, gl_window.window());
                    let draw_data = ui.render();
                    renderer
                        .render(&mut target, draw_data)
                        .expect("Rendering failed");
                }
                // Put it all on the screen.
                target.finish().expect("Failed to swap buffers");
            }
        })
}
