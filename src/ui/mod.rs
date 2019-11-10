use glium::glutin;
use glium::Surface;
use imgui::{im_str, Context, FontSource, Window};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use std::time::Instant;

pub const TITLE: &str = "NDCell";

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
        data: include_bytes!("/usr/share/fonts/noto/NotoSans-Regular.ttf"),
        size_pixels: font_size,
        config: None,
    }]);
    // TODO: Load list of OS system fonts using another package

    // Initialize imgui renderer.
    let mut renderer = Renderer::init(&mut imgui, &display).expect("Failed to initialize renderer");

    // Main loop
    let mut last_frame_time = Instant::now();
    let mut closed = false;
    while !closed {
        events_loop.poll_events(|ev| {
            platform.handle_event(imgui.io_mut(), &window, &ev);
            match ev {
                glutin::Event::WindowEvent { event, .. } => match event {
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

        Window::new(im_str!("NDCell")).build(&ui, || {
            ui.text(im_str!("Hello, world!"));
        });

        let mut target = display.draw();
        target.clear_color_srgb(0.2, 0.2, 0.2, 1.0);
        platform.prepare_render(&ui, &window);
        let draw_data = ui.render();
        renderer
            .render(&mut target, draw_data)
            .expect("Rendering failed");
        target.finish().expect("Failed to swap buffers");
    }
}
