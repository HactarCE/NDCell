use imgui::*;

use crate::windows::BuildParams;

#[derive(Debug, Default)]
pub struct DebugWindow {
    pub is_visible: bool,
}
impl DebugWindow {
    /// Builds the window.
    pub fn build(&mut self, params: &mut BuildParams<'_>) {
        let BuildParams { ui, .. } = params;

        if self.is_visible {
            Window::new(&ImString::new("Debug values"))
                .size([400.0, 300.0], Condition::FirstUseEver)
                .build(&ui, || {
                    ui.text("");
                    if ui.button(
                        im_str!("Start capture"),
                        [ui.window_content_region_width(), 20.0],
                    ) {
                        optick::start_capture();
                    }
                    if ui.button(
                        im_str!("Stop capture"),
                        [ui.window_content_region_width(), 20.0],
                    ) {
                        optick::stop_capture("capture");
                    }

                    let mut debug_info = crate::debug::FRAME_DEBUG_INFO.lock();
                    ui.text(&*debug_info);
                    *debug_info = String::new();
                })
        }
    }
}
