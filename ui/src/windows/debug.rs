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
                    let mut debug_info = crate::debug::FRAME_DEBUG_INFO.lock();
                    ui.text(&*debug_info);
                    *debug_info = String::new();
                })
        }
    }
}
