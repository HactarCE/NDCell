use imgui::*;

use crate::ui::State;

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
        ui.text(im_str!("Hello, world!"));
    });
}
