use imgui::*;

/// Builds the main window.
pub fn build(ui: &imgui::Ui) {
    Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
        ui.text(im_str!("Hello, world!"));
    });
}
