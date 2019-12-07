use imgui::*;

use crate::ui::State;

#[derive(Default)]
pub struct WindowState {
    pub visible: bool,
}

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    if state.gui.simulation.visible {
        Window::new(&ImString::new("Simulation")).build(&ui, || {
            ui.text("sim controls go here");
        })
    }
}
