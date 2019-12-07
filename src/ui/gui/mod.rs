mod main;
mod simulation;

use crate::ui::State;

#[derive(Default)]
pub struct GuiWindows {
    pub main: main::WindowState,
    pub simulation: simulation::WindowState,
}

/// Builds all the imgui windows.
pub fn build_windows(state: &mut State, ui: &imgui::Ui) {
    main::build(state, ui);
    simulation::build(state, ui);
}
