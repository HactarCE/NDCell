mod main;
// TODO this module probably shouldn't be public?
pub mod simulation;

/// Builds all the imgui windows.
pub fn build_windows(ui: &imgui::Ui) {
    main::build(ui);
    simulation::build(ui);
}
