mod main;

/// Builds all the imgui windows.
pub fn build_windows(ui: &imgui::Ui) {
    main::build(ui);
}
