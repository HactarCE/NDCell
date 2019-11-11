mod main;

use crate::ui::State;

/// Builds all the imgui windows.
pub fn build_windows(state: &mut State, ui: &imgui::Ui) {
    main::build(state, ui);
}
