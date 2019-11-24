use imgui::*;

use crate::ui::render::{Grid2D, GridView};
use crate::ui::State;

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
        ui.text(im_str!("Hello, world!"));
        match &state.grid_view {
            GridView::Grid2D(Grid2D { x, y, slice, scale }) => {
                ui.text(format!("X = {}", x));
                ui.text(format!("Y = {}", y));
                ui.text(format!("Generations = {}", slice.get_generations()));
                ui.text(format!("Population = {}", slice.get_population()));
                ui.text(format!("Scale = {}", scale));
                ui.text(format!("Max node layer = {}", slice.get_max_layer()));
            }
            _ => unimplemented!(),
        };
    });
}
