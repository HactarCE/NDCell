use imgui::*;

use crate::ui::gridview::*;
use crate::ui::render::{AutomatonView2D, GridView};
use crate::ui::State;

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
        ui.text(im_str!("Hello, world!"));
        match &state.grid_view {
            GridView::Grid2D(AutomatonView2D {
                x,
                y,
                automaton,
                zoom,
            }) => {
                ui.text(format!("X = {}", x));
                ui.text(format!("Y = {}", y));
                ui.text(format!(
                    "Generations = {}",
                    automaton.get_generation_count()
                ));
                ui.text(format!(
                    "Population = {}",
                    automaton.get_root().get_population()
                ));
                ui.text(format!("Zoom = {}", zoom));
                ui.text(format!(
                    "Max node layer = {}",
                    automaton.get_root().get_layer()
                ));
            }
            _ => unimplemented!(),
        };
    });
}
