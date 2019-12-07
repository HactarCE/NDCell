use imgui::*;

use crate::automaton::projection::*;
use crate::automaton::space::Axis;
use crate::ui::render::{AutomatonView2D, GridView, Viewport2D};
use crate::ui::State;

#[derive(Default)]
pub struct WindowState {}

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
        ui.text(im_str!("Hello, world!"));
        match &state.grid_view {
            GridView::Grid2D(AutomatonView2D {
                automaton,
                viewport:
                    Viewport2D {
                        pos,
                        x_offset,
                        y_offset,
                        zoom,
                    },
                ..
            }) => {
                ui.text(format!("Framerate = {} FPS", ui.io().framerate as usize));
                ui.text(format!("X = {}", pos[Axis::X] as f32 + x_offset));
                ui.text(format!("Y = {}", pos[Axis::Y] as f32 + y_offset));
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
        ui.checkbox(im_str!("Simulation"), &mut state.gui.simulation.visible);
    });
}
