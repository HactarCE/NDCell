use imgui::*;

use crate::automaton::space::Axis;
use crate::ui::gridview::{GridView, GridView2D, GridViewTrait, Viewport2D};
use crate::ui::State;

#[derive(Default)]
pub struct WindowState {}

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
        ui.text("Hello, world!");
        ui.text(format!("Framerate = {} FPS", ui.io().framerate as usize));
        ui.text(format!(
            "Generations = {}",
            state.grid_view.get_generation_count()
        ));
        ui.text(format!("Population = {}", state.grid_view.get_population()));
        match &state.grid_view {
            GridView::View2D(GridView2D {
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
                ui.text(format!("X = {}", pos[Axis::X] as f32 + x_offset));
                ui.text(format!("Y = {}", pos[Axis::Y] as f32 + y_offset));
                ui.text(format!("Zoom = {}", zoom));
            }
            _ => unimplemented!(),
        };
        ui.checkbox(im_str!("Simulation"), &mut state.gui.simulation.visible);
    });
}
