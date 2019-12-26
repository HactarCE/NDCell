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
                automaton: _,
                viewport:
                    Viewport2D {
                        pos,
                        x_offset,
                        y_offset,
                        zoom,
                    },
                ..
            }) => {
                // TODO fix formatting to show 3.4 and 3, but not 3.0
                let x = pos[Axis::X] as f32 + x_offset;
                let y = pos[Axis::Y] as f32 + y_offset;
                if format!("{:.1}", x).ends_with("0") {
                    ui.text(format!("X = {:.0}", x))
                } else {
                    ui.text(format!("X = {:.1}", x));
                }
                if format!("{:.1}", y).ends_with("0") {
                    ui.text(format!("Y = {:.0}", y))
                } else {
                    ui.text(format!("Y = {:.1}", y));
                }
                ui.text(format!("Zoom = {}", zoom));
            }
            _ => unimplemented!(),
        };
        ui.checkbox(im_str!("Simulation"), &mut state.gui.simulation.visible);
    });
}
