use imgui::*;

use crate::automaton::{AnyDimBigVec, AsFVec, Dim, Dim2D, NdSimulate, X, Y};
use crate::ui::gridview::{GridView, GridView2D, Viewport2D};
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
                viewport: Viewport2D { pos, offset, zoom },
                ..
            }) => {
                ui.text(format!("Zoom = {}", zoom));
                let total_pos = pos.as_fvec() + offset.clone();
                for &ax in Dim2D::axes() {
                    let value = total_pos[ax];
                    if format!("{:.1}", value).ends_with("0") {
                        ui.text(format!("{} = {:.0}", ax.name(), value));
                    } else {
                        ui.text(format!("{} = {:.1}", ax.name(), value));
                    }
                }
                if let Some(AnyDimBigVec::Vec2D(hover_pos)) = &state.input_state.hovered_cell {
                    ui.text(format!(
                        "Selected: X = {}, Y = {}",
                        hover_pos[X], hover_pos[Y]
                    ));
                } else {
                    ui.text("");
                }
            }
            _ => unimplemented!(),
        };
        ui.checkbox(im_str!("Simulation"), &mut state.gui.simulation.visible);
    });
}
