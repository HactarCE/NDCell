use imgui::*;

mod simulation;

use crate::automaton::{AsFVec, Dim, Dim2D, NdSimulate, X, Y};
use crate::ui::config::*;
use crate::ui::gridview::*;
use simulation::SimulationWindow;

#[derive(Debug, Default)]
pub struct MainWindow {
    simulation: SimulationWindow,
}
impl MainWindow {
    /// Builds the main window.
    pub fn build(&mut self, ui: &imgui::Ui, config: &mut Config, gridview: &GridView) {
        Window::new(&ImString::new(crate::ui::TITLE)).build(&ui, || {
            ui.text("Hello, world!");
            ui.text(format!("Framerate = {} FPS", ui.io().framerate as usize));
            ui.text(format!("Generations = {}", gridview.get_generation_count()));
            ui.text(format!("Population = {}", gridview.get_population()));
            match &gridview {
                GridView::View2D(view2d) => {
                    let Viewport2D { pos, offset, zoom } = &view2d.viewport;
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
                    if let Some(hover_pos) = view2d.get_render_result(0).hover_pos.as_ref() {
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
            ui.checkbox(im_str!("Simulation"), &mut self.simulation.is_visible);
        });
        self.simulation.build(ui, config, gridview)
    }
}
