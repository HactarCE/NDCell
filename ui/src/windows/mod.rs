use imgui::*;

use ndcell_core::axis::{X, Y};
use ndcell_core::prelude::*;

mod simulation;

use crate::config::*;
use crate::gridview::*;
use simulation::SimulationWindow;

const RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];
const YELLOW: [f32; 4] = [1.0, 1.0, 0.0, 1.0];
const GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
const BLUE: [f32; 4] = [0.0, 0.5, 1.0, 1.0];

#[derive(Debug, Default)]
pub struct MainWindow {
    simulation: SimulationWindow,
}
impl MainWindow {
    /// Builds the main window.
    pub fn build(&mut self, ui: &imgui::Ui, config: &mut Config, gridview: &GridView) {
        Window::new(&ImString::new(crate::TITLE)).build(&ui, || {
            ui.text(format!("NDCell v{}", env!("CARGO_PKG_VERSION")));
            ui.text("");
            let fps = ui.io().framerate as usize;
            ui.text_colored(fps_color(fps), format!("Framerate = {} FPS", fps));
            if let GridView::View2D(view2d) = gridview {
                let total_update_ms: f64 = view2d
                    .last_sim_times()
                    .iter()
                    .map(|duration| duration.as_secs_f64())
                    .sum();
                if total_update_ms == 0.0 {
                    ui.text("");
                } else {
                    let avg_update_ms = total_update_ms / view2d.last_sim_times().len() as f64;
                    let ups = (1.0 / avg_update_ms) as usize;
                    ui.text_colored(fps_color(ups), format!("Max sim speed = {} UPS", ups));
                }
                if view2d.is_drawing() {
                    ui.text_colored(BLUE, "DRAWING");
                } else if view2d.is_waiting() || view2d.is_running() {
                    ui.text_colored(
                        if view2d.is_running() {
                            if view2d.is_waiting() {
                                RED
                            } else {
                                GREEN
                            }
                        } else {
                            YELLOW
                        },
                        "SIMULATING",
                    );
                } else {
                    ui.text("");
                }
            }
            ui.text("");
            ui.text(format!("Generations = {}", gridview.generation_count()));
            ui.text(format!("Population = {}", gridview.population()));
            ui.text("");
            match &gridview {
                GridView::View2D(view2d) => {
                    let cam = view2d.camera();
                    ui.text(format!("Scale = {}", cam.scale()));
                    for &ax in Dim2D::axes() {
                        let value = &cam.pos()[ax];
                        if format!("{:.1}", value).ends_with("0") {
                            ui.text(format!("{} = {:.0}", ax.name(), value));
                        } else {
                            ui.text(format!("{} = {:.1}", ax.name(), value));
                        }
                    }
                    if let Some(AnyDimVec::Vec2D(hover_pos)) =
                        view2d.nth_render_result(0).hover_pos.as_ref()
                    {
                        ui.text(format!(
                            "Cursor: X = {}, Y = {}",
                            hover_pos[X].floor().0,
                            hover_pos[Y].floor().0,
                        ));
                    } else {
                        ui.text("");
                    }
                }
                GridView::View3D(_view3d) => {
                    ui.text("Oohh, 3D. Very cool!");
                    ui.text("");
                }
            };
            const MEBIBYTE: usize = 1024 * 1024;
            ui.text(format!(
                "Estimated RAM usage = {}/{} MiB",
                gridview.memory_usage().div_ceil(&MEBIBYTE),
                config.sim.max_memory.div_ceil(&MEBIBYTE),
            ));
            if ui.button(
                im_str!("Trigger garbage collection"),
                [ui.window_content_region_width(), 30.0],
            ) {
                gridview.enqueue(Command::GarbageCollect)
            }
            ui.text("");
            ui.checkbox(im_str!("Simulation"), &mut self.simulation.is_visible);
        });
        self.simulation.build(ui, config, gridview)
    }
}

fn fps_color(fps: usize) -> [f32; 4] {
    if fps >= 58 {
        // Green
        [0.0, 1.0, 0.0, 1.0]
    } else if fps >= 29 {
        // Yellow
        [1.0, 1.0, 0.0, 1.0]
    } else {
        // Red
        [1.0, 0.0, 0.0, 1.0]
    }
}
