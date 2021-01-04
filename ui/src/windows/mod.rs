use imgui::*;
use std::time::Duration;

use ndcell_core::prelude::*;
use Axis::{X, Y, Z};

mod simulation;

use crate::commands::Command;
use crate::gridview::*;
use crate::mouse::MouseState;
use crate::CONFIG;
use simulation::SimulationWindow;

const RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];
const YELLOW: [f32; 4] = [1.0, 1.0, 0.0, 1.0];
const GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
const BLUE: [f32; 4] = [0.0, 0.5, 1.0, 1.0];

pub struct BuildParams<'a> {
    pub ui: &'a imgui::Ui<'a>,
    pub mouse: MouseState,
    pub gridview: &'a mut GridView,
}

#[derive(Debug, Default)]
pub struct MainWindow {
    simulation: SimulationWindow,
}
impl MainWindow {
    /// Builds the main window.
    pub fn build(&mut self, params: &mut BuildParams<'_>) {
        let BuildParams {
            ui,
            mouse,
            gridview,
        } = params;

        Window::new(&ImString::new(crate::TITLE)).build(&ui, || {
            let config = CONFIG.lock();

            ui.text(format!("NDCell v{}", env!("CARGO_PKG_VERSION")));
            ui.text("");
            let fps = ui.io().framerate.ceil() as usize;
            ui.text_colored(fps_color(fps), format!("Framerate = {} FPS", fps));
            let total_sim_time: Duration = gridview
                .last_sim_times()
                .iter()
                .map(|duration| duration)
                .sum();
            if total_sim_time == Duration::default() {
                // .is_zero()
                ui.text("");
            } else {
                let avg_sim_time = total_sim_time / gridview.last_sim_times().len() as u32;
                ui.text_colored(
                    fps_color((1.0 / avg_sim_time.as_secs_f64()) as usize),
                    format!("Avg. sim time = {:.1?}", avg_sim_time),
                );
            }
            if gridview.is_drawing() {
                ui.text_colored(BLUE, "DRAWING");
            } else {
                match gridview.work_type() {
                    Some(WorkType::SimStep) => ui.text_colored(YELLOW, "STEPPING"),
                    Some(WorkType::SimContinuous) => ui.text_colored(GREEN, "RUNNING"),
                    None => ui.text(""),
                }
            }
            ui.text("");
            ui.text(format!("Generations = {}", gridview.generation_count()));
            ui.text(format!("Population = {}", gridview.population()));
            ui.text("");
            {
                let new_ndim = match gridview.ndim() {
                    2 => 3,
                    _ => 2,
                };
                if ui.button(
                    &ImString::new(format!("Switch to {}D", new_ndim)),
                    [ui.window_content_region_width(), 30.0],
                ) {
                    **gridview = crate::make_default_gridview(new_ndim);
                }
            }
            ui.text("");
            match &gridview {
                GridView::View2D(view2d) => {
                    let cam = view2d.camera();
                    ui.text("2D");
                    ui.text(format!("Scale = {}", cam.scale()));
                    for &ax in Dim2D::axes() {
                        let value = &cam.pos()[ax];
                        if format!("{:.1}", value).ends_with("0") {
                            ui.text(format!("{} = {:.0}", ax.name(), value));
                        } else {
                            ui.text(format!("{} = {:.1}", ax.name(), value));
                        }
                    }
                    if let Some(mouse_pos) = view2d.camera().try_pixel_to_screen_pos(mouse.pos) {
                        ui.text(format!(
                            "Cursor: X = {}, Y = {}",
                            mouse_pos.int_cell()[X],
                            mouse_pos.int_cell()[Y],
                        ));
                    } else {
                        ui.text("");
                    }
                }
                GridView::View3D(view3d) => {
                    let cam = view3d.camera();
                    ui.text("3D");
                    ui.text(format!("Scale = {}", cam.scale()));
                    for &ax in Dim3D::axes() {
                        let value = &cam.pos()[ax];
                        if format!("{:.1}", value).ends_with("0") {
                            ui.text(format!("{} = {:.0}", ax.name(), value));
                        } else {
                            ui.text(format!("{} = {:.1}", ax.name(), value));
                        }
                    }
                    ui.text(format!("Pitch = {:.2?}°", cam.pitch().0));
                    ui.text(format!("Yaw = {:.2?}°", cam.yaw().0));
                    if let Some(hover_pos) = view3d.hovered_cell_pos(mouse.pos) {
                        ui.text(format!(
                            "Cursor: X = {}, Y = {}, Z = {}",
                            hover_pos[X].floor().0,
                            hover_pos[Y].floor().0,
                            hover_pos[Z].floor().0,
                        ));
                    } else {
                        ui.text("");
                    }
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
            ui.text(format!(
                "Selected cell state: {}",
                gridview.selected_cell_state(),
            ));
            ui.text("");
            ui.checkbox(im_str!("Simulation"), &mut self.simulation.is_visible);
        });

        self.simulation.build(params);
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
