use imgui::*;
use std::time::Duration;

use ndcell_core::prelude::*;

#[cfg(debug_assertions)]
mod debug;
pub mod error_popup;
mod setup;
mod simulation;

use crate::commands::Cmd;
use crate::gridview::*;
use crate::mouse::MouseState;
use crate::CONFIG;
#[cfg(debug_assertions)]
use debug::DebugWindow;
use setup::SetupWindow;
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
    #[cfg(debug_assertions)]
    debug: DebugWindow,
    setup: SetupWindow,
    simulation: SimulationWindow,
}
impl MainWindow {
    /// Builds the window.
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

            let width = ui.window_content_region_width();
            let button_width = (width - 10.0) / 2.0;
            if ui.button(im_str!("Load file"), [button_width, 40.0]) {
                if let Ok(response) = nfd2::open_file_dialog(Some("rle,mc"), None) {
                    if let nfd2::Response::Okay(path) = response {
                        let rule = gridview.rule();
                        if let Ok(s) = std::fs::read_to_string(path) {
                            if let Ok(automaton) =
                                ndcell_core::io::import_automaton_from_string(&s, rule)
                            {
                                match automaton.unwrap() {
                                    Automaton::Automaton2D(a) => **gridview = a.into(),
                                    Automaton::Automaton3D(a) => **gridview = a.into(),
                                    _ => (),
                                }
                            }
                        }
                    }
                }
            }
            ui.same_line(button_width + 18.0);
            if ui.button(im_str!("Save file"), [button_width, 40.0]) {
                if let Ok(response) = nfd2::open_save_dialog(Some("rle;mc"), None) {
                    if let nfd2::Response::Okay(path) = response {
                        let ca_format = match path.extension() {
                            Some(ext) if ext == "mc" => CaFormat::Macrocell,
                            _ => CaFormat::Rle,
                        };
                        if let Ok(s) = gridview.export(ca_format) {
                            let _ = std::fs::write(path, s);
                        }
                    }
                }
            }

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
                    let vp = view2d.viewpoint();
                    ui.text("2D");
                    ui.text(format!("Scale = {}", vp.scale()));
                    for &ax in Dim2D::axes() {
                        let value = &vp.center()[ax];
                        if format!("{:.1}", value).ends_with("0") {
                            ui.text(format!("{} = {:.0}", ax.name(), value));
                        } else {
                            ui.text(format!("{} = {:.1}", ax.name(), value));
                        }
                    }
                    if let Some(pixel) = mouse.pos {
                        let cell = view2d.screen_pos(pixel).cell();
                        ui.text(format!("Cursor: {}", cell));
                    } else {
                        ui.text("");
                    }
                }
                GridView::View3D(view3d) => {
                    let vp = view3d.viewpoint();
                    ui.text("3D");
                    ui.text(format!("Scale = {}", vp.scale()));
                    for &ax in Dim3D::axes() {
                        let value = &vp.center()[ax];
                        if format!("{:.1}", value).ends_with("0") {
                            ui.text(format!("{} = {:.0}", ax.name(), value));
                        } else {
                            ui.text(format!("{} = {:.1}", ax.name(), value));
                        }
                    }
                    ui.text(format!("Pitch = {:.2?}°", vp.pitch().0));
                    ui.text(format!("Yaw = {:.2?}°", vp.yaw().0));
                    if let Some(hit) = mouse.pos.and_then(|pixel| view3d.screen_pos(pixel).raycast)
                    {
                        ui.text(format!("Cursor: {} {}", hit.cell, hit.face));
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
                im_str!("Clear cache"),
                [ui.window_content_region_width(), 30.0],
            ) {
                gridview.enqueue(Cmd::ClearCache);
            }
            ui.text("");
            ui.text(format!(
                "Selected cell state: {}",
                gridview.selected_cell_state(),
            ));
            ui.text("");
            ui.checkbox(im_str!("Setup"), &mut self.setup.is_visible);
            ui.checkbox(im_str!("Simulation"), &mut self.simulation.is_visible);

            #[cfg(debug_assertions)]
            ui.checkbox(im_str!("Debug values"), &mut self.debug.is_visible);
        });

        #[cfg(debug_assertions)]
        self.debug.build(params);
        self.setup.build(params);
        self.simulation.build(params);
    }
}

fn fps_color(fps: usize) -> [f32; 4] {
    if fps >= 58 {
        GREEN
    } else if fps >= 29 {
        YELLOW
    } else {
        RED
    }
}
