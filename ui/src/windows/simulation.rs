use imgui::*;

use ndcell_core::prelude::*;

use crate::commands::*;
use crate::windows::BuildParams;
use crate::CONFIG;

#[derive(Debug, Default)]
pub struct SimulationWindow {
    pub is_visible: bool,
}
impl SimulationWindow {
    /// Builds the window.
    pub fn build(&mut self, params: &mut BuildParams<'_>) {
        let BuildParams {
            ui,
            mouse: _,
            gridview,
        } = params;

        if self.is_visible {
            Window::new(&ImString::new("Simulation")).build(&ui, || {
                let mut config = CONFIG.lock();

                let mut width = ui.window_content_region_width();
                if width < 100.0 {
                    width = 200.0;
                }
                if ui.button(im_str!("Step 1 generation"), [width, 40.0]) {
                    gridview.enqueue(Cmd::Step(1));
                }
                ui.spacing();
                ui.spacing();
                ui.separator();
                ui.spacing();
                ui.spacing();
                // TODO: implement custom BigInt-compatible number entry widget
                let old_step_size_i32 = config.sim.step_size.to_i32().unwrap_or(std::i32::MAX);
                let mut step_size_i32 = old_step_size_i32;
                ui.input_int(im_str!("Sim step"), &mut step_size_i32)
                    .step(16)
                    .step_fast(256)
                    .build();
                if step_size_i32 <= 0 {
                    step_size_i32 = 1;
                }
                if old_step_size_i32 != step_size_i32 {
                    config.sim.step_size = step_size_i32.into();
                }
                if ui.button(
                    &ImString::new(format!("Step {} generations", config.sim.step_size)),
                    [width, 40.0],
                ) {
                    gridview.enqueue(Cmd::StepStepSize);
                }
                ui.spacing();
                ui.spacing();
                {
                    if ui.button(
                        if gridview.is_running() {
                            im_str!("Stop")
                        } else {
                            im_str!("Start")
                        },
                        [width, 60.0],
                    ) {
                        gridview.enqueue(Cmd::ToggleRunning);
                    }
                }
                ui.spacing();
                ui.spacing();
                ui.separator();
                ui.spacing();
                ui.spacing();
                ui.checkbox(im_str!("Breakpoint"), &mut config.sim.use_breakpoint);
                if config.sim.use_breakpoint {
                    let old_breakpoint_gen_i32 =
                        config.sim.breakpoint_gen.to_i32().unwrap_or(std::i32::MAX);
                    let mut breakpoint_gen_i32 = old_breakpoint_gen_i32;
                    ui.input_int(im_str!(""), &mut breakpoint_gen_i32)
                        .step(16)
                        .step_fast(256)
                        .build();
                    if old_breakpoint_gen_i32 != breakpoint_gen_i32 {
                        config.sim.breakpoint_gen = breakpoint_gen_i32.into();
                    }
                    if &config.sim.breakpoint_gen < gridview.generation_count() {
                        config.sim.breakpoint_gen = gridview.generation_count().clone()
                    }
                }
                ui.spacing();
                ui.spacing();
                ui.separator();
                ui.spacing();
                ui.spacing();
                let button_width = (width - 10.0) / 2.0;
                if ui.button(im_str!("Undo"), [button_width, 60.0]) {
                    gridview.enqueue(Cmd::Undo);
                }
                ui.same_line(button_width + 18.0);
                if ui.button(im_str!("Redo"), [button_width, 60.0]) {
                    gridview.enqueue(Cmd::Redo);
                }
                ui.spacing();
                ui.spacing();
                if ui.button(im_str!("Reset"), [width, 40.0]) {
                    gridview.enqueue(Cmd::Reset);
                }
            })
        }
    }
}
