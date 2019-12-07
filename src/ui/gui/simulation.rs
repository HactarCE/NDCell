use imgui::*;

use crate::ui::State;

#[derive(Default)]
pub struct WindowState {
    pub visible: bool,
    pub running: bool,
}

/// Builds the main window.
pub fn build(state: &mut State, ui: &imgui::Ui) {
    if state.gui.simulation.visible {
        Window::new(&ImString::new("Simulation")).build(&ui, || {
            let mut width = ui.window_content_region_width();
            if width < 100.0 {
                width = 200.0;
            }
            let running = &mut state.gui.simulation.running;
            if ui.button(im_str!("Step 1 generation"), [width, 40.0]) {
                if *running {
                    *running = false;
                } else {
                    state.grid_view.push_to_history();
                    state.grid_view.step_single();
                }
            };
            ui.spacing();
            ui.spacing();
            ui.separator();
            ui.spacing();
            ui.spacing();
            let old_sim_step_size = state.grid_view.get_sim_step_size();
            let mut sim_step_size = old_sim_step_size as i32;
            ui.input_int(im_str!(""), &mut sim_step_size)
                .step(16)
                .step_fast(256)
                .build();
            if sim_step_size <= 0 {
                sim_step_size = 1;
            }
            if old_sim_step_size as i32 != sim_step_size {
                *running = false;
                state.grid_view.set_sim_step_size(sim_step_size as usize);
            }
            if ui.button(
                &ImString::new(format!("Step {} generations", sim_step_size)),
                [width, 40.0],
            ) {
                if *running {
                    *running = false;
                } else {
                    state.grid_view.push_to_history();
                    state.grid_view.step();
                }
            }
            ui.spacing();
            ui.spacing();
            if ui.button(
                if *running {
                    im_str!("Stop")
                } else {
                    im_str!("Start")
                },
                [width, 60.0],
            ) {
                if !*running {
                    state.grid_view.push_to_history();
                }
                *running = !*running;
            }
            ui.spacing();
            ui.spacing();
            ui.separator();
            ui.spacing();
            ui.spacing();
            let button_width = (width - 20.0) / 2.0;
            if ui.button(im_str!("Undo"), [button_width, 60.0]) {
                *running = false;
                state.grid_view.undo();
            }
            ui.same_line(button_width + 20.0);
            if ui.button(im_str!("Redo"), [button_width, 60.0]) {
                *running = false;
                state.grid_view.redo();
            }
            ui.spacing();
            ui.spacing();
            if ui.button(im_str!("Reset"), [width, 40.0]) {
                state.grid_view.reset();
            }

            if *running {
                state.grid_view.step();
            }
        })
    }
}
