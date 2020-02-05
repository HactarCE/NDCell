use imgui::*;
use num::{BigInt, ToPrimitive};
use ref_thread_local::RefThreadLocal;

use crate::automaton::NdSimulate;
use crate::ui::{gridview_mut, GridViewTrait, History};

ref_thread_local! {
    pub static managed IS_VISIBLE: bool = false;
    pub static managed STEP_SIZE: BigInt = 4.into();
    pub static managed USE_BREAKPOINT: bool = false;
    pub static managed BREAKPOINT_GEN: BigInt = 0.into();
}

/// Builds the main window.
pub fn build(ui: &imgui::Ui) {
    if *IS_VISIBLE.borrow() {
        let mut gridview = gridview_mut();
        Window::new(&ImString::new("Simulation")).build(&ui, || {
            let mut width = ui.window_content_region_width();
            if width < 100.0 {
                width = 200.0;
            }
            if ui.button(im_str!("Step 1 generation"), [width, 40.0]) {
                gridview_mut().step_n(&1.into());
            }
            ui.spacing();
            ui.spacing();
            ui.separator();
            ui.spacing();
            ui.spacing();
            // TODO: implement custom BigInt-compatible number entry widget
            let mut step_size_big = STEP_SIZE.borrow_mut();
            let old_step_size_i32 = step_size_big.to_i32().unwrap_or(std::i32::MAX);
            let mut step_size_i32 = old_step_size_i32;
            ui.input_int(im_str!("Sim step"), &mut step_size_i32)
                .step(16)
                .step_fast(256)
                .build();
            if step_size_i32 <= 0 {
                step_size_i32 = 1;
            }
            if old_step_size_i32 != step_size_i32 {
                *step_size_big = step_size_i32.into();
            }
            if ui.button(
                &ImString::new(format!("Step {} generations", *step_size_big)),
                [width, 40.0],
            ) {
                gridview_mut().step();
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
                    gridview.toggle_running();
                }
            }
            ui.spacing();
            ui.spacing();
            ui.separator();
            ui.spacing();
            ui.spacing();
            ui.checkbox(im_str!("Breakpoint"), &mut USE_BREAKPOINT.borrow_mut());
            if *USE_BREAKPOINT.borrow() {
                let mut breakpoint_gen = BREAKPOINT_GEN.borrow_mut();
                let old_breakpoint_gen_i32 = breakpoint_gen.to_i32().unwrap_or(std::i32::MAX);
                let mut breakpoint_gen_i32 = old_breakpoint_gen_i32;
                ui.input_int(im_str!(""), &mut breakpoint_gen_i32)
                    .step(16)
                    .step_fast(256)
                    .build();
                if old_breakpoint_gen_i32 != breakpoint_gen_i32 {
                    *breakpoint_gen = breakpoint_gen_i32.into();
                }
                if &*breakpoint_gen < gridview.get_generation_count() {
                    *breakpoint_gen = gridview.get_generation_count().clone()
                }
            }
            ui.spacing();
            ui.spacing();
            ui.separator();
            ui.spacing();
            ui.spacing();
            let button_width = (width - 20.0) / 2.0;
            if ui.button(im_str!("Undo"), [button_width, 60.0]) {
                gridview.undo();
            }
            ui.same_line(button_width + 20.0);
            if ui.button(im_str!("Redo"), [button_width, 60.0]) {
                gridview.redo();
            }
            ui.spacing();
            ui.spacing();
            if ui.button(im_str!("Reset"), [width, 40.0]) {
                gridview.stop_running();
                gridview.undo_to_gen(&0.into());
            }
        })
    }
}
