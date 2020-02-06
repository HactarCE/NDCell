use imgui::*;
use ref_thread_local::RefThreadLocal;

use crate::automaton::{AsFVec, Dim, Dim2D, NdSimulate, X, Y};
use crate::ui::gridview::*;

/// Builds the main window.
pub fn build(ui: &imgui::Ui) {
    let gridview = &*crate::ui::gridview();
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
                if let Some(hover_pos) = &view2d.get_render_result().hover_pos {
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
        ui.checkbox(
            im_str!("Simulation"),
            &mut super::simulation::IS_VISIBLE.borrow_mut(),
        );
    });
}
