use enum_dispatch::enum_dispatch;
use num::BigInt;
use ref_thread_local::RefThreadLocal;

mod view2d;
mod view3d;

use crate::automaton::*;
use crate::ui::history::History;
pub use view2d::{GridView2D, Viewport2D, Zoom2D};
pub use view3d::GridView3D;

/// Methods implemented by GridView by dispatching to the implementation of the
/// GridView2D or GridView3D within.
#[enum_dispatch]
pub trait GridViewTrait: NdSimulate + History {
    fn render(&mut self, target: &mut glium::Frame);

    fn do_frame(&mut self);

    fn step_n(&mut self, step_size: &BigInt) {
        self.record();
        NdSimulate::step_forward(self, step_size);
    }
    fn step(&mut self) {
        self.step_n(&crate::ui::gui::simulation::STEP_SIZE.borrow());
    }

    fn do_sim_frame(&mut self) {
        if self.is_running() {
            if *crate::ui::gui::simulation::USE_BREAKPOINT.borrow()
                && self.get_generation_count()
                    >= &crate::ui::gui::simulation::BREAKPOINT_GEN.borrow()
            {
                self.stop_running();
            } else {
                NdSimulate::step_forward(self, &crate::ui::gui::simulation::STEP_SIZE.borrow());
            }
        }
    }
    fn is_running(&self) -> bool;
    fn start_running(&mut self);
    fn stop_running(&mut self);
    fn toggle_running(&mut self) -> bool {
        if self.is_running() {
            self.stop_running();
            false
        } else {
            self.start_running();
            true
        }
    }

    fn undo_to_gen(&mut self, gen: &BigInt) -> usize {
        let mut i = 0;
        while self.get_generation_count() > gen && self.undo() {
            i += 1;
        }
        i
    }

    fn get_automaton<'a>(&'a self) -> Automaton<'a>;
    fn get_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a>;
}

/// An enum between 2D and 3D views that manages the automaton.
#[enum_dispatch(GridViewTrait)]
pub enum GridView {
    View2D(pub GridView2D),
    View3D(pub GridView3D),
}

/// Conversions from an NdAutomaton to a GridView.
impl From<Automaton2D> for GridView {
    fn from(automaton: Automaton2D) -> Self {
        Self::View2D(GridView2D::from(ProjectedAutomaton::from(automaton)))
    }
}

impl IntoNdSimulate for GridView {
    fn ndsim(&self) -> &dyn NdSimulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
    fn ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}

impl History for GridView {
    fn record(&mut self) {
        self.history().record()
    }
    fn has_undo(&mut self) -> bool {
        self.history().has_undo()
    }
    fn has_redo(&mut self) -> bool {
        self.history().has_redo()
    }
    fn undo(&mut self) -> bool {
        self.history().undo()
    }
    fn redo(&mut self) -> bool {
        self.history().redo()
    }
}

impl GridView {
    fn history(&mut self) -> &mut dyn History {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}
