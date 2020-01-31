use std::cell::RefCell;
use std::rc::Rc;

mod render;
mod shaders;
mod viewport;
mod zoom;

use super::GridViewTrait;
use crate::automaton::*;
pub use viewport::Viewport2D;
pub use zoom::Zoom2D;

#[derive(Clone)]
pub struct GridView2D {
    /// Automaton being simulated and displayed.
    pub automaton: ProjectedAutomaton<Dim2D>,
    /// Target viewport.
    pub viewport: Viewport2D,
    /// Viewport that interpolates to the target and is used for drawing.
    pub interpolating_viewport: Viewport2D,
    render_cache: Rc<RefCell<render::RenderCache>>,
}

impl GridViewTrait for GridView2D {
    fn draw(
        &mut self,
        target: &mut glium::Frame,
        cursor_position: Option<(i32, i32)>,
    ) -> Option<AnyDimBigVec> {
        render::draw(self, target, cursor_position).map(AnyDimVec::from)
    }
    fn do_frame(&mut self) {
        const DECAY_CONSTANT: f64 = 4.0;
        if self.interpolating_viewport != self.viewport {
            self.interpolating_viewport = Viewport2D::interpolate(
                &self.interpolating_viewport,
                &self.viewport,
                1.0 / DECAY_CONSTANT,
            );
        }
    }
    fn get_automaton<'a>(&'a self) -> Automaton<'a> {
        Automaton::from(&self.automaton)
    }
    fn get_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        AutomatonMut::from(&mut self.automaton)
    }
}

impl IntoNdSimulate for GridView2D {
    fn ndsim(&self) -> &dyn NdSimulate {
        &self.automaton
    }
    fn ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        &mut self.automaton
    }
}

impl GridView2D {
    pub fn new(display: Rc<glium::Display>, automaton: ProjectedAutomaton<Dim2D>) -> Self {
        Self {
            automaton: ProjectedAutomaton::from(automaton),
            viewport: Default::default(),
            interpolating_viewport: Default::default(),
            render_cache: Rc::new(RefCell::new(render::RenderCache::new(display))),
        }
    }
    pub fn default(display: Rc<glium::Display>) -> Self {
        Self::new(display, ProjectedAutomaton::default())
    }
    pub fn use_viewport_from(&mut self, other: &Self) {
        self.viewport = other.viewport.clone();
        self.interpolating_viewport = other.interpolating_viewport.clone();
    }
    pub fn get_cell(&mut self, pos: &BigVec2D) -> u8 {
        self.automaton.get_projected_tree().get_cell(pos)
    }
    pub fn set_cell(&mut self, pos: &BigVec2D, new_cell_state: u8) {
        self.automaton.set_cell(pos, new_cell_state)
    }
}
