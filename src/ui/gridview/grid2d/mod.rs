mod render;
mod shaders;
mod viewport;
mod zoom;

use super::GridViewTrait;
use crate::automaton::NdProjectedAutomatonTrait;
use crate::automaton::*;
use render::RenderCache;
use std::rc::Rc;
pub use viewport::Viewport2D;
pub use zoom::Zoom2D;

#[derive(Default)]
pub struct GridView2D {
    pub automaton: ProjectedAutomaton<Dim2D>,
    pub viewport: Viewport2D,
    render_cache: Option<RenderCache>,
}
impl From<ProjectedAutomaton<Dim2D>> for GridView2D {
    fn from(automaton: ProjectedAutomaton<Dim2D>) -> Self {
        Self {
            automaton,
            ..Default::default()
        }
    }
}

impl GridViewTrait for GridView2D {
    fn draw(&mut self, display: &Rc<glium::Display>, target: &mut glium::Frame) {
        if let None = self.render_cache {
            self.render_cache = Some(RenderCache::new(display.clone()));
        }
        self.draw_internal(target);
    }
    fn get_population(&self) -> usize {
        self.automaton.get_population()
    }
    fn get_generation_count(&self) -> usize {
        self.automaton.get_generation_count()
    }
}
