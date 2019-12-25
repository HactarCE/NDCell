use std::rc::Rc;

mod render;
mod shaders;
mod viewport;
mod zoom;

use super::GridViewTrait;
use crate::automaton::NdProjectedAutomatonTrait;
use crate::automaton::*;
pub use viewport::Viewport2D;
pub use zoom::Zoom2D;

pub struct GridView2D {
    pub automaton: ProjectedAutomaton<Dim2D>,
    pub viewport: Viewport2D,
    render_cache: render::RenderCache,
    shaders: render::Shaders,
    vbos: render::VBOs,
    textures: render::Textures,
    display: Rc<glium::Display>,
}
impl GridView2D {
    pub fn new(display: Rc<glium::Display>, automaton: ProjectedAutomaton<Dim2D>) -> Self {
        Self {
            automaton: ProjectedAutomaton::from(automaton),
            viewport: Default::default(),
            render_cache: render::RenderCache::default(),
            shaders: render::Shaders::compile(&display),
            vbos: render::VBOs::new(&display),
            textures: render::Textures::new(&display),
            display,
        }
    }
    pub fn default(display: Rc<glium::Display>) -> Self {
        Self::new(display, ProjectedAutomaton::default())
    }
}

impl GridViewTrait for GridView2D {
    fn draw(&mut self, target: &mut glium::Frame) {
        render::draw(self, target);
    }
    fn get_population(&self) -> usize {
        self.automaton.get_population()
    }
    fn get_generation_count(&self) -> usize {
        self.automaton.get_generation_count()
    }
}
