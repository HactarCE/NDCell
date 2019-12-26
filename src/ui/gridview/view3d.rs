use std::rc::Rc;

use super::GridViewTrait;
use crate::automaton::*;

pub struct GridView3D {}
impl GridView3D {
    pub fn new(display: Rc<glium::Display>, automaton: ProjectedAutomaton<Dim3D>) -> Self {
        unimplemented!()
    }
    pub fn default(display: Rc<glium::Display>) -> Self {
        Self::new(display, ProjectedAutomaton::default())
    }
}

impl GridViewTrait for GridView3D {
    fn draw(&mut self, target: &mut glium::Frame) {
        unimplemented!()
    }
    fn do_frame(&mut self) {
        unimplemented!()
    }
    fn get_population(&self) -> usize {
        unimplemented!()
    }
    fn get_generation_count(&self) -> usize {
        unimplemented!()
    }
}
