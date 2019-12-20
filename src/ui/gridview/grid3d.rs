use std::rc::Rc;

use super::GridViewTrait;
use crate::automaton::*;

pub struct GridView3D {}
impl From<ProjectedAutomaton<Dim3D>> for GridView3D {
    fn from(automaton: ProjectedAutomaton<Dim3D>) -> Self {
        unimplemented!()
    }
}

impl GridViewTrait for GridView3D {
    fn draw(&mut self, display: &Rc<glium::Display>, target: &mut glium::Frame) {
        unimplemented!()
    }
    fn get_population(&self) -> usize {
        unimplemented!()
    }
    fn get_generation_count(&self) -> usize {
        unimplemented!()
    }
}
