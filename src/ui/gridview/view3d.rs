use std::rc::Rc;

use super::GridViewTrait;
use crate::automaton::*;

#[derive(Clone)]
pub struct GridView3D {}

impl GridView3D {
    pub fn new(_display: Rc<glium::Display>, _automaton: ProjectedAutomaton<Dim3D>) -> Self {
        unimplemented!()
    }
    pub fn default(display: Rc<glium::Display>) -> Self {
        Self::new(display, ProjectedAutomaton::default())
    }
}

impl IntoNdSimulate for GridView3D {
    fn ndsim(&self) -> &dyn NdSimulate {
        unimplemented!()
    }
    fn ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        unimplemented!()
    }
}

impl GridViewTrait for GridView3D {
    fn draw(
        &mut self,
        _target: &mut glium::Frame,
        _cursor_position: Option<(i32, i32)>,
    ) -> Option<AnyDimBigVec> {
        unimplemented!()
    }
    fn do_frame(&mut self) {
        unimplemented!()
    }
    fn get_automaton<'a>(&'a self) -> Automaton<'a> {
        unimplemented!()
    }
    fn get_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        unimplemented!()
    }
}
