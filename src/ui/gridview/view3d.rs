use std::rc::Rc;

use super::GridViewTrait;
use crate::automaton::*;
use crate::ui::history::HistoryManager;

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
    fn do_frame(&mut self) {
        unimplemented!()
    }

    fn is_running(&self) -> bool {
        unimplemented!()
    }
    fn start_running(&mut self) {
        unimplemented!()
    }
    fn stop_running(&mut self) {
        unimplemented!()
    }
    fn get_automaton<'a>(&'a self) -> Automaton<'a> {
        unimplemented!()
    }
    fn get_automaton_mut<'a>(&'a mut self) -> AutomatonMut<'a> {
        unimplemented!()
    }
}

pub struct HistoryEntry {}

impl HistoryManager for GridView3D {
    type HistoryEntry = HistoryEntry;
    fn history_entry(&self) -> HistoryEntry {
        unimplemented!()
    }
    fn restore(&mut self, _entry: HistoryEntry) -> HistoryEntry {
        unimplemented!()
    }
    fn undo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        unimplemented!()
    }
    fn redo_stack(&mut self) -> &mut Vec<HistoryEntry> {
        unimplemented!()
    }
}
