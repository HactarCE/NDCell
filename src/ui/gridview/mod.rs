use enum_dispatch::enum_dispatch;
use std::rc::Rc;

mod grid2d;
mod grid3d;

use crate::automaton::{Dim, Dim2D, Dim3D, NdProjectedAutomaton, ProjectedAutomaton};
pub use grid2d::{GridView2D, Viewport2D};
pub use grid3d::GridView3D;

/// The trait implemented by GridView by dispatching to the implementation of
/// the GridView2D or GridView3D within.
#[enum_dispatch]
pub trait GridViewTrait {
    fn draw(&mut self, display: &Rc<glium::Display>, target: &mut glium::Frame);
    fn get_population(&self) -> usize;
    fn get_generation_count(&self) -> usize;
}

/// An enum between 2D and 3D views that manages the automaton.
#[enum_dispatch(GridViewTrait)]
pub enum GridView {
    View2D(GridView2D),
    View3D(GridView3D),
}

/// Conversions from a ProjectedAutomaton to a GridView.
impl From<ProjectedAutomaton<Dim2D>> for GridView {
    fn from(automaton: ProjectedAutomaton<Dim2D>) -> Self {
        Self::View2D(GridView2D::from(automaton))
    }
}
impl From<ProjectedAutomaton<Dim3D>> for GridView {
    fn from(automaton: ProjectedAutomaton<Dim3D>) -> Self {
        Self::View3D(GridView3D::from(automaton))
    }
}

/// Conversions from an NdProjectedAutomaton to a GridView.
impl<D: Dim, P: Dim> From<NdProjectedAutomaton<D, P>> for GridView
where
    ProjectedAutomaton<P>: From<NdProjectedAutomaton<D, P>>,
    Self: From<ProjectedAutomaton<P>>,
{
    fn from(automaton: NdProjectedAutomaton<D, P>) -> Self {
        Self::from(ProjectedAutomaton::from(automaton))
    }
}
