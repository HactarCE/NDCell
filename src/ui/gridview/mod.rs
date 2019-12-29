use enum_dispatch::enum_dispatch;
use std::rc::Rc;

mod view2d;
mod view3d;

use crate::automaton::*;
pub use view2d::{GridView2D, Viewport2D, Zoom2D};
pub use view3d::GridView3D;

/// The trait implemented by GridView by dispatching to the implementation of
/// the GridView2D or GridView3D within.
#[enum_dispatch]
pub trait GridViewTrait: Clone + NdSimulate {
    fn draw(&mut self, target: &mut glium::Frame);
    fn do_frame(&mut self);
}

/// An enum between 2D and 3D views that manages the automaton.
#[enum_dispatch(GridViewTrait)]
#[derive(Clone)]
pub enum GridView {
    View2D(GridView2D),
    View3D(GridView3D),
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

impl IntoNdSimulate for GridView {
    fn into(&self) -> &dyn NdSimulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
    fn into_mut(&mut self) -> &mut dyn NdSimulate {
        match self {
            Self::View2D(view2d) => view2d,
            Self::View3D(view3d) => view3d,
        }
    }
}

impl GridView {
    pub fn new_2d<T>(display: Rc<glium::Display>, automaton: T) -> Self
    where
        ProjectedAutomaton<Dim2D>: From<T>,
    {
        Self::View2D(GridView2D::new(
            display,
            ProjectedAutomaton::from(automaton),
        ))
    }
    pub fn new_3d<T>(display: Rc<glium::Display>, automaton: T) -> Self
    where
        ProjectedAutomaton<Dim3D>: From<T>,
    {
        Self::View3D(GridView3D::new(
            display,
            ProjectedAutomaton::from(automaton),
        ))
    }
}
