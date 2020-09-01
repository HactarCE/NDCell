use enum_dispatch::enum_dispatch;
use std::convert::TryInto;
use std::sync::{Arc, Mutex};

use crate::dim::*;
use crate::ndtree::NdTree;
use crate::ndvec::BigVec;
use crate::num::{BigInt, BigUint};
use crate::projection::{NdProjection, NdProjectionError, NdProjector, ProjectionParams};
use crate::sim::{AsNdSimulate, NdSimulate, Simulation};

/// ProjectedAutomaton functionality implemented by dispatching to
/// NdProjectedAutomaton.
#[enum_dispatch]
pub trait NdProjectedAutomatonTrait<P: Dim> {
    /// Returns the projected NdTree.
    fn projected_tree(&self) -> NdTree<P>;
    /// Returns the ProjectionParams used to create this projection.
    fn projection_params(&self) -> ProjectionParams;
    /// Sets the projection from the ProjectionParams.
    fn set_projection_params(&mut self, params: ProjectionParams) -> Result<(), NdProjectionError>;
    /// Sets a cell using projected coordinates.
    fn set_cell(&mut self, pos: &BigVec<P>, state: u8);
}

/// A cellular automaton projected to 1D.
pub type ProjectedAutomaton1D = ProjectedAutomaton<Dim1D>;
/// A cellular automaton projected to 2D.
pub type ProjectedAutomaton2D = ProjectedAutomaton<Dim2D>;
/// A cellular automaton projected to 3D.
pub type ProjectedAutomaton3D = ProjectedAutomaton<Dim3D>;
/// A cellular automaton projected to 4D.
pub type ProjectedAutomaton4D = ProjectedAutomaton<Dim4D>;
/// A cellular automaton projected to 5D.
pub type ProjectedAutomaton5D = ProjectedAutomaton<Dim5D>;
/// A cellular automaton projected to 6D.
pub type ProjectedAutomaton6D = ProjectedAutomaton<Dim6D>;

/// An automaton of an unknown dimensionality combined with a projection to a
/// given dimensionality.
#[allow(missing_docs)]
#[enum_dispatch(NdProjectedAutomatonTrait)]
#[derive(Clone)]
pub enum ProjectedAutomaton<P: Dim> {
    From1D(NdProjectedAutomaton<Dim1D, P>),
    From2D(NdProjectedAutomaton<Dim2D, P>),
    From3D(NdProjectedAutomaton<Dim3D, P>),
    From4D(NdProjectedAutomaton<Dim4D, P>),
    From5D(NdProjectedAutomaton<Dim5D, P>),
    From6D(NdProjectedAutomaton<Dim6D, P>),
}
impl<D: Dim, P: Dim> From<NdAutomaton<D>> for ProjectedAutomaton<P>
where
    NdProjectedAutomaton<D, P>: From<NdAutomaton<D>>,
    Self: From<NdProjectedAutomaton<D, P>>,
{
    fn from(automaton: NdAutomaton<D>) -> Self {
        Self::from(NdProjectedAutomaton::from(automaton))
    }
}
impl<P: Dim> AsNdSimulate for ProjectedAutomaton<P> {
    fn as_ndsim(&self) -> &dyn NdSimulate {
        match self {
            Self::From1D(inner) => inner,
            Self::From2D(inner) => inner,
            Self::From3D(inner) => inner,
            Self::From4D(inner) => inner,
            Self::From5D(inner) => inner,
            Self::From6D(inner) => inner,
        }
    }
    fn as_ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        match self {
            Self::From1D(inner) => inner,
            Self::From2D(inner) => inner,
            Self::From3D(inner) => inner,
            Self::From4D(inner) => inner,
            Self::From5D(inner) => inner,
            Self::From6D(inner) => inner,
        }
    }
}

/// A D-dimensional automaton with a projection from a D-dimensional grid to a
/// P-dimensional one.
#[allow(missing_docs)]
#[derive(Clone)]
pub struct NdProjectedAutomaton<D: Dim, P: Dim> {
    pub automaton: NdAutomaton<D>,
    pub projection: NdProjection<D, P>,
}
impl<D: Dim> From<NdAutomaton<D>> for NdProjectedAutomaton<D, D> {
    fn from(automaton: NdAutomaton<D>) -> Self {
        Self {
            automaton,
            projection: Default::default(),
        }
    }
}
impl<D: Dim, P: Dim> AsNdSimulate for NdProjectedAutomaton<D, P> {
    fn as_ndsim(&self) -> &dyn NdSimulate {
        &self.automaton
    }
    fn as_ndsim_mut(&mut self) -> &mut dyn NdSimulate {
        &mut self.automaton
    }
}
impl<D: Dim, P: Dim> NdProjectedAutomatonTrait<P> for NdProjectedAutomaton<D, P> {
    fn projected_tree(&self) -> NdTree<P> {
        self.projection.project_tree(&self.automaton.tree)
    }
    fn projection_params(&self) -> ProjectionParams {
        self.projection.params()
    }
    fn set_projection_params(&mut self, params: ProjectionParams) -> Result<(), NdProjectionError> {
        self.projection = NdProjection(params.try_into()?);
        Ok(())
    }
    fn set_cell(&mut self, pos: &BigVec<P>, state: u8) {
        self.automaton.tree.set_cell(
            todo!("cache access"),
            &self.projection.unproject_pos(pos),
            state,
        );
    }
}

/// A fully-fledged cellular automaton, including a grid (NdTree), rule
/// (Simulation), and generation count.
#[allow(missing_docs)]
#[derive(Clone)]
pub struct NdAutomaton<D: Dim> {
    pub tree: NdTree<D>,
    pub sim: Arc<Mutex<Simulation<D>>>,
    pub generations: BigInt,
}
impl<D: Dim> NdSimulate for NdAutomaton<D> {
    fn ndim(&self) -> usize {
        D::NDIM
    }
    fn population(&self) -> &BigUint {
        self.tree.root.population()
    }
    fn generation_count(&self) -> &BigInt {
        &self.generations
    }
    fn set_generation_count(&mut self, generations: BigInt) {
        self.generations = generations;
    }
    fn step(&mut self, step_size: &BigInt) {
        self.sim.lock().unwrap().step(&mut self.tree, step_size);
        self.generations += step_size;
    }
}
impl<D: Dim> NdAutomaton<D> {
    /// Creates a new automaton with the given number of states.
    pub fn with_state_count(state_count: usize) -> Self {
        Self {
            tree: NdTree::with_state_count(state_count),
            sim: Default::default(),
            generations: Default::default(),
        }
    }
    /// Sets the simulation of the automaton.
    pub fn set_sim(&mut self, new_sim: Simulation<D>) {
        self.sim = Arc::new(Mutex::new(new_sim));
    }
}

/// A 1D cellular automaton.
pub type Automaton1D = NdAutomaton<Dim1D>;
/// A 2D cellular automaton.
pub type Automaton2D = NdAutomaton<Dim2D>;
/// A 3D cellular automaton.
pub type Automaton3D = NdAutomaton<Dim3D>;
/// A 4D cellular automaton.
pub type Automaton4D = NdAutomaton<Dim4D>;
/// A 5D cellular automaton.
pub type Automaton5D = NdAutomaton<Dim5D>;
/// A 6D cellular automaton.
pub type Automaton6D = NdAutomaton<Dim6D>;

/// An immutable reference to a cellular automaton of an unknown dimensionality.
#[allow(missing_docs)]
pub enum Automaton<'a> {
    Automaton1D(&'a Automaton1D),
    Automaton2D(&'a Automaton2D),
    Automaton3D(&'a Automaton3D),
    Automaton4D(&'a Automaton4D),
    Automaton5D(&'a Automaton5D),
    Automaton6D(&'a Automaton6D),
}
impl<'a, P: Dim> From<&'a ProjectedAutomaton<P>> for Automaton<'a> {
    fn from(projected_automaton: &'a ProjectedAutomaton<P>) -> Self {
        match projected_automaton {
            ProjectedAutomaton::From1D(inner) => Self::Automaton1D(&inner.automaton),
            ProjectedAutomaton::From2D(inner) => Self::Automaton2D(&inner.automaton),
            ProjectedAutomaton::From3D(inner) => Self::Automaton3D(&inner.automaton),
            ProjectedAutomaton::From4D(inner) => Self::Automaton4D(&inner.automaton),
            ProjectedAutomaton::From5D(inner) => Self::Automaton5D(&inner.automaton),
            ProjectedAutomaton::From6D(inner) => Self::Automaton6D(&inner.automaton),
        }
    }
}

/// A mutable reference to a cellular automaton of an unknown dimensionality.
#[allow(missing_docs)]
pub enum AutomatonMut<'a> {
    Automaton1D(&'a mut Automaton1D),
    Automaton2D(&'a mut Automaton2D),
    Automaton3D(&'a mut Automaton3D),
    Automaton4D(&'a mut Automaton4D),
    Automaton5D(&'a mut Automaton5D),
    Automaton6D(&'a mut Automaton6D),
}
impl<'a, P: Dim> From<&'a mut ProjectedAutomaton<P>> for AutomatonMut<'a> {
    fn from(projected_automaton: &'a mut ProjectedAutomaton<P>) -> Self {
        match projected_automaton {
            ProjectedAutomaton::From1D(inner) => Self::Automaton1D(&mut inner.automaton),
            ProjectedAutomaton::From2D(inner) => Self::Automaton2D(&mut inner.automaton),
            ProjectedAutomaton::From3D(inner) => Self::Automaton3D(&mut inner.automaton),
            ProjectedAutomaton::From4D(inner) => Self::Automaton4D(&mut inner.automaton),
            ProjectedAutomaton::From5D(inner) => Self::Automaton5D(&mut inner.automaton),
            ProjectedAutomaton::From6D(inner) => Self::Automaton6D(&mut inner.automaton),
        }
    }
}