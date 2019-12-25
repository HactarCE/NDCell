//! The simulation backend.
//!
//! This module contains everything needed to store and simulate cellular
//! automata.

use enum_dispatch::enum_dispatch;
use std::convert::TryInto;
use std::marker::PhantomData;

pub mod projection;
pub mod rule;
pub mod simulation;
pub mod space;

pub use projection::*;
pub use rule::{DummyRule, Rule};
pub use simulation::*;
pub use space::*;

#[enum_dispatch]
pub trait NdProjectedAutomatonTrait<P: Dim> {
    fn get_ndim(&self) -> usize;
    fn set_step_size(&mut self, step_size: usize);
    fn get_step_size(&self) -> usize;
    fn step(&mut self);
    fn step_single(&mut self);
    fn get_projected_tree(&self) -> NdTree<u8, P>;
    fn get_projection_params(&self) -> ProjectionParams;
    fn set_projection_params(&mut self, params: ProjectionParams) -> Result<(), NdProjectionError>;
    fn get_population(&self) -> usize;
    fn get_generation_count(&self) -> usize;
}

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
impl<P: Dim> Default for ProjectedAutomaton<P> {
    fn default() -> Self {
        let inner = Box::new(NdProjectedAutomaton::<P, P>::default());
        match P::NDIM {
            1 => Self::From1D(unsafe {
                *std::mem::transmute::<
                    Box<NdProjectedAutomaton<P, P>>,
                    Box<NdProjectedAutomaton<Dim1D, P>>,
                >(inner)
            }),
            2 => Self::From2D(unsafe {
                *std::mem::transmute::<
                    Box<NdProjectedAutomaton<P, P>>,
                    Box<NdProjectedAutomaton<Dim2D, P>>,
                >(inner)
            }),
            3 => Self::From3D(unsafe {
                *std::mem::transmute::<
                    Box<NdProjectedAutomaton<P, P>>,
                    Box<NdProjectedAutomaton<Dim3D, P>>,
                >(inner)
            }),
            4 => Self::From4D(unsafe {
                *std::mem::transmute::<
                    Box<NdProjectedAutomaton<P, P>>,
                    Box<NdProjectedAutomaton<Dim4D, P>>,
                >(inner)
            }),
            5 => Self::From5D(unsafe {
                *std::mem::transmute::<
                    Box<NdProjectedAutomaton<P, P>>,
                    Box<NdProjectedAutomaton<Dim5D, P>>,
                >(inner)
            }),
            6 => Self::From6D(unsafe {
                *std::mem::transmute::<
                    Box<NdProjectedAutomaton<P, P>>,
                    Box<NdProjectedAutomaton<Dim6D, P>>,
                >(inner)
            }),
            _ => unreachable!("Dimensions above 6 are not supported"),
        }
    }
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

#[derive(Clone)]
pub struct NdProjectedAutomaton<D: Dim, P: Dim> {
    pub automaton: NdAutomaton<D>,
    pub projection: NdProjection<u8, D, P>,
}
impl<D: Dim> From<NdAutomaton<D>> for NdProjectedAutomaton<D, D> {
    fn from(automaton: NdAutomaton<D>) -> Self {
        Self {
            automaton,
            projection: Default::default(),
        }
    }
}
impl<D: Dim> Default for NdProjectedAutomaton<D, D> {
    fn default() -> Self {
        Self::from(NdAutomaton::default())
    }
}
impl<D: Dim, P: Dim> NdProjectedAutomatonTrait<P> for NdProjectedAutomaton<D, P> {
    fn get_ndim(&self) -> usize {
        D::NDIM
    }
    fn set_step_size(&mut self, step_size: usize) {
        self.automaton.sim.set_step_size(step_size);
    }
    fn get_step_size(&self) -> usize {
        self.automaton.sim.get_step_size()
    }
    fn step(&mut self) {
        self.automaton.sim.step(&mut self.automaton.tree);
        self.automaton.generations += self.automaton.sim.get_step_size();
    }
    fn step_single(&mut self) {
        self.automaton.sim.step_single(&mut self.automaton.tree);
        self.automaton.generations += 1;
    }
    fn get_projected_tree(&self) -> NdTree<u8, P> {
        self.projection.project(&self.automaton.tree)
    }
    fn get_projection_params(&self) -> ProjectionParams {
        self.projection.get_params()
    }
    fn set_projection_params(&mut self, params: ProjectionParams) -> Result<(), NdProjectionError> {
        self.projection = NdProjection(params.try_into()?);
        Ok(())
    }
    fn get_population(&self) -> usize {
        self.automaton.tree.get_root().population
    }
    fn get_generation_count(&self) -> usize {
        self.automaton.generations
    }
}

#[derive(Clone, Default)]
pub struct NdAutomaton<D: Dim> {
    pub tree: NdTree<u8, D>,
    pub sim: Simulation<u8, D>,
    pub generations: usize,
}

#[cfg(test)]
mod tests;
