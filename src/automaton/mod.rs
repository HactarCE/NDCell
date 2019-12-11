//! The simulation backend.
//!
//! This module contains everything needed to store and simulate cellular
//! automata.

use enum_dispatch::enum_dispatch;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

pub mod projection;
pub mod rule;
pub mod simulation;
pub mod space;

pub use projection::*;
pub use rule::{DummyRule, Rule};
pub use simulation::*;
pub use space::*;

#[enum_dispatch(NdAutomatonTrait)]
#[derive(Clone)]
enum Automaton<P: Dim> {
    Automaton1D(NdAutomaton<Dim1D, P>),
    Automaton2D(NdAutomaton<Dim2D, P>),
    Automaton3D(NdAutomaton<Dim3D, P>),
    Automaton4D(NdAutomaton<Dim4D, P>),
    Automaton5D(NdAutomaton<Dim5D, P>),
    Automaton6D(NdAutomaton<Dim6D, P>),
}

#[derive(Clone)]
struct NdAutomaton<D: Dim, P: Dim> {
    pub tree: NdTree<u8, D>,
    pub sim: Rc<RefCell<Simulation<u8, D>>>,
    projection: NdProjection<u8, D, P>,
}

trait NdAutomatonTrait<P: Dim> {
    fn get_ndim() -> usize;
}
impl<D: Dim, P: Dim> NdAutomatonTrait<P> for NdAutomaton<D, P> {
    fn get_ndim() -> usize {
        D::NDIM
    }
}

#[cfg(test)]
mod tests;
