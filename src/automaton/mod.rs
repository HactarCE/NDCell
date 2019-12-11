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
    pub generations: usize,
    projection: NdProjection<u8, D, P>,
}

trait NdAutomatonTrait<P: Dim> {
    fn get_ndim(&self) -> usize;
    fn set_step_size(&mut self, step_size: usize);
    fn get_step_size(&self) -> usize;
    fn step(&mut self);
    fn step_single(&mut self);
    fn get_projected_tree(&self) -> NdTree<u8, P>;
}
impl<D: Dim, P: Dim> NdAutomatonTrait<P> for NdAutomaton<D, P> {
    fn get_ndim(&self) -> usize {
        D::NDIM
    }
    fn set_step_size(&mut self, step_size: usize) {
        self.sim.borrow_mut().set_step_size(step_size);
    }
    fn get_step_size(&self) -> usize {
        self.sim.borrow().get_step_size()
    }
    fn step(&mut self) {
        let mut sim = self.sim.borrow_mut();
        sim.step(&mut self.tree);
        self.generations += sim.get_step_size();
    }
    fn step_single(&mut self) {
        let mut sim = self.sim.borrow_mut();
        sim.step_single(&mut self.tree);
        self.generations += 1;
    }
    fn get_projected_tree(&self) -> NdTree<u8, P> {
        self.projection.project(&self.tree)
    }
}

#[cfg(test)]
mod tests;
