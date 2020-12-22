//! High-level CA interface.

use std::sync::Arc;

use crate::dim::*;
use crate::ndtree::{NdTree, NodeRefTrait};
use crate::num::{BigInt, BigUint, Zero};
use crate::sim::rule::{DummyRule, NdRule};
use crate::sim::{hashlife, AsSimulate, Simulate};

/// 1D cellular automaton.
pub type Automaton1D = NdAutomaton<Dim1D>;
/// 2D cellular automaton.
pub type Automaton2D = NdAutomaton<Dim2D>;
/// 3D cellular automaton.
pub type Automaton3D = NdAutomaton<Dim3D>;
/// 4D cellular automaton.
pub type Automaton4D = NdAutomaton<Dim4D>;
/// 5D cellular automaton.
pub type Automaton5D = NdAutomaton<Dim5D>;
/// 6D cellular automaton.
pub type Automaton6D = NdAutomaton<Dim6D>;

/// Cellular automaton of any dimensionality.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum Automaton {
    Automaton1D(Automaton1D),
    Automaton2D(Automaton2D),
    Automaton3D(Automaton3D),
    Automaton4D(Automaton4D),
    Automaton5D(Automaton5D),
    Automaton6D(Automaton6D),
}
impl Default for Automaton {
    fn default() -> Self {
        Self::Automaton2D(Automaton2D::default())
    }
}
impl<D: Dim> From<NdAutomaton<D>> for Automaton {
    fn from(automaton: NdAutomaton<D>) -> Self {
        let a = Box::new(automaton);
        match_ndim!(match D {
            1 => Self::Automaton1D(unsafe {
                *std::mem::transmute::<Box<NdAutomaton<D>>, Box<NdAutomaton<Dim1D>>>(a)
            }),
            2 => Self::Automaton2D(unsafe {
                *std::mem::transmute::<Box<NdAutomaton<D>>, Box<NdAutomaton<Dim2D>>>(a)
            }),
            3 => Self::Automaton3D(unsafe {
                *std::mem::transmute::<Box<NdAutomaton<D>>, Box<NdAutomaton<Dim3D>>>(a)
            }),
            4 => Self::Automaton4D(unsafe {
                *std::mem::transmute::<Box<NdAutomaton<D>>, Box<NdAutomaton<Dim4D>>>(a)
            }),
            5 => Self::Automaton5D(unsafe {
                *std::mem::transmute::<Box<NdAutomaton<D>>, Box<NdAutomaton<Dim5D>>>(a)
            }),
            6 => Self::Automaton6D(unsafe {
                *std::mem::transmute::<Box<NdAutomaton<D>>, Box<NdAutomaton<Dim6D>>>(a)
            }),
        })
    }
}
impl AsSimulate for Automaton {
    fn as_sim(&self) -> &dyn Simulate {
        match self {
            Self::Automaton1D(a) => a,
            Self::Automaton2D(a) => a,
            Self::Automaton3D(a) => a,
            Self::Automaton4D(a) => a,
            Self::Automaton5D(a) => a,
            Self::Automaton6D(a) => a,
        }
    }
    fn as_sim_mut(&mut self) -> &mut dyn Simulate {
        match self {
            Self::Automaton1D(a) => a,
            Self::Automaton2D(a) => a,
            Self::Automaton3D(a) => a,
            Self::Automaton4D(a) => a,
            Self::Automaton5D(a) => a,
            Self::Automaton6D(a) => a,
        }
    }
}

/// Immutable reference to a cellular automaton of any dimensionality.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum AutomatonRef<'a> {
    Automaton1D(&'a Automaton1D),
    Automaton2D(&'a Automaton2D),
    Automaton3D(&'a Automaton3D),
    Automaton4D(&'a Automaton4D),
    Automaton5D(&'a Automaton5D),
    Automaton6D(&'a Automaton6D),
}

/// Mutable reference to a cellular automaton of any dimensionality.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum AutomatonMut<'a> {
    Automaton1D(&'a mut Automaton1D),
    Automaton2D(&'a mut Automaton2D),
    Automaton3D(&'a mut Automaton3D),
    Automaton4D(&'a mut Automaton4D),
    Automaton5D(&'a mut Automaton5D),
    Automaton6D(&'a mut Automaton6D),
}

/// Cellular automaton simulation, including a grid of cells (stored using an
/// `NdTree`), a rule, and a generation count.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct NdAutomaton<D: Dim> {
    pub ndtree: NdTree<D>,
    pub rule: Arc<dyn NdRule<D>>,
    pub generations: BigInt,
    pub comments: String,
}
impl<D: Dim> Default for NdAutomaton<D> {
    fn default() -> Self {
        Self {
            ndtree: NdTree::default(),
            rule: Arc::new(DummyRule),
            generations: BigInt::zero(),
            comments: String::new(),
        }
    }
}
impl<D: Dim> Simulate for NdAutomaton<D> {
    fn ndim(&self) -> usize {
        D::NDIM
    }
    fn population(&self) -> BigUint {
        self.ndtree.root_ref().population()
    }
    fn generation_count(&self) -> &BigInt {
        &self.generations
    }
    fn set_generation_count(&mut self, generations: BigInt) {
        self.generations = generations;
    }
    fn step(&mut self, gens: &BigInt) {
        hashlife::step(&mut self.ndtree, &*self.rule, gens);
        self.generations += gens;
    }

    fn memory_usage(&self) -> usize {
        self.ndtree.pool().access().memory_usage()
    }
    fn yield_to_gc(&self) {
        let _ = self.ndtree.pool().yield_to_gc();
    }
    fn schedule_gc(
        &self,
        post_gc: Box<dyn Send + FnOnce(usize, usize, usize)>,
    ) -> std::thread::JoinHandle<()> {
        let node_pool = self.ndtree.pool().new_ref();
        let ret = std::thread::spawn(move || {
            let mut node_pool_access = node_pool.total_access();
            let (dropped, kept) = node_pool_access.strong_gc();
            let new_memory_usage = node_pool_access.memory_usage();
            post_gc(dropped, kept, new_memory_usage);
        });
        ret
    }
}
impl<D: Dim> NdAutomaton<D> {
    /// Sets the rule of the automaton.
    pub fn set_rule(&mut self, new_rule: impl 'static + NdRule<D>) {
        self.rule = Arc::new(new_rule);
    }
}
