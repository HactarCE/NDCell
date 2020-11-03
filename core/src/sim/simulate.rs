use crate::num::{BigInt, BigUint};

/// Dimension-independent automaton simulation methods.
pub trait Simulate {
    /// Returns the number of dimensions of the underlying automaton.
    fn ndim(&self) -> usize;
    /// Returns the number of live cells in the simulation.
    fn population(&self) -> BigUint;
    /// Returns the number of generations that have elapsed in the simulation.
    fn generation_count(&self) -> &BigInt;
    /// Sets the number of generations that have elapsed in the simulation.
    fn set_generation_count(&mut self, generations: BigInt);
    /// Steps forward in the simulation by the given number of generations.
    fn step(&mut self, gens: &BigInt);

    /// Returns memory usage.
    fn memory_usage(&self) -> usize;
    /// Block if the GC thread is waiting and the `RwLock` has decided it's been
    /// waiting long enough.
    fn yield_to_gc(&self);
    /// Spawns a garbage collection thread that collects garbage and then runs a
    /// closure with the arguments `nodes_dropped`, `nodes_kept`, and `new_memory_usage`.
    fn schedule_gc(
        &self,
        post_gc: Box<dyn Send + FnOnce(usize, usize, usize)>,
    ) -> std::thread::JoinHandle<()>;
}

/// A proxy trait for `Simulate`.
///
/// To avoid a ton of boilerplate re-implementing all of the above methods of
/// `Simulate`, we intead only have to re-implement these two methods of
/// `AsSimulate`.
pub trait AsSimulate {
    /// Convert to an immutable Simulate trait object.
    fn as_sim(&self) -> &dyn Simulate;
    /// Convert to a mutable Simulate trait object.
    fn as_sim_mut(&mut self) -> &mut dyn Simulate;
}

impl<T> Simulate for T
where
    T: AsSimulate,
{
    fn ndim(&self) -> usize {
        self.as_sim().ndim()
    }
    fn population(&self) -> BigUint {
        self.as_sim().population()
    }
    fn generation_count(&self) -> &BigInt {
        self.as_sim().generation_count()
    }
    fn set_generation_count(&mut self, generations: BigInt) {
        self.as_sim_mut().set_generation_count(generations);
    }
    fn step(&mut self, gens: &BigInt) {
        self.as_sim_mut().step(gens);
    }

    fn memory_usage(&self) -> usize {
        self.as_sim().memory_usage()
    }
    fn yield_to_gc(&self) {
        self.as_sim().yield_to_gc();
    }
    fn schedule_gc(
        &self,
        post_gc: Box<dyn Send + FnOnce(usize, usize, usize)>,
    ) -> std::thread::JoinHandle<()> {
        self.as_sim().schedule_gc(post_gc)
    }
}
