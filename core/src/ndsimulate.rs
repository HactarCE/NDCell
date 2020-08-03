use num::BigInt;

/// Simulation-related methods whose type signatures are the same for all
/// automata, regardless of dimensionality.
pub trait NdSimulate {
    /// Returns the number of dimensions of the underlying automaton.
    fn ndim(&self) -> usize;
    /// Returns the number of live cells in the simulation.
    fn population_count(&self) -> &BigInt;
    /// Returns the number of generations that have elapsed in the simulation.
    fn generation_count(&self) -> &BigInt;
    /// Sets the number of generations that have elapsed in the simulation.
    fn set_generation_count(&mut self, generations: BigInt);
    /// Steps forward in the simulation by the given number of generations.
    fn step(&mut self, step_size: &BigInt);
}

/// A proxy trait for NdSimulate.
///
/// To avoid a ton of boilerplate re-implementing all of the above methods of
/// NdSimulate, we intead only have to re-implement these two methods of
/// IntoNdSimulate.
pub trait AsNdSimulate {
    /// Convert to an immutable NdSimulate trait object.
    fn as_ndsim(&self) -> &dyn NdSimulate;
    /// Convert to a mutable NdSimulate trait object.
    fn as_ndsim_mut(&mut self) -> &mut dyn NdSimulate;
}

impl<T> NdSimulate for T
where
    T: AsNdSimulate,
{
    fn ndim(&self) -> usize {
        self.as_ndsim().ndim()
    }
    fn population_count(&self) -> &BigInt {
        self.as_ndsim().population_count()
    }
    fn generation_count(&self) -> &BigInt {
        self.as_ndsim().generation_count()
    }
    fn set_generation_count(&mut self, generations: BigInt) {
        self.as_ndsim_mut().set_generation_count(generations);
    }
    fn step(&mut self, step_size: &BigInt) {
        self.as_ndsim_mut().step(step_size);
    }
}
