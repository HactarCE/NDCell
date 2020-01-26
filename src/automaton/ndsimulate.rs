use num::BigInt;

/// Simulation-related methods whose type signatures are the same for all
/// automata, regardless of dimensionality.
pub trait NdSimulate {
    /// Returns the number of dimensions of the underlying automaton.
    fn get_ndim(&self) -> usize;
    /// Returns the number of live cells in the simulation.
    fn get_population(&self) -> &BigInt;
    /// Returns the simulation step size.
    fn get_step_size(&self) -> usize;
    /// Sets the simulation step size.
    fn set_step_size(&mut self, step_size: usize);
    /// Returns the number of generations that have elapsed in the simulation.
    fn get_generation_count(&self) -> isize;
    /// Sets the number of generations that have elapsed in the simulation.
    fn set_generation_count(&mut self, generations: isize);
    /// Step forward in the simulation by the step size.
    fn step(&mut self);
    /// Step forward one generation in the simulation.
    fn step_single(&mut self);
}

/// A proxy trait for NdSimulate.
///
/// To avoid a ton of boilerplate re-implementing all of the above methods of
/// NdSimulate, we intead only have to re-implement these two methods of
/// IntoNdSimulate.
pub trait IntoNdSimulate {
    /// Convert to an immutable NdSimulate trait object.
    fn ndsim(&self) -> &dyn NdSimulate;
    /// Convert to a mutable NdSimulate trait object.
    fn ndsim_mut(&mut self) -> &mut dyn NdSimulate;
}

impl<T> NdSimulate for T
where
    T: IntoNdSimulate,
{
    fn get_ndim(&self) -> usize {
        self.ndsim().get_ndim()
    }
    fn get_population(&self) -> &BigInt {
        self.ndsim().get_population()
    }
    fn get_step_size(&self) -> usize {
        self.ndsim().get_step_size()
    }
    fn set_step_size(&mut self, step_size: usize) {
        self.ndsim_mut().set_step_size(step_size);
    }
    fn get_generation_count(&self) -> isize {
        self.ndsim().get_generation_count()
    }
    fn set_generation_count(&mut self, generations: isize) {
        self.ndsim_mut().set_generation_count(generations);
    }
    fn step(&mut self) {
        self.ndsim_mut().step();
    }
    fn step_single(&mut self) {
        self.ndsim_mut().step_single();
    }
}
