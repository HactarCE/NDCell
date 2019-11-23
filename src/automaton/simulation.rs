use std::collections::HashMap;

use super::*;

/// A HashLife simulation of a given automaton that caches simulation results.
pub struct Simulation<'a, C: CellType, D: Dim, R: Rule<C, D>> {
    rule: &'a R,
    step_size: usize,
    results: ResultsCache<C, D>,
}

impl<'a, C: CellType, D: Dim, R: Rule<C, D>> Simulation<'a, C, D, R> {
    /// Constructs a new Simulation with the given rule and step size.
    pub fn new(rule: &'a R, step_size: usize) -> Self {
        Self {
            rule,
            step_size,
            results: ResultsCache::default(),
        }
    }

    /// Advances the given NdTree by step_size generations.
    pub fn step(&mut self, _tree: &mut NdTree<C, D>) {
        unimplemented!()
    }
}

/// A cache of simulation results for a variety of step sizes.
#[derive(Debug, Default)]
struct ResultsCache<C: CellType, D: Dim>(HashMap<usize, SingleStepResultsCache<C, D>, NodeHasher>);
impl<C: CellType, D: Dim> ResultsCache<C, D> {
    fn get_result(
        &self,
        node: &NdCachedNode<C, D>,
        step_size: usize,
    ) -> Option<&NdCachedNode<C, D>> {
        self.0
            .get(&step_size)
            .and_then(|single_step_cache| single_step_cache.get_result(node))
    }
    fn set_result(
        &mut self,
        node: NdCachedNode<C, D>,
        step_size: usize,
        result: NdCachedNode<C, D>,
    ) {
        self.0
            .entry(step_size)
            .or_insert_with(SingleStepResultsCache::default)
            .set_result(node, result);
    }
}

/// A cache of simulation results for a given step size.
#[derive(Debug, Default)]
struct SingleStepResultsCache<C: CellType, D: Dim>(
    HashMap<NdCachedNode<C, D>, NdCachedNode<C, D>, NodeHasher>,
);
impl<C: CellType, D: Dim> SingleStepResultsCache<C, D> {
    fn get_result(&self, node: &NdCachedNode<C, D>) -> Option<&NdCachedNode<C, D>> {
        self.0.get(node)
    }
    fn set_result(&mut self, node: NdCachedNode<C, D>, result: NdCachedNode<C, D>) {
        self.0.insert(node, result);
    }
}
