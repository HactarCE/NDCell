mod compiler;
mod runtime;
mod textpoint;

use super::Rule;
use crate::automaton::space::*;
use textpoint::*;

#[derive(Debug)]
pub struct NdcaRule<D: Dim> {
    phantom: std::marker::PhantomData<D>,
}

impl<D: Dim> Rule<u8, D> for NdcaRule<D> {
    fn radius(&self) -> usize {
        unimplemented!()
    }
    fn transition(&self, _napkin: &NdArraySlice<'_, u8, D>) -> u8 {
        unimplemented!()
    }
}
