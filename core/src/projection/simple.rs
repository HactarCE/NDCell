use super::*;

/// A basic no-op projection that simply returns the same automaton with the
/// same number of dimensions.
#[derive(Debug, Clone)]
pub struct SimpleProjection;

impl<C: CellType, D: Dim> NdProjector<C, D, D> for SimpleProjection {
    fn project(&self, tree: &NdTree<C, D>) -> NdTree<C, D> {
        tree.clone()
    }
    fn unproject_pos(&self, pos: &BigVec<D>) -> BigVec<D> {
        pos.clone()
    }
    fn overwrite_projected(&self, _destination: &mut NdTree<C, D>, _source: &NdTree<C, D>) {
        unimplemented!()
    }
    fn get_params(&self) -> ProjectionParams {
        ProjectionParams::Simple
    }
}
