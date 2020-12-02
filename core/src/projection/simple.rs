use super::*;

/// A basic no-op projection that simply returns the same automaton with the
/// same number of dimensions.
#[derive(Debug, Clone)]
pub struct SimpleProjection;

impl<D: Dim> NdProjector<D, D> for SimpleProjection {
    fn projected_node_pool<'a>(&'a self, tree: &'a NdTree<D>) -> &'a SharedNodePool<D> {
        tree.pool()
    }
    fn project_tree(&self, tree: &NdTree<D>) -> NdTree<D> {
        tree.clone()
    }
    fn unproject_pos(&self, pos: &BigVec<D>) -> BigVec<D> {
        pos.clone()
    }
    fn overwrite_projected(&self, _destination: &mut NdTree<D>, _source: &NdTree<D>) {
        unimplemented!()
    }
    fn params(&self) -> ProjectionParams {
        ProjectionParams::Simple
    }
}
