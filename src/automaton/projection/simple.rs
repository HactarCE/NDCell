use super::*;

#[derive(Debug, Clone)]
pub struct SimpleProjection;

impl<C: CellType, D: Dim> NdProjector<C, D, D> for SimpleProjection {
    fn project(&self, tree: &NdTree<C, D>) -> NdTree<C, D> {
        tree.clone()
    }
    fn overwrite_projected(&self, destination: &mut NdTree<C, D>, source: &NdTree<C, D>) {
        unimplemented!()
    }
    fn get_params(&self) -> ProjectionParams {
        ProjectionParams::Simple
    }
}
