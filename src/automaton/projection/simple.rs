use super::*;

#[derive(Debug, Clone)]
pub struct SimpleProjection;

impl<C: CellType, D: Dim> NdProjector<C, D, D> for SimpleProjection {
    fn project(&self, tree: &NdTree<C, D>) -> NdTree<C, D> {
        tree.clone()
    }
    fn get_params(self) -> ProjectionParams {
        ProjectionParams::Simple
    }
}
