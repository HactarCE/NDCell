//! Arbitrary regions of an N-dimensional grid.

use super::{ArcNode, NdTree, NodeRef, NodeRefTrait};
use crate::dim::Dim;
use crate::lazyvec::LazyVec;
use crate::ndrect::{BigRect, CanContain};
use crate::ndvec::BigVec;

/// Finite set of cell positions, with several possible representations.
#[derive(Debug, Clone)]
pub enum Region<D: Dim> {
    /// Region containing no cells.
    Empty,
    /// Rectangular region.
    Rect(BigRect<D>),
    /// Arbitrarily-shaped region, represented using cell states #0 and #1.
    NdTree(NdTree<D>),
}
impl<D: Dim> Default for Region<D> {
    fn default() -> Self {
        Region::Empty
    }
}
impl<D: Dim> From<BigRect<D>> for Region<D> {
    fn from(rect: BigRect<D>) -> Self {
        Region::Rect(rect)
    }
}
impl<D: Dim> Region<D> {
    /// Returns the smallest rectangle containing the region, or `None` if the
    /// region is empty.
    pub fn bounding_rect(&self) -> Option<BigRect<D>> {
        match self {
            Region::Empty => None,
            Region::Rect(rect) => Some(rect.clone()),
            Region::NdTree(ndtree) => ndtree.bounding_rect(),
        }
    }

    /// Converts the region into an ND-tree using state #1 for cells that are
    /// inside the region and state #0 for cells that are not.
    pub fn into_ndtree(self, center: BigVec<D>) -> NdTree<D> {
        match self {
            Region::Empty => NdTree::with_center(center),
            Region::Rect(rect) => {
                let mut ret = NdTree::with_center(center);
                ret.expand_to(&rect);

                let node_pool = ret.pool().access();

                let mut full_nodes: LazyVec<NodeRef<'_, D>, _> =
                    LazyVec::new(|previous| match previous.last() {
                        Some(smaller_full_node) => {
                            node_pool.join_nodes(std::iter::repeat(*smaller_full_node))
                        }
                        None => node_pool.get_from_cells(vec![1_u8]),
                    });

                // It should be possible to do this in O(d log n) time, but this
                // O(d n^(d-1)) algorithm is simpler. If converting a rectangle
                // to an ND-tree is a performance bottleneck, this can be
                // rewritten.
                let root = ret.root_ref();
                let new_root = root.recursive_modify_with_offset(
                    ret.base_pos(),
                    &mut |node_pos, node| {
                        let node_rect = node.big_rect() + node_pos;
                        if rect.contains(&node_rect) {
                            Some(*full_nodes.get(node.layer().to_usize()))
                        } else if rect.intersects(&node_rect) {
                            None
                        } else {
                            Some(node) // already empty
                        }
                    },
                    &mut |pos, _| rect.contains(pos) as u8,
                );

                let new_root = ArcNode::from(new_root);
                drop((node_pool, root));
                ret.set_root_centered(new_root);
                ret
            }
            Region::NdTree(mut ndtree) => {
                ndtree.recenter(&center);
                ndtree
            }
        }
    }
}
impl<D: Dim> CanContain<BigVec<D>> for Region<D> {
    fn contains(&self, pos: &BigVec<D>) -> bool {
        match self {
            Region::Empty => false,
            Region::Rect(rect) => rect.contains(pos),
            Region::NdTree(ndtree) => ndtree.get_cell(pos) != 0_u8,
        }
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;
    use crate::ndrect::proptest_irect2d;
    use crate::ndvec::proptest_ivec2d;

    proptest! {
        #[test]
        fn test_rect_region_into_ndtree(
            rect in proptest_irect2d(-100..=100),
            center in proptest_ivec2d(-200..=200),
            cells_to_check in prop::collection::vec(proptest_ivec2d(-100..=100), 1..=20),
        ) {
            let center = center.to_bigvec();

            let region = Region::Rect(rect.to_bigrect());
            let ndtree = region.clone().into_ndtree(center.clone());
            assert_eq!(center, ndtree.center_pos());

            for pos in cells_to_check {
                let expected = rect.contains(&pos);
                let big_pos = pos.to_bigvec();
                assert_eq!(expected, region.contains(&big_pos));
                assert_eq!(expected, ndtree.get_cell(&big_pos) == 1_u8);
            }
        }
    }
}
