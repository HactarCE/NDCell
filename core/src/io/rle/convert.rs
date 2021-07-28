use super::*;
use crate::axis::Axis::X;
use crate::dim::Dim;
use crate::io::utils::{SemiReverseRectIter, SemiReverseRectIterItem};
use crate::ndrect::{BigRect, NdRect};
use crate::ndtree::{
    LeafNodeRef, NdTree, NodeRef, NodeRefEnum, NodeRefTrait, NonLeafNodeRef, Region, SharedNodePool,
};
use crate::ndvec::{BigVec, NdVec, UVec};
use crate::num::{BigInt, Integer, ToPrimitive, Zero};

impl CaFormatTrait for Rle {
    fn to_string_2_state(&self) -> String {
        format!("{:b}", self)
    }

    fn rule(&self) -> Option<&str> {
        self.header.rule.as_ref().map(|r| r.as_str())
    }
    fn with_rule(mut self, rule: Option<impl ToString>) -> Self {
        self.header.rule = rule.map(|r| r.to_string());
        self
    }

    fn generation(&self) -> BigInt {
        match &self.cxrle_header {
            Some(cxrle_header) => cxrle_header.gen.clone(),
            None => BigInt::zero(),
        }
    }
    fn with_generation(mut self, generation: BigInt) -> Self {
        self.cxrle_header
            .get_or_insert_with(CxrleHeader::default)
            .gen = generation;
        self
    }

    fn comments(&self) -> &str {
        &self.comments
    }
    fn comments_mut(&mut self) -> &mut String {
        &mut self.comments
    }

    fn region<D: Dim>(&self) -> Region<D> {
        Region::Rect(self.rect())
    }

    fn to_ndtree<D: Dim>(&self, node_pool: SharedNodePool<D>) -> Result<NdTree<D>, Self::Err> {
        let mut ret = NdTree::with_node_pool(node_pool);

        let start: BigVec<D> = self.first_run_start();

        let mut pos = start.clone();
        for run in &self.runs {
            match run.item {
                RleItem::Cell(0) => pos[X] += run.count,
                RleItem::Cell(state) => {
                    for _ in 0..run.count {
                        ret.set_cell(&pos, state);
                        pos[X] += 1;
                    }
                }
                RleItem::Next(axis) => {
                    // Reset all axes before this one.
                    for &ax in D::axes() {
                        if ax < axis {
                            pos[ax] = start[ax].clone();
                        }
                    }
                    // Advance along this axis, but negative because all axes
                    // except X are reversed.
                    pos[axis] -= run.count;
                }
                RleItem::End => break,
            }
        }

        Ok(ret)
    }

    fn from_ndtree<D: Dim>(
        ndtree: &NdTree<D>,
        rect: Option<BigRect<D>>,
    ) -> Result<Self, Self::Err> {
        let mut header = RleHeader {
            size: NdVec::zero(),
            rule: None,
        };
        let mut cxrle_header = None;
        let mut runs = RleRunVec::default();

        if let Some(bounding_rect) = rect.or_else(|| ndtree.bounding_rect()) {
            let size = bounding_rect.size();
            let pos = {
                // Negate all axes except X.
                let mut cxrle_bounding_rect = bounding_rect.clone();
                for &ax in &D::axes()[1..] {
                    cxrle_bounding_rect.negate_axis(ax);
                }
                cxrle_bounding_rect.min()
            };

            header.size = NdVec::repeat(1);
            let mut cxrle_pos = NdVec::zero();
            for &ax in D::axes() {
                header.size[ax] = size[ax].to_usize().ok_or(RleError::TooBig)?;
                cxrle_pos[ax] = pos[ax].clone();
            }
            cxrle_header = Some(CxrleHeader {
                pos: cxrle_pos,
                gen: BigInt::zero(),
            });

            let bounding_rect = bounding_rect - ndtree.base_pos();
            let row_rect_min = bounding_rect.min();
            let mut row_rect_max = bounding_rect.max();
            row_rect_max[X] = row_rect_min[X].clone();
            let row_rect = NdRect::span(row_rect_min, row_rect_max);
            let row_length = bounding_rect.size()[X].to_usize().ok_or(RleError::TooBig)?;
            for rect_iter_item in SemiReverseRectIter::new(row_rect) {
                match rect_iter_item {
                    SemiReverseRectIterItem::Pos(row_start) => {
                        let root_node = ndtree.root_ref();
                        let runs_iter = rle_row_of_node(root_node.as_ref(), row_start, row_length);
                        runs.try_extend(runs_iter)?;
                    }
                    SemiReverseRectIterItem::Next(ax) => runs.append(RleItem::Next(ax)),
                }
            }
        }

        runs.append(RleItem::End);
        let runs = runs.into_vec();

        Ok(Rle {
            header,
            cxrle_header,
            runs,
            comments: String::new(),
        })
    }
}

fn rle_row_of_node<'pool, D: Dim>(
    node: NodeRef<'pool, D>,
    start_pos: BigVec<D>,
    max_length: usize,
) -> Box<dyn 'pool + Iterator<Item = RleResult<RleRun>>> {
    if max_length == 0 {
        return Box::new(std::iter::empty());
    }
    debug_assert!(BigInt::from(max_length) <= node.big_len());
    if let Some(single_state) = node.single_state() {
        // All cells in this node are the same state, so encode a run of that
        // cell state with the length of this node.
        Box::new(std::iter::once_with(move || {
            Ok(RleRun {
                count: std::cmp::min(
                    max_length,
                    (node.layer().big_len() - &node.layer().modulo_pos(&start_pos)[X])
                        .to_usize()
                        .ok_or(RleError::TooBig)?,
                ),
                item: RleItem::Cell(single_state),
            })
        }))
    } else {
        match node.as_enum() {
            NodeRefEnum::Leaf(node) => {
                let start_pos = node.modulo_pos(&start_pos).to_uvec();
                rle_row_of_leaf_node(node, start_pos, max_length)
            }
            NodeRefEnum::NonLeaf(node) => rle_row_of_non_leaf_node(node, start_pos, max_length),
        }
    }
}
fn rle_row_of_leaf_node<'pool, D: Dim>(
    node: LeafNodeRef<'pool, D>,
    start_pos: UVec<D>,
    max_length: usize,
) -> Box<dyn 'pool + Iterator<Item = RleResult<RleRun>>> {
    let mut end_pos = start_pos.clone();
    end_pos[X] += max_length - 1;

    let start_index = node.pos_to_cell_index(start_pos);
    let end_index = node.pos_to_cell_index(end_pos);
    Box::new(
        node.cells()[start_index..=end_index]
            .iter()
            .copied()
            .map(RleItem::Cell)
            .map(RleRun::from)
            .map(Ok),
    )
}
fn rle_row_of_non_leaf_node<'pool, D: Dim>(
    node: NonLeafNodeRef<'pool, D>,
    start_pos: BigVec<D>,
    max_length: usize,
) -> Box<dyn 'pool + Iterator<Item = RleResult<RleRun>>> {
    let index1 = node.child_index_with_pos(&start_pos);
    let index2 = index1 | X.bit();
    if index1 == index2 {
        rle_row_of_node(node.child_at_index(index1), start_pos, max_length)
    } else {
        let mut start_pos_2 = start_pos.clone();
        start_pos_2[X].set_zero();
        let child_layer_len = node.layer().child_layer().big_len();
        let max_length_1 = std::cmp::min(
            max_length,
            (&child_layer_len - start_pos[X].mod_floor(&child_layer_len))
                .to_usize()
                .unwrap_or(usize::MAX),
        );
        let max_length_2 = max_length - max_length_1;
        Box::new(itertools::chain(
            rle_row_of_node(node.child_at_index(index1), start_pos, max_length_1),
            rle_row_of_node(node.child_at_index(index2), start_pos_2, max_length_2),
        ))
    }
}
