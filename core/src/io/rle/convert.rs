use std::sync::Arc;

use super::*;
use crate::automaton::{Automaton, AutomatonRef, NdAutomaton};
use crate::axis::Axis::X;
use crate::dim::Dim;
use crate::io::utils::{SemiReverseRectIter, SemiReverseRectIterItem};
use crate::ndarray::NdArray;
use crate::ndrect::{BigRect, CanContain, NdRect};
use crate::ndtree::{
    LeafNodeRef, NdTree, NodeRef, NodeRefEnum, NodeRefTrait, NonLeafNodeRef, SharedNodePool,
};
use crate::ndvec::{BigVec, NdVec, UVec, UVec6D};
use crate::num::{BigInt, ToPrimitive, Zero};
use crate::sim::rule::{NdRule, Rule};

impl Automaton {
    /// Loads an automaton from an RLE string using a new node pool.
    pub fn from_rle_str(
        s: &str,
        resolve_rule: impl FnOnce(Option<&str>) -> RleResult<Rule>,
    ) -> RleResult<Self> {
        Self::from_rle(&s.parse()?, resolve_rule)
    }

    /// Loads an automaton from an RLE struct using a new node pool.
    pub fn from_rle(
        rle: &Rle,
        resolve_rule: impl FnOnce(Option<&str>) -> RleResult<Rule>,
    ) -> RleResult<Self> {
        fn _rle_to_ndautomaton<D: Dim>(
            rle: &Rle,
            rule: Arc<dyn NdRule<D>>,
        ) -> RleResult<Automaton> {
            NdAutomaton::from_rle_with_node_pool(rle, SharedNodePool::new(), |_| Ok(rule))
                .map(|a| a.into())
        }

        match resolve_rule(rle.rule())? {
            Rule::Rule1D(rule) => _rle_to_ndautomaton(&rle, rule),
            Rule::Rule2D(rule) => _rle_to_ndautomaton(&rle, rule),
            Rule::Rule3D(rule) => _rle_to_ndautomaton(&rle, rule),
            Rule::Rule4D(rule) => _rle_to_ndautomaton(&rle, rule),
            Rule::Rule5D(rule) => _rle_to_ndautomaton(&rle, rule),
            Rule::Rule6D(rule) => _rle_to_ndautomaton(&rle, rule),
        }
    }
}
impl AutomatonRef<'_> {
    /// Exports the automaton to an RLE string.
    pub fn to_rle_string(&self) -> RleResult<String> {
        match self {
            Self::Automaton1D(a) => a.to_rle_string(None),
            Self::Automaton2D(a) => a.to_rle_string(None),
            Self::Automaton3D(a) => a.to_rle_string(None),
            Self::Automaton4D(a) => a.to_rle_string(None),
            Self::Automaton5D(a) => a.to_rle_string(None),
            Self::Automaton6D(a) => a.to_rle_string(None),
        }
    }
    /// Exports the automaton to an extended RLE string, which includes a CXRLE
    /// header.
    pub fn to_cxrle_string(&self) -> RleResult<String> {
        match self {
            Self::Automaton1D(a) => a.to_cxrle_string(None),
            Self::Automaton2D(a) => a.to_cxrle_string(None),
            Self::Automaton3D(a) => a.to_cxrle_string(None),
            Self::Automaton4D(a) => a.to_cxrle_string(None),
            Self::Automaton5D(a) => a.to_cxrle_string(None),
            Self::Automaton6D(a) => a.to_cxrle_string(None),
        }
    }

    /// Exports the automaton to an RLE struct.
    pub fn to_rle(&self) -> RleResult<Rle> {
        match self {
            Self::Automaton1D(a) => a.to_rle(None),
            Self::Automaton2D(a) => a.to_rle(None),
            Self::Automaton3D(a) => a.to_rle(None),
            Self::Automaton4D(a) => a.to_rle(None),
            Self::Automaton5D(a) => a.to_rle(None),
            Self::Automaton6D(a) => a.to_rle(None),
        }
    }
}
impl<D: Dim> NdAutomaton<D> {
    /// Loads an automaton from an RLE string using a new node pool.
    pub fn from_rle_str(
        s: &str,
        resolve_rule: impl FnOnce(Option<&str>) -> RleResult<Arc<dyn NdRule<D>>>,
    ) -> RleResult<Self> {
        Self::from_rle_str_with_node_pool(s, SharedNodePool::new(), resolve_rule)
    }
    /// Loads an automaton from an RLE string using an existing node pool.
    pub fn from_rle_str_with_node_pool(
        s: &str,
        node_pool: SharedNodePool<D>,
        resolve_rule: impl FnOnce(Option<&str>) -> RleResult<Arc<dyn NdRule<D>>>,
    ) -> RleResult<Self> {
        Self::from_rle_with_node_pool(&s.parse()?, node_pool, resolve_rule)
    }

    /// Loads an automaton from an RLE struct using an existing node pool.
    pub fn from_rle_with_node_pool(
        rle: &Rle,
        node_pool: SharedNodePool<D>,
        resolve_rule: impl FnOnce(Option<&str>) -> RleResult<Arc<dyn NdRule<D>>>,
    ) -> RleResult<Self> {
        let rule = resolve_rule(rle.rule())?;
        let generations = rle.generation();
        Ok(NdAutomaton {
            tree: NdTree::from_rle_with_node_pool(rle, node_pool),
            rule,
            generations,
        })
    }

    /// Exports a rectangle from the automaton to an RLE string. If `rect` is
    /// `None`, the entire grid is exported.
    pub fn to_rle_string(&self, rect: Option<BigRect<D>>) -> RleResult<String> {
        let rle = self.to_rle(rect)?;
        if self.rule.max_state() > 1_u8 {
            Ok(rle.to_string())
        } else {
            Ok(rle.to_string_2_state())
        }
    }
    /// Exports a rectangle from the automaton to an extended RLE string, which
    /// includes a CXRLE header. If `rect` is `None`, the entire grid is
    /// exported.
    pub fn to_cxrle_string(&self, rect: Option<BigRect<D>>) -> RleResult<String> {
        let rle = self.to_rle(rect)?.without_cxrle();
        if self.rule.max_state() > 1_u8 {
            Ok(rle.to_string())
        } else {
            Ok(rle.to_string_2_state())
        }
    }
    /// Exports a rectangle from the automaton to an RLE struct. If `rect` is
    /// `None`, the entire grid is exported.
    pub fn to_rle(&self, rect: Option<BigRect<D>>) -> RleResult<Rle> {
        Ok(self
            .tree
            .to_rle(rect)?
            .with_rule(Some(self.rule.to_string()))
            .with_generation(self.generations.clone()))
    }
}

impl<D: Dim> NdTree<D> {
    /// Loads an ND-tree from an RLE string using a new node pool.
    pub fn from_rle_str(s: &str) -> RleResult<Self> {
        Self::from_rle_str_with_node_pool(s, SharedNodePool::new())
    }
    /// Loads an ND-tree from an RLE string using an existing node pool.
    pub fn from_rle_str_with_node_pool(s: &str, node_pool: SharedNodePool<D>) -> RleResult<Self> {
        Ok(Self::from_rle_with_node_pool(&s.parse()?, node_pool))
    }

    /// Loads an ND-tree from an RLE struct using an existing node pool.
    pub fn from_rle_with_node_pool(rle: &Rle, node_pool: SharedNodePool<D>) -> Self {
        let mut ret = NdTree::with_node_pool(node_pool);

        let mut start: BigVec<D> = rle
            .cxrle_header
            .as_ref()
            // Convert from 6D to ND
            .map(|cxrle| BigVec::from_fn(|ax| cxrle.pos[ax].clone()))
            .unwrap_or(BigVec::origin());
        // Negate all axes except the X axis.
        for &ax in &D::axes()[1..] {
            start[ax] *= -1;
        }

        let mut pos = start.clone();

        for run in &rle.runs {
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

        ret
    }

    /// Exports a rectangle from the automaton to an RLE struct. If `rect` is
    /// `None`, the entire grid is exported.
    pub fn to_rle(&self, rect: Option<BigRect<D>>) -> RleResult<Rle> {
        let mut header = RleHeader {
            size: NdVec::origin(),
            rule: None,
        };
        let mut cxrle_header = None;
        let mut runs = RleRunVec::default();

        if let Some(bounding_rect) = rect.or_else(|| self.bounding_rect()) {
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
            let mut cxrle_pos = NdVec::origin();
            for &ax in D::axes() {
                header.size[ax] = size[ax].to_usize().ok_or(RleError::TooBig)?;
                cxrle_pos[ax] = pos[ax].clone();
            }
            cxrle_header = Some(CxrleHeader {
                pos: cxrle_pos,
                gen: BigInt::zero(),
            });

            let bounding_rect = bounding_rect - self.offset();
            let row_rect_min = bounding_rect.min();
            let mut row_rect_max = bounding_rect.max();
            row_rect_max[X] = row_rect_min[X].clone();
            let row_rect = NdRect::span(row_rect_min, row_rect_max);
            for rect_iter_item in SemiReverseRectIter::new(row_rect) {
                match rect_iter_item {
                    SemiReverseRectIterItem::Pos(row_start) => {
                        let root_node = self.root_ref();
                        let runs_iter = rle_row_of_node(root_node.as_ref(), row_start);
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

// This conversion fails (panics) if the resulting array is too large to fit in
// memory.
impl<D: Dim> From<&Rle> for NdArray<u8, D> {
    fn from(rle: &Rle) -> Self {
        let size: UVec<D> = UVec::from_fn(|ax| rle.header.size[ax] as usize);

        let mut ret = NdArray::from_fn(size.clone(), |_| 0);
        let rect = ret.rect();

        let mut start = rect.max();
        start[X] = 0;
        let mut pos = start.clone();

        for run in &rle.runs {
            match run.item {
                RleItem::Cell(state) => {
                    if state != 0_u8 && rect.contains(&pos) {
                        let max_x = std::cmp::min(size[X], pos[X].saturating_add(run.count));
                        for _x in pos[X]..max_x {
                            ret[pos.clone()] = state;
                            pos[X] = pos[X].saturating_add(1);
                        }
                    }
                }

                RleItem::Next(axis) => {
                    // Reset all axes before this one.
                    for &ax in D::axes() {
                        if ax < axis {
                            pos[ax] = start[ax];
                        }
                    }
                    if D::axes().contains(&axis) {
                        // Increment this axis.
                        pos[axis] = pos[axis].saturating_add(run.count);
                    } else {
                        // Or end early if there are too many dimensions.
                        return ret;
                    }
                    // Return if we are beyond the array bounds.
                    if !rect.contains(&pos) {
                        return ret;
                    }
                }

                RleItem::End => return ret,
            }
        }

        ret
    }
}
impl<D: Dim> From<&NdArray<u8, D>> for Rle {
    fn from(ndarray: &NdArray<u8, D>) -> Self {
        let mut size = UVec6D::repeat(1);
        for &ax in D::axes() {
            size[ax] = ndarray.size()[ax];
        }

        let mut runs = RleRunVec::default();
        for item in SemiReverseRectIter::new(ndarray.rect().to_bigrect()) {
            runs.append(match item {
                SemiReverseRectIterItem::Pos(pos) => RleItem::Cell(ndarray[pos.to_uvec()]),
                SemiReverseRectIterItem::Next(axis) => RleItem::Next(axis),
            });
        }
        let runs = runs.into_vec();

        Self {
            header: RleHeader { size, rule: None },
            cxrle_header: None,
            runs,
            comments: String::new(),
        }
    }
}

fn rle_row_of_node<'pool, D: Dim>(
    node: NodeRef<'pool, D>,
    start_pos: BigVec<D>,
) -> Box<dyn 'pool + Iterator<Item = RleResult<RleRun>>> {
    if let Some(single_state) = node.single_state() {
        // All cells in this node are the same state, so encode a run of that
        // cell state with the length of this node.
        Box::new(std::iter::once_with(move || {
            Ok(RleRun {
                count: (node.layer().big_len() - &node.layer().modulo_pos(&start_pos)[X])
                    .to_usize()
                    .ok_or(RleError::TooBig)?,
                item: RleItem::Cell(single_state),
            })
        }))
    } else {
        match node.as_enum() {
            NodeRefEnum::Leaf(node) => {
                let start_pos = node.modulo_pos(&start_pos).to_uvec();
                rle_row_of_leaf_node(node, start_pos)
            }
            NodeRefEnum::NonLeaf(node) => rle_row_of_non_leaf_node(node, start_pos),
        }
    }
}
fn rle_row_of_leaf_node<'pool, D: Dim>(
    node: LeafNodeRef<'pool, D>,
    start_pos: UVec<D>,
) -> Box<dyn 'pool + Iterator<Item = RleResult<RleRun>>> {
    let mut end_pos = start_pos.clone();
    end_pos[X] = node.len() - 1;

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
) -> Box<dyn 'pool + Iterator<Item = RleResult<RleRun>>> {
    let index1 = node.child_index_with_pos(&start_pos);
    let index2 = index1 | X.bit();
    if index1 == index2 {
        rle_row_of_node(node.child_at_index(index1), start_pos)
    } else {
        let mut start_pos2 = start_pos.clone();
        start_pos2[X].set_zero();
        Box::new(itertools::chain(
            rle_row_of_node(node.child_at_index(index1), start_pos),
            rle_row_of_node(node.child_at_index(index2), start_pos2),
        ))
    }
}
