use itertools::Itertools;

use super::{CachedNodeRefTrait, Layer, NodeRef, NodeRefEnum, NodeRefTrait};
use crate::dim::Dim;
use crate::ndrect::{BigRect, CanContain};
use crate::ndvec::BigVec;

/// N-dimensional mask limited to a finite power-of-2 square.
pub trait NdMask<D: Dim> {
    /// Returns the layer representing the log-2 size of the mask.
    fn layer(&self) -> Layer;

    /// Returns whether the mask covers its entire power-of-2 square.
    fn total_cover(&self) -> MaskCover;
    /// Returns whether the mask covers the rectangle.
    fn rect_cover(&self, rect: &BigRect<D>) -> MaskCover;

    /// Returns whether the point (modulo the power-of-2 square size) is
    /// included by the mask.
    fn has_cell(&self, point: &BigVec<D>) -> bool;
    /// Returns an array of booleans describing which cells are included by the
    /// mask.
    ///
    /// # Panics
    ///
    /// This method panics if the array would be too large to fit in memory.
    fn bools(&self) -> Vec<bool>;

    /// Subdivides the mask into 2^NDIM pieces.
    fn subdivide(&self) -> Result<Vec<NdMaskEnum<D>>, bool>;

    /// Generates a new ND-tree node from `destination` and `source` by calling
    /// one of the given closures with their corresponding children, recursing
    /// if the closure returns `None` or the mask only partially covers the
    /// node. Cells excluded by the mask will be taken from `destination`.
    ///
    /// Note that `self` and `other` do **not** need to use the same cache; the
    /// returned node will be from the same cache as `self`.
    ///
    /// # Panics
    ///
    /// This method panics if the mask, `destination`, and `source` are not all
    /// at the same layer, if `paste_full_node` returns a node at a different
    /// layer than the ones passed into it, or if `paste_full_node` returns a
    /// node from a cache other than `destination`'s.
    fn paste_node<'dest, 'src>(
        &self,
        destination: NodeRef<'dest, D>,
        source: NodeRef<'src, D>,
        paste_full_node: &mut impl FnMut(
            NodeRef<'dest, D>,
            NodeRef<'src, D>,
        ) -> Option<NodeRef<'dest, D>>,
        paste_cell: &mut impl FnMut(u8, u8) -> u8,
    ) -> NodeRef<'dest, D> {
        assert_eq!(self.layer(), destination.layer());
        assert_eq!(self.layer(), source.layer());

        match self.total_cover() {
            MaskCover::None => return destination,
            MaskCover::Partial => (),
            MaskCover::Full => match paste_full_node(destination, source) {
                Some(pasted) => return pasted,
                None => (),
            },
        }

        // We must recurse.
        let cache = destination.cache();
        match (destination.as_enum(), source.as_enum()) {
            (NodeRefEnum::Leaf(dest), NodeRefEnum::Leaf(src)) => cache.get_from_cells(
                self.bools()
                    .into_iter()
                    .zip(dest.cells().iter().zip(src.cells()))
                    .map(|(mask_value, (&dest_cell, &src_cell))| {
                        if mask_value {
                            paste_cell(dest_cell, src_cell)
                        } else {
                            dest_cell
                        }
                    })
                    .collect_vec(),
            ),
            (NodeRefEnum::NonLeaf(dest), NodeRefEnum::NonLeaf(src)) => cache.join_nodes(
                self.subdivide()
                    .unwrap()
                    .into_iter()
                    .zip(dest.children().zip(src.children()))
                    .map(|(mask_child, (dest_child, src_child))| {
                        mask_child.paste_node(dest_child, src_child, paste_full_node, paste_cell)
                    }),
            ),
            _ => unreachable!(),
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum MaskCover {
    None,
    Partial,
    Full,
}

#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NdMaskEnum<D: Dim> {
    Empty(Layer),
    Rect(NdRectMask<D>),
}
impl<D: Dim> From<NdRectMask<D>> for NdMaskEnum<D> {
    fn from(m: NdRectMask<D>) -> Self {
        Self::Rect(m)
    }
}
impl<D: Dim> NdMask<D> for NdMaskEnum<D> {
    fn layer(&self) -> Layer {
        match self {
            Self::Empty(layer) => *layer,
            Self::Rect(m) => m.layer(),
        }
    }

    fn total_cover(&self) -> MaskCover {
        match self {
            Self::Empty(_) => MaskCover::None,
            Self::Rect(m) => m.total_cover(),
        }
    }
    fn rect_cover(&self, rect: &BigRect<D>) -> MaskCover {
        match self {
            Self::Empty(_) => MaskCover::None,
            Self::Rect(m) => m.rect_cover(rect),
        }
    }

    fn has_cell(&self, point: &BigVec<D>) -> bool {
        match self {
            Self::Empty(_) => false,
            Self::Rect(m) => m.has_cell(point),
        }
    }
    fn bools(&self) -> Vec<bool> {
        match self {
            Self::Empty(layer) => vec![false; layer.num_cells::<D>().unwrap()],
            Self::Rect(m) => m.bools(),
        }
    }

    fn subdivide(&self) -> Result<Vec<NdMaskEnum<D>>, bool> {
        match self {
            Self::Empty(layer) => {
                if *layer == Layer(0) {
                    Err(false)
                } else {
                    Ok(vec![Self::Empty(layer.child_layer()); D::BRANCHING_FACTOR])
                }
            }
            Self::Rect(m) => m.subdivide(),
        }
    }
}
impl<D: Dim> NdMaskEnum<D> {
    /// Creates a mask including all cells in a node at the given layer that
    /// overlap with the rectangle.
    pub fn from_rect(layer: Layer, rect: &BigRect<D>) -> Self {
        match NdRectMask::new(layer, rect) {
            Some(m) => Self::Rect(m),
            None => Self::Empty(layer),
        }
    }
}

/// Rectangular mask.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NdRectMask<D: Dim> {
    layer: Layer,
    rect: BigRect<D>,
}
impl<D: Dim> NdRectMask<D> {
    /// Creates a mask including all cells in a node at the given layer that
    /// overlap with the rectangle, or `None` if no cells are included.
    pub fn new(layer: Layer, rect: &BigRect<D>) -> Option<Self> {
        rect.intersection(&layer.big_rect())
            .map(|rect| Self { layer, rect })
    }
}
impl<D: Dim> NdMask<D> for NdRectMask<D> {
    fn layer(&self) -> Layer {
        self.layer
    }

    fn total_cover(&self) -> MaskCover {
        if self.rect == self.layer.big_rect() {
            MaskCover::Full
        } else {
            MaskCover::Partial
        }
    }
    fn rect_cover(&self, other_rect: &BigRect<D>) -> MaskCover {
        match self.rect.intersection(other_rect) {
            None => MaskCover::None,
            Some(r) if r == *other_rect => MaskCover::Full,
            Some(_) => MaskCover::Partial,
        }
    }

    fn has_cell(&self, point: &BigVec<D>) -> bool {
        self.rect.contains(&self.layer.modulo_pos(point))
    }
    fn bools(&self) -> Vec<bool> {
        // We can't return a vector if the rectangle doesn't fit in a `URect`
        // anyway.
        let rect = self.rect.to_urect();
        self.layer
            .rect()
            .unwrap()
            .into_iter()
            .map(|pos| rect.contains(&pos))
            .collect_vec()
    }

    fn subdivide(&self) -> Result<Vec<NdMaskEnum<D>>, bool> {
        if self.layer == Layer(0) {
            Err(true)
        } else {
            let child_layer = self.layer.child_layer();
            Ok((0..D::BRANCHING_FACTOR)
                .map(|i| {
                    let child_offset = self.layer.big_child_offset(i);
                    NdMaskEnum::from_rect(child_layer, &(self.rect.clone() - child_offset))
                })
                .collect_vec())
        }
    }
}
