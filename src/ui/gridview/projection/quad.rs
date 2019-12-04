use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use super::*;

/// Information describing how to slice an NdTree to get a 2D quadtree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NdProjectionInfo2D<D: Dim> {
    /// A vector determining where to slice the NdTree.
    pub slice_pos: NdVec<D>,
    /// The axis of the NdTree to be displayed horizontally.
    h: Axis,
    /// The axis of the NdTree to be displayed vertically.
    v: Axis,
    /// A cache of branch indices for the given layer.
    branch_idx_cache: RefCell<Vec<[usize; 4]>>,
}

impl<D: Dim> Hash for NdProjectionInfo2D<D> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.slice_pos.hash(hasher);
        self.h.hash(hasher);
        self.v.hash(hasher);
    }
}

impl<D: Dim> NdProjectionInfo<D> for NdProjectionInfo2D<D> {
    type PDim = Dim2D;
    fn get_slice_pos(&self) -> NdVec<D> {
        self.slice_pos
    }
    fn with_slice_pos(&self, slice_pos: NdVec<D>) -> Self {
        Self::new(slice_pos, self.h, self.v)
            .expect("Changing the slice position invalidated an NdProjectionInfo")
    }
    fn pdim_to_ndim(&self, pos: Vec2D) -> NdVec<D> {
        let mut ret = self.slice_pos;
        ret[self.h] = *pos.x();
        ret[self.v] = *pos.y();
        ret
    }
    fn ndim_to_pdim(&self, pos: NdVec<D>) -> Vec2D {
        [pos[self.h], pos[self.v]].into()
    }
}

impl<D: Dim> Default for NdProjectionInfo2D<D> {
    fn default() -> Self {
        if D::NDIM < 2 {
            panic!("Cannot create NdProjectionInfo2D for a grid with fewer than 2 dimensions.");
        }
        Self::new(NdVec::origin(), Axis::X, Axis::Y).unwrap()
    }
}

impl<D: Dim> NdProjectionInfo2D<D> {
    /// Returns true if the given axes are different and within the
    /// dimensionality of this grid.
    fn check_axes(h: Axis, v: Axis) -> bool {
        h != v && (h as usize) < D::NDIM && (v as usize) < D::NDIM
    }
    /// Constructs a new projection with the given slice position and display
    /// axes. Returns Err(()) if the display axes are not valid.
    pub fn new(
        slice_pos: NdVec<D>,
        horizontal_display_axis: Axis,
        vertical_display_axis: Axis,
    ) -> Result<Self, ()> {
        let h = horizontal_display_axis;
        let v = vertical_display_axis;
        if Self::check_axes(h, v) {
            Ok(Self {
                slice_pos,
                h,
                v,
                branch_idx_cache: RefCell::default(),
            })
        } else {
            Err(())
        }
    }
    /// Returns a new identical projection but with the given axes displayed
    /// vertically and horizontally.
    pub fn with_display_axes(&self, horizontal: Axis, vertical: Axis) -> Result<Self, ()> {
        Self::new(self.slice_pos, horizontal, vertical)
    }
    /// Returns the axis displayed horizontally.
    pub fn get_horizontal_display_axis(&self) -> Axis {
        self.h
    }
    /// Returns the axis displayed vertically.
    pub fn get_vertical_display_axis(&self) -> Axis {
        self.v
    }

    pub fn get_ndim_branch_idx_for_pos(&self, layer: usize, pos: Vec2D) -> usize {
        self.get_ndim_branch_idx(layer, ndtree_branch_idx::<Dim2D>(layer, pos))
    }
    pub fn get_ndim_branch_idx(&self, layer: usize, branch_idx_2d: usize) -> usize {
        let cache = self.branch_idx_cache.borrow();
        match cache.get(layer) {
            Some(branch_indices) => branch_indices[branch_idx_2d],
            None => {
                drop(cache);
                let mut cache = self.branch_idx_cache.borrow_mut();
                if cache.is_empty() {
                    // Add a dummy entry for layer 0.
                    cache.push(Default::default());
                }
                let h_bit = self.h.branch_bit();
                let v_bit = self.v.branch_bit();
                for l in cache.len()..=layer {
                    // Compute the branch index for layer l.
                    let base_branch_idx =
                        ndtree_branch_idx::<D>(l, self.slice_pos) & !h_bit & !v_bit;
                    cache.push([
                        base_branch_idx,
                        base_branch_idx | h_bit,
                        base_branch_idx | v_bit,
                        base_branch_idx | h_bit | v_bit,
                    ]);
                }
                cache[layer][branch_idx_2d]
            }
        }
    }
}
