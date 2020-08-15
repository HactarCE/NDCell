use itertools::Itertools;
use seahash::SeaHasher;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{BitAnd, BitOr};

use super::LangCellState;
use crate::{MAX_NDIM, MAX_PATTERN_SIZE};

/// Cell state pattern value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pattern {
    /// Flattened array of cell states.
    pub cells: Vec<LangCellState>,
    /// Size and shape of the pattern.
    pub shape: PatternShape,
    /// Cell state lookup table for individual cell state
    /// rotations/reflections.
    pub lut: Option<u8>,
}
impl Pattern {
    /// Returns a new pattern filled with zeros.
    pub fn zeros(shape: PatternShape, lut: Option<u8>) -> Self {
        Self {
            cells: vec![0; shape.bounds().len()],
            shape,
            lut,
        }
    }
    /// Returns the number of dimensions for this pattern.
    pub fn ndim(&self) -> usize {
        self.shape.ndim()
    }
}

/// The shape of a cell state pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternShape {
    /// Bounding rectangle of this pattern.
    bounds: Bounds,
    /// Mask, represented as a flat vector of booleans for each cell state.
    mask: Vec<bool>,
}
impl PatternShape {
    /// Constructs an empty pattern shape.
    pub fn empty(ndim: usize) -> Self {
        Self {
            bounds: Bounds::single_cell(ndim),
            mask: vec![false],
        }
    }
    /// Constructs a square pattern shape with side length 2r+1 given a radius
    /// r.
    pub fn moore(ndim: usize, radius: usize) -> Self {
        assert!(ndim <= MAX_NDIM, "Too many dimensions");
        assert!(radius <= 100, "Cannot build pattern with radius > 100");
        let radius = radius as isize;
        Self::rect(Bounds(vec![(-radius, radius); ndim]))
    }
    /// Construcs a rectangular pattern shape spanning the given bounds.
    pub fn rect(bounds: Bounds) -> Self {
        let mask = vec![true; bounds.len()];
        Self { bounds, mask }
    }
    /// Constructs an arbitrary pattern shape from bounds and a mask.
    pub fn with_mask(bounds: Bounds, mask: Vec<bool>) -> Self {
        let rect_cells = bounds.len();
        assert_eq!(
            rect_cells,
            mask.len(),
            "Pattern mask is wrong length for bounds",
        );
        assert!(rect_cells <= MAX_PATTERN_SIZE, "Pattern bounds too big");
        Self { bounds, mask }.shrink()
    }
    /// Constructs an arbitrary pattern shape from bounds an a function to
    /// generate the mask one cell at a time.
    pub fn from_fn(bounds: Bounds, f: impl FnMut(Vec<isize>) -> bool) -> Self {
        let mask = bounds.iter().map(f).collect();
        Self::with_mask(bounds, mask)
    }

    /// Returns the number of dimensions for this pattern shape.
    pub fn ndim(&self) -> usize {
        self.bounds.ndim()
    }
    /// Returns the bounds of this pattern shape.
    pub fn bounds(&self) -> &Bounds {
        &self.bounds
    }
    /// Returns the flattened mask of this pattern shape.
    pub fn flat_mask(&self) -> &Vec<bool> {
        &self.mask
    }

    /// Returns the strides used to index a flat array spanning this pattern's
    /// bounds.
    pub fn strides(&self) -> Vec<usize> {
        self.bounds()
            .size()
            .iter()
            // Each stride is the product of the lengths of the axes before it.
            .scan(1, |stride, axis_len| {
                let this_stride = *stride;
                *stride *= axis_len;
                Some(this_stride)
            })
            .collect()
    }
    /// Returns the index of the given position in a flattened array
    /// representing cells or a mask with this pattern shape. Returns None if
    /// the position is out of bounds or excluded by the mask.
    pub fn flatten_idx(&self, pos: &[isize]) -> Option<usize> {
        assert_eq!(
            self.ndim(),
            pos.len(),
            "Dimension mismatch between pattern shape and cell coordinates",
        );
        if self.bounds.contains(pos) {
            Some(self.flatten_idx_unchecked(pos) as usize)
        } else {
            None
        }
    }
    /// Returns the index of the given position in a flattened array
    /// representing cells or a mask with this pattern shape. May return an
    /// out-of-bounds index if the given position is out-of-bounds.
    pub fn flatten_idx_unchecked(&self, pos: &[isize]) -> isize {
        pos.iter()
            .zip(self.bounds.min())
            .zip(self.strides())
            .map(|((axis_pos, axis_min), axis_stride)| (axis_pos - axis_min) * axis_stride as isize)
            .sum()
    }

    /// Returns whether the given position is within this pattern shape.
    pub fn contains_pos(&self, pos: &[isize]) -> bool {
        if let Some(idx) = self.flatten_idx(pos) {
            self.mask[idx]
        } else {
            false
        }
    }

    /// Returns whether this pattern shape is rectangular.
    pub fn is_rect(&self) -> bool {
        self.mask.iter().all(|&x| x)
    }
    /// Returns whether this pattern shape is empty.
    pub fn is_empty(&self) -> bool {
        self == &Self::empty(self.ndim())
    }

    /// Returns a hashcode for this pattern shape.
    pub fn hashcode(&self) -> u64 {
        let mut h = SeaHasher::new();
        self.hash(&mut h);
        h.finish()
    }
    /// Returns the number of cells in this pattern shape in O(n^d) time (where
    /// n is the number of cells in the _bounding box_).
    pub fn len(&self) -> usize {
        self.mask.iter().filter(|&&x| x).count()
    }

    /// Returns a vector of positions in this pattern shape, in row-major order.
    pub fn positions(&self) -> Vec<Vec<isize>> {
        self.bounds()
            .iter()
            .zip(&self.mask)
            .filter(|(_pos, &mask_bit)| mask_bit)
            .map(|(pos, _mask_bit)| pos)
            .collect()
    }

    /// Returns a new pattern shape that is equivalent to this one, but with the
    /// smallest possible rectangular bounds.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn shrink(self) -> Self {
        // TODO: test this method!

        // Find lower and upper bound along each dimension. This is O(n^d) where
        // it could be O(d*n^(d-1)) in the best case by only checking cells on
        // the exterior, but oh well.
        let mut new_bounds = Bounds::unbounded(self.ndim());
        for pos in self.bounds().iter() {
            if self.contains_pos(&pos) {
                new_bounds.expand_to(&pos);
            }
        }
        if new_bounds == Bounds::unbounded(self.ndim()) {
            // Canonicalize empty pattern mask.
            Self::empty(self.ndim())
        } else if new_bounds == *self.bounds() {
            // Bail early if no change is needed.
            return self;
        } else {
            Self::from_fn(new_bounds, |pos| self.contains_pos(&pos))
        }
    }
}
impl fmt::Display for PatternShape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size_strs: Vec<String> = self.bounds().size().iter().map(|x| x.to_string()).collect();
        write!(f, "{}", size_strs.join("x"))?;
        if !self.is_rect() {
            write!(f, "~{:08x}", self.hashcode() as u32)?;
        }
        Ok(())
    }
}
impl BitOr for PatternShape {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        let mut new_bounds = self.bounds().clone();
        new_bounds.expand_to(&rhs.bounds().min());
        new_bounds.expand_to(&rhs.bounds().max());
        Self::from_fn(new_bounds, |pos| {
            self.contains_pos(&pos) || rhs.contains_pos(&pos)
        })
        .shrink()
    }
}
impl BitAnd for PatternShape {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        let mut new_bounds = Bounds::single_cell(self.ndim());
        // new_bounds.expand_to()
        let (lhs_min, lhs_max) = self.bounds().min_and_max();
        let (rhs_min, rhs_max) = rhs.bounds().min_and_max();
        for dim in 0..MAX_NDIM {
            new_bounds[dim] = (
                std::cmp::max(lhs_min[dim], rhs_min[dim]),
                std::cmp::min(lhs_max[dim], rhs_max[dim]),
            );
        }
        Self::from_fn(new_bounds, |pos| {
            self.contains_pos(&pos) && rhs.contains_pos(&pos)
        })
        .shrink()
    }
}

/// Bounding box for a pattern, represented as an inculsive range for each axis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Bounds(Vec<(isize, isize)>);
impl Bounds {
    /// Constructs a bounding box that encompasses the entire range of what can
    /// be represented using isize.
    pub fn unbounded(ndim: usize) -> Self {
        Self(vec![(isize::MIN, isize::MAX); ndim])
    }
    /// Constructs a bounding box containing just the origin.
    pub fn single_cell(ndim: usize) -> Self {
        Self(vec![(0, 0); ndim])
    }

    /// Returns the number of dimensions for this bounding box.
    pub fn ndim(&self) -> usize {
        self.0.len()
    }
    /// Returns the minimum coordinate for this bounding box.
    pub fn min(&self) -> Vec<isize> {
        self.0.iter().map(|&(lower, _upper)| lower).collect()
    }
    /// Returns the maximum coordinate for this bounding box.
    pub fn max(&self) -> Vec<isize> {
        self.0.iter().map(|&(_lower, upper)| upper).collect()
    }
    /// Returns the minimum and maximum coordinates for this bounding box.
    pub fn min_and_max(&self) -> (Vec<isize>, Vec<isize>) {
        (self.min(), self.max())
    }
    /// Returns the length of this bounding box along each axis.
    pub fn size(&self) -> Vec<usize> {
        self.0
            .iter()
            .map(|&(lower, upper)| (upper - lower) as usize + 1)
            .collect()
    }
    /// Returns the number of cells in this bounding box in O(d) time (where d
    /// is the number of dimensions).
    pub fn len(&self) -> usize {
        self.size().iter().product()
    }

    /// Returns an iterator over the positions of this bounding box, in
    /// row-major order.
    pub fn iter(&self) -> impl Iterator<Item = Vec<isize>> {
        self.0
            .iter()
            // Reverse the iterator so that X increments in the innermost loop, and
            // the Y, Z, etc. dimensions increment in outer loops.
            .rev()
            .map(|&(lower, upper)| lower..=upper)
            .multi_cartesian_product()
            // Reverse the coordinates of each position back into the correct order.
            .map(|mut pos| {
                pos.reverse();
                pos
            })
    }

    /// Returns whether this bounding box contains the given position.
    pub fn contains(&self, pos: &[isize]) -> bool {
        self.0
            .iter()
            .zip(pos)
            .all(|((min, max), pos)| min <= pos && pos <= max)
    }

    /// Expands this bounding box to encompass the given position.
    pub fn expand_to(&mut self, pos: &[isize]) {
        for ((lower, upper), &pos) in self.0.iter_mut().zip(pos) {
            *lower = std::cmp::min(*lower, pos);
            *upper = std::cmp::max(*upper, pos);
        }
    }
}
impl std::ops::Index<usize> for Bounds {
    type Output = (isize, isize);
    fn index(&self, dim: usize) -> &(isize, isize) {
        &self.0[dim]
    }
}
impl std::ops::IndexMut<usize> for Bounds {
    fn index_mut(&mut self, dim: usize) -> &mut (isize, isize) {
        &mut self.0[dim]
    }
}
