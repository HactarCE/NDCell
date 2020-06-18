use itertools::Itertools;
use seahash::SeaHasher;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{BitAnd, BitOr};

use crate::{MAX_NDIM, MAX_PATTERN_SIZE};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Bounds(Vec<(isize, isize)>);
impl Bounds {
    pub fn unbounded(ndim: usize) -> Self {
        Self(vec![(isize::MIN, isize::MAX); ndim])
    }
    pub fn single_cell(ndim: usize) -> Self {
        Self(vec![(0, 0); ndim])
    }

    pub fn ndim(&self) -> usize {
        self.0.len()
    }
    pub fn min(&self) -> Vec<isize> {
        self.0.iter().map(|&(lower, _upper)| lower).collect()
    }
    pub fn max(&self) -> Vec<isize> {
        self.0.iter().map(|&(_lower, upper)| upper).collect()
    }
    pub fn min_and_max(&self) -> (Vec<isize>, Vec<isize>) {
        (self.min(), self.max())
    }
    pub fn size(&self) -> Vec<usize> {
        self.0
            .iter()
            .map(|&(lower, upper)| (upper - lower) as usize + 1)
            .collect()
    }
    pub fn len(&self) -> usize {
        self.size().iter().product()
    }

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

    pub fn contains(&self, pos: &[isize]) -> bool {
        self.0
            .iter()
            .zip(pos)
            .all(|((min, max), pos)| min <= pos && pos <= max)
    }

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternShape {
    bounds: Bounds,
    mask: Vec<bool>,
}
impl PatternShape {
    pub fn empty(ndim: usize) -> Self {
        Self {
            bounds: Bounds::single_cell(ndim),
            mask: vec![false],
        }
    }
    pub fn moore(ndim: usize, radius: usize) -> Self {
        assert!(ndim <= MAX_NDIM, "Too many dimensions");
        assert!(radius <= 100, "Cannot build pattern with radius > 100");
        let radius = radius as isize;
        Self::rect(Bounds(vec![(-radius, radius); ndim]))
    }
    pub fn rect(bounds: Bounds) -> Self {
        let mask = vec![true; bounds.len()];
        Self::with_mask(bounds, mask)
    }
    pub fn with_mask(bounds: Bounds, mask: Vec<bool>) -> Self {
        let rect_cells = bounds.len();
        assert_eq!(
            rect_cells,
            mask.len(),
            "Pattern mask is wrong length for bounds"
        );
        assert!(rect_cells <= MAX_PATTERN_SIZE, "Pattern bounds too big");
        Self { bounds, mask }.shrink()
    }
    pub fn from_fn(bounds: Bounds, f: impl FnMut(Vec<isize>) -> bool) -> Self {
        let mask = bounds.iter().map(f).collect();
        Self::with_mask(bounds, mask)
    }

    pub fn ndim(&self) -> usize {
        self.bounds.len()
    }
    pub fn bounds(&self) -> &Bounds {
        &self.bounds
    }
    pub fn flat_mask(&self) -> &Vec<bool> {
        &self.mask
    }

    fn get_mask_idx(&self, pos: &[isize]) -> Option<usize> {
        assert_eq!(
            self.bounds.len(),
            pos.len(),
            "Dimension mismatch between pattern shape and cell coordinates"
        );
        if self.bounds.contains(pos) {
            let size = self.bounds().size();
            let min = self.bounds().min();
            let mut ret = 0;
            for axis in (0..self.ndim()).rev() {
                ret *= size[axis];
                ret += (pos[axis] - min[axis]) as usize;
            }
            Some(ret)
        } else {
            None
        }
    }

    pub fn get(&self, pos: &[isize]) -> bool {
        if let Some(idx) = self.get_mask_idx(pos) {
            self.mask[idx]
        } else {
            false
        }
    }

    pub fn is_rect(&self) -> bool {
        self.mask.iter().all(|&x| x)
    }
    pub fn is_empty(&self) -> bool {
        self == &Self::empty(self.ndim())
    }

    pub fn hashcode(&self) -> u64 {
        let mut h = SeaHasher::new();
        self.hash(&mut h);
        h.finish()
    }
    pub fn len(&self) -> usize {
        self.mask.iter().filter(|&&x| x).count()
    }

    pub fn positions(&self) -> Vec<Vec<isize>> {
        self.bounds()
            .iter()
            .zip(&self.mask)
            .filter(|(_pos, &mask_bit)| mask_bit)
            .map(|(pos, _mask_bit)| pos)
            .collect()
    }

    #[must_use]
    fn shrink(self) -> Self {
        // TODO: test this method!

        // Find lower and upper bound along each dimension. This is O(n^d) where
        // it could be O(d*n^(d-1)) in the best case by only checking cells on
        // the exterior, but oh well.
        let mut new_bounds = Bounds::unbounded(self.ndim());
        for pos in self.bounds().iter() {
            if self.get(&pos) {
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
            Self::from_fn(new_bounds, |pos| self.get(&pos))
        }
    }
}
impl fmt::Display for PatternShape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size_strs: Vec<String> = self.bounds().size().iter().map(|x| x.to_string()).collect();
        write!(f, "pattern{}", size_strs.join("x"))?;
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
        Self::from_fn(new_bounds, |pos| self.get(&pos) || rhs.get(&pos)).shrink()
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
        Self::from_fn(new_bounds, |pos| self.get(&pos) && rhs.get(&pos)).shrink()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct RleRun {
    start: u32,
    len: u32,
}
impl RleRun {
    pub fn start(self) -> u32 {
        self.start
    }
    pub fn last(self) -> u32 {
        self.start + self.len - 1
    }
    pub fn after_end(self) -> u32 {
        self.start + self.len
    }
    pub fn len(self) -> u32 {
        self.len
    }
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// struct Mask(Vec<bool>);

// impl Mask {
//     fn from_fn(bounds: &[(isize, isize)], f: impl FnMut(Vec<isize>) -> bool) -> Self {
//         let bools = Vec::with_capacity(
//             bounds
//                 .iter()
//                 .map(|&(lower, upper)| (upper - lower + 1) as usize)
//                 .product(),
//         );
//         for pos in iter_positions_in_bounds(bounds) {
//             bools.push(f(pos));
//         }
//         Self(bools)
//     }
//     fn square(bounds: &[(isize, isize)]) -> Self {
//         let len = bounds
//             .iter()
//             .map(|&(lower, upper)| (upper - lower + 1) as usize)
//             .product();
//         Self(vec![true; len])
//     }
//     fn outer(bounds: &[(isize, isize)]) -> Self {
//         Self::from_fn(|pos| pos != vec![0; bounds.len()]);
//     }
// }

// // TODO: note that the vector always has at least one entry. Even entries are
// // excluded from the pattern; odd entries are included in the pattern.
// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct PatternMask(Vec<RleRun>);
// impl PatternMask {
//     pub fn new_solid(cell_count: u32) -> Self {
//         Self(vec![
//             RleRun { start: 0, len: 0 },
//             RleRun {
//                 start: 0,
//                 len: cell_count as u32,
//             },
//         ])
//     }
//     pub fn is_solid(&self) -> bool {
//         self.0 == Self::new_solid(self.len()).0
//     }
//     pub fn len(&self) -> u32 {
//         self.0.last().unwrap().after_end()
//     }
//     pub fn count(&self) -> u32 {
//         self.0
//             .iter()
//             // Take only the even runs.
//             .skip(1)
//             .step_by(2)
//             .copied()
//             // And add up their lengths.
//             .map(RleRun::len)
//             .sum()
//     }
//     pub fn set_run(&mut self, range: Range<u32>, include: bool) {
//         let Range { start, end } = range;
//         todo!()
//     }
//     pub fn into_cell_iter(self) -> impl Iterator<Item = bool> {
//         self.0
//             .into_iter()
//             .enumerate()
//             .flat_map(|(i, RleRun { len, .. })| std::iter::repeat(i % 2 == 1).take(len as usize))
//     }
//     fn simplify(&mut self) {
//         // let source_idx = 1;
//         // let destination_idx = 1;
//         // while source_idx < self.0.len() {
//         //     if self.0[source_idx].len == 0 {}
//         //     source_idx += 1;
//         //     destination_idx += 1;
//         // }
//         // TODO: eliminate zero-length runs and merge their neighbors
//         todo!()
//     }
// }
