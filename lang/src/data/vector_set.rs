use codemap::Span;
use itertools::Itertools;
use std::fmt;

use ndcell_core::ndarray::Array6D;
use ndcell_core::prelude::{Axis, CanContain, Dim, Dim6D, IRect6D, IVec6D};

use super::{LangInt, MAX_VECTOR_SET_EXTENT, MAX_VECTOR_SET_SIZE};
use crate::errors::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VectorSet {
    /// Number of components in each vector; extra components are zero.
    vec_len: usize,
    /// Minimum rectangular bounding box of the vector set, or `None` if the set
    /// is empty.
    bounds: Option<IRect6D>,
    /// Mask with size matching the size of `bounds`, or `None` if the set is
    /// empty or the whole rectangle.
    mask: Option<Array6D<bool>>,
}
impl fmt::Display for VectorSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ndim = self.vec_len();
        match self.bounds() {
            None => write!(f, "VectorSet[{}].empty", ndim)?,
            Some(b) => {
                write!(
                    f,
                    "{}..{}",
                    crate::utils::display_bracketed_list(&b.min().0[..ndim]),
                    crate::utils::display_bracketed_list(&b.max().0[..ndim]),
                )?;
                if let Some(m) = self.mask() {
                    write!(f, " & \"")?;
                    for &in_ in m.as_flat_slice() {
                        write!(f, "{}", if in_ { '#' } else { '.' })?;
                    }
                    write!(f, "\"")?;
                }
            }
        }
        Ok(())
    }
}
impl VectorSet {
    /// Constructs an empty vector set.
    pub fn empty(span: Span, vec_len: usize) -> Result<Self> {
        check_vector_set_vec_len(span, vec_len)?;

        Self {
            vec_len,
            bounds: None,
            mask: None,
        }
        .canonicalize()
    }
    /// Constructs a vector set containing a single vector.
    pub fn single_vector(span: Span, vec: &[LangInt], vec_span: Span) -> Result<Self> {
        let vec_len = vec.len();
        check_vector_set_vec_len(span, vec_len)?;

        Self {
            vec_len,
            bounds: Some(IRect6D::single_cell(vec_to_ivec6d(vec_span, vec)?)),
            mask: None,
        }
        .canonicalize()
    }
    /// Constructs a rectangular vector set.
    pub fn rect(
        span: Span,
        a: &[LangInt],
        a_span: Span,
        b: &[LangInt],
        b_span: Span,
    ) -> Result<Self> {
        let vec_len = std::cmp::max(a.len(), b.len());
        check_vector_set_vec_len(span, vec_len)?;

        let a = vec_to_ivec6d(a_span, a)?;
        let b = vec_to_ivec6d(b_span, b)?;
        let bounds = Some(IRect6D::span(a, b));
        check_vector_set_bounds(span, bounds)?;
        Self {
            vec_len,
            bounds,
            mask: None,
        }
        .canonicalize()
    }

    /// Constructs a vector set using a predicate.
    pub fn with_mask_from_fn(
        span: Span,
        vec_len: usize,
        bounds: Option<IRect6D>,
        mask_fn: impl FnMut(IVec6D) -> bool,
    ) -> Result<Self> {
        check_vector_set_vec_len(span, vec_len)?;

        check_vector_set_bounds(span, bounds)?;
        let mask = bounds.map(|b| mask_from_fn(b, mask_fn));
        Self {
            vec_len,
            bounds,
            mask,
        }
        .canonicalize()
    }
    /// Filters the vector set using a predicate.
    pub fn filter(&self, span: Span, mut filter_fn: impl FnMut(IVec6D) -> bool) -> Result<Self> {
        Self::with_mask_from_fn(span, self.vec_len(), self.bounds(), |pos| {
            self.contains(pos) && filter_fn(pos)
        })
    }
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn canonicalize(self) -> Result<Self> {
        if let Some(b) = self.bounds() {
            // Check that extra dimensions are unused.
            for &ax in &Dim6D::axes()[self.vec_len()..] {
                if b.min()[ax] != 0 || b.size()[ax] != 1 {
                    internal_error!("incorrect vector length for vector set")
                }
            }
        }

        if self.mask().is_none() {
            // If the set is equivalent to its bounding rectangle, then it is
            // already canonical.
            return Ok(self);
        }

        // Find lower and upper bounds along each dimension. This is O(n^d)
        // where it could be O(d*n^(d-1)) in the best case by only checking
        // cells on the exterior, but oh well.
        let new_bounds = bounds_from_list(self.iter());

        let mut new_mask = if self.bounds() == new_bounds {
            // The bounds haven't changed, so use the old mask.
            self.mask
        } else if let Some(b) = new_bounds {
            // Create a new mask that contains the same vectors.
            Some(mask_from_fn(b, |pos| self.contains(pos)))
        } else {
            // There are no bounds, so there is no mask.
            None
        };
        if let Some(m) = &new_mask {
            if m.as_flat_slice().iter().all(|&x| x) {
                // If the whole rectangle is included, no mask is necessary.
                new_mask = None;
            }
        }

        Ok(Self {
            vec_len: self.vec_len,
            bounds: new_bounds,
            mask: new_mask,
        })
    }

    /// Constructs a Moore neighborhood.
    pub fn moore(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        check_vector_set_vec_len(span, ndim)?;

        if radius < 0 {
            Self::empty(span, ndim)
        } else {
            Self::rect(
                span,
                &[-radius; 6][..ndim],
                radius_span,
                &[radius; 6][..ndim],
                radius_span,
            )
        }
    }
    /// Constructs a von Neumann neighborhood.
    pub fn vn(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        Self::moore(span, ndim, radius, radius_span)?
            .filter(span, |pos| pos.abs().sum() <= radius as isize)
    }
    /// Constructs a circular neighborhood. `x² + y² <= r² + r`
    pub fn circular(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        let rhs = radius
            .checked_mul(radius)
            .and_then(|r_squared| r_squared.checked_add(radius));
        Self::generic_circular(span, ndim, radius, radius_span, rhs)
    }
    /// Constructs an L² neighborhood. `x² + y² <= r²`
    pub fn l2(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        let rhs = radius.checked_mul(radius);
        Self::generic_circular(span, ndim, radius, radius_span, rhs)
    }
    fn generic_circular(
        span: Span,
        ndim: usize,
        radius: LangInt,
        radius_span: Span,
        rhs: Option<LangInt>,
    ) -> Result<Self> {
        match rhs {
            None => Self::empty(span, ndim),
            Some(rhs) => Self::moore(span, ndim, radius, radius_span)?.filter(span, |pos| {
                pos.0.iter().map(|x| x * x).sum::<isize>() as LangInt <= rhs
            }),
        }
    }
    /// Constructs a checkerboard Moore neighborhood.
    pub fn checkerboard(
        span: Span,
        ndim: usize,
        radius: LangInt,
        radius_span: Span,
    ) -> Result<Self> {
        let f = |pos: IVec6D| pos.sum() % 2 != 0 || pos.is_zero();
        Self::moore(span, ndim, radius, radius_span)?.filter(span, f)
    }
    /// Constructs a 2D hash neighborhoood.
    pub fn hash(span: Span, radius: LangInt, radius_span: Span) -> Result<Self> {
        use Axis::{X, Y};
        let f = |pos: IVec6D| pos[X].abs() == 1 || pos[Y].abs() == 1 || pos.is_zero();
        Self::moore(span, 2, radius, radius_span)?.filter(span, f)
    }
    /// Constructs a cross neighborhood.
    pub fn cross(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        let f = |pos: IVec6D| pos.0.iter().filter(|&&x| x != 0).count() <= 1;
        Self::moore(span, ndim, radius, radius_span)?.filter(span, f)
    }
    /// Constructs a saltire neighborhood.
    pub fn saltire(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        let f = |pos: IVec6D| pos.abs().0[..ndim].iter().all_equal();
        Self::moore(span, ndim, radius, radius_span)?.filter(span, f)
    }
    /// Constructs a star neighborhood.
    pub fn star(span: Span, ndim: usize, radius: LangInt, radius_span: Span) -> Result<Self> {
        let f = |pos: IVec6D| pos.abs().0.iter().filter(|&&x| x != 0).all_equal();
        Self::moore(span, ndim, radius, radius_span)?.filter(span, f)
    }

    /// Returns an iterator over all the vectors in the set.
    pub fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = IVec6D> {
        (&self).into_iter()
    }

    /// Returns the length of each vector in the set.
    pub fn vec_len(&self) -> usize {
        self.vec_len
    }
    /// Returns the minimum rectangular bounding box of the vector set, or
    /// `None` if the set is empty.
    pub fn bounds(&self) -> Option<IRect6D> {
        self.bounds
    }
    /// Returns the mask with size matching the size of `bounds`, or `None` if
    /// the set is empty or the whole rectangle.
    pub fn mask(&self) -> &Option<Array6D<bool>> {
        &self.mask
    }
    /// Returns whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.bounds().is_none()
    }
    /// Returns the number of vectors in the set.
    pub fn len(&self) -> usize {
        if let Some(mask) = self.mask() {
            mask.as_flat_slice().iter().filter(|&&in_| in_).count()
        } else if let Some(rect) = self.bounds() {
            rect.count() as usize
        } else {
            0
        }
    }
    /// Returns whether the set contains a vector.
    pub fn contains(&self, pos: IVec6D) -> bool {
        match self.bounds() {
            None => false,
            Some(b) => {
                b.contains(&pos)
                    && match self.mask() {
                        None => true,
                        Some(m) => m[(pos - b.min()).to_uvec()],
                    }
            }
        }
    }

    /// Returns the set containing all vectors present in both `self` and
    /// `other`.
    pub fn intersection(&self, span: Span, other: &Self) -> Result<Self> {
        let vec_len = std::cmp::min(self.vec_len(), other.vec_len());
        let bounds = self.bounds_intersection(other);
        VectorSet::with_mask_from_fn(span, vec_len, bounds, |pos| {
            self.contains(pos) && other.contains(pos)
        })
    }
    fn bounds_intersection(&self, other: &Self) -> Option<IRect6D> {
        IRect6D::intersection(&self.bounds()?, &other.bounds()?)
    }

    /// Returns the set containing all vectors present in `self` or `other`.
    pub fn union(&self, span: Span, other: &Self) -> Result<Self> {
        let vec_len = std::cmp::max(self.vec_len(), other.vec_len());
        let bounds = self.bounds_union(other);
        Self::with_mask_from_fn(span, vec_len, bounds, |pos| {
            self.contains(pos) || other.contains(pos)
        })
    }
    fn bounds_union(&self, other: &Self) -> Option<IRect6D> {
        match (self.bounds(), other.bounds()) {
            (Some(a), Some(b)) => Some(IRect6D::span_rects(&a, &b)),
            (a, b) => a.or(b),
        }
    }

    /// Returns the set containing all vectors present in `self` but not
    /// `other`.
    pub fn difference(&self, span: Span, other: &Self) -> Result<Self> {
        Self::with_mask_from_fn(span, self.vec_len(), self.bounds(), |pos| {
            self.contains(pos) && !other.contains(pos)
        })
    }

    /// Returns the set containing all vectors present in `self` or `other`, but
    /// not both.
    pub fn symmetric_difference(&self, span: Span, other: &Self) -> Result<Self> {
        let vec_len = std::cmp::max(self.vec_len(), other.vec_len());
        let bounds = self.bounds_union(other);
        Self::with_mask_from_fn(span, vec_len, bounds, |pos| {
            self.contains(pos) ^ other.contains(pos)
        })
    }
}

impl<'a> IntoIterator for &'a VectorSet {
    type Item = IVec6D;
    type IntoIter = Box<dyn 'a + Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        match self.bounds() {
            None => Box::new(std::iter::empty()),
            Some(b) => match self.mask() {
                None => Box::new(b.iter()),
                Some(m) => Box::new(
                    b.iter()
                        .zip_eq(m.as_flat_slice())
                        .filter(|(_pos, &in_set)| in_set)
                        .map(|(pos, _in_set)| pos),
                ),
            },
        }
    }
}

/// Checks whether a vector set can be constructed with the given vector length
/// and returns an error if it cannot.
fn check_vector_set_vec_len(span: Span, vec_len: usize) -> Result<()> {
    match (1..=6).contains(&vec_len) {
        true => Ok(()),
        false => Err(Error::invalid_vector_length_for_set(span, vec_len)),
    }
}

/// Checks that the bounds of the vector set are sufficiently small and returns
/// an error if they are not.
fn check_vector_set_bounds(span: Span, bounds: Option<IRect6D>) -> Result<()> {
    if let Some(b) = bounds {
        let min = b.min();
        let max = b.max();

        // There's got to be a more elegant way to write this function.
        let mut size = 1_usize;
        for &ax in Dim6D::axes() {
            check_vector_component(span, min[ax])?;
            check_vector_component(span, max[ax])?;

            match size.checked_mul(b.len(ax) as usize) {
                Some(i) if i <= MAX_VECTOR_SET_SIZE => size = i,
                _ => return Err(Error::invalid_vector_set_size(span)),
            }
        }
    }
    Ok(())
}

/// Constructs the mask for a vector set by evaluating a function on each
/// position within the bounding rectangle.
fn mask_from_fn(bounds: IRect6D, mask_fn: impl FnMut(IVec6D) -> bool) -> Array6D<bool> {
    Array6D::from_flat_slice(
        bounds.size().to_uvec(),
        bounds.iter().map(mask_fn).collect_vec(),
    )
}

/// Returns the minimum bounding rectangle for a list of positions, or `None` if the list is empty.
fn bounds_from_list(positions: impl IntoIterator<Item = IVec6D>) -> Option<IRect6D> {
    positions
        .into_iter()
        .map(IRect6D::single_cell)
        .reduce(|r1, r2| IRect6D::span_rects(&r1, &r2))
}

fn vec_to_ivec6d(span: Span, v: &[LangInt]) -> Result<IVec6D> {
    // Check that all values are within range.
    for &i in v {
        check_vector_component(span, i as isize)?;
    }

    Ok(IVec6D::from_fn(|ax| {
        v.get(ax as usize).map(|&i| i as isize).unwrap_or(0)
    }))
}
fn ivec6d_to_vec(vec_len: usize, v: IVec6D) -> Vec<LangInt> {
    (0..vec_len.clamp(1, 6))
        .map(|i| v.0[i] as LangInt)
        .collect()
}

/// Checks that a vector component is sufficiently small and returns an error if
/// it is not.
fn check_vector_component(span: Span, i: isize) -> Result<()> {
    match is_vector_component_valid(i as isize) {
        true => Ok(()),
        false => Err(Error::invalid_vector_component_for_set(span)),
    }
}
/// Returns whether a vector component is sufficiently small.
fn is_vector_component_valid(i: isize) -> bool {
    (-MAX_VECTOR_SET_EXTENT..MAX_VECTOR_SET_EXTENT).contains(&i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_set_ops() {
        let span = crate::utils::nonsense_span();

        let a = VectorSet::rect(span, &[0, -3], span, &[4], span).unwrap();

        let b = {
            let moore = VectorSet::moore(span, 3, 1, span).unwrap();
            let circ = VectorSet::circular(span, 3, 1, span).unwrap();
            moore.difference(span, &circ).unwrap()
        };
        assert_eq!(b.len(), 8); // corner cells of a 3x3x3 cube

        let a_or_b = a.union(span, &b).unwrap();
        assert_eq!(a_or_b, b.union(span, &a).unwrap());
        assert!(!a_or_b.is_empty());
        assert_eq!(
            a_or_b.to_string(),
            "[-1, -3, -1]..[4, 1, 1] & \"\
                ......\
                ......\
                #.#...\
                ......\
                #.#...\
                \
                .#####\
                .#####\
                .#####\
                .#####\
                ......\
                \
                ......\
                ......\
                #.#...\
                ......\
                #.#...\
            \"",
        );

        let a_and_b = a.intersection(span, &b).unwrap();
        assert_eq!(a_and_b, b.intersection(span, &a).unwrap());
        assert!(a_and_b.is_empty());
        assert_eq!(a_and_b.to_string(), "VectorSet[2].empty");

        let a_minus_b = a.difference(span, &b).unwrap();
        assert!(!a_minus_b.is_empty());
        assert_eq!(a_minus_b, a);

        let b_minus_a = b.difference(span, &a).unwrap();
        assert!(!b_minus_a.is_empty());
        assert_eq!(b_minus_a, b);

        let c = VectorSet::moore(span, 2, 0, span)
            .unwrap()
            .union(span, &b)
            .unwrap();

        let a_or_c = a.union(span, &c).unwrap();
        assert_eq!(a_or_c, c.union(span, &a).unwrap());
        assert_eq!(a_or_c, a_or_b);

        let a_and_c = a.intersection(span, &c).unwrap();
        assert_eq!(a_and_c, c.intersection(span, &a).unwrap());
        assert!(!a_and_c.is_empty());
        assert_eq!(a_and_c.to_string(), "[0, 0]..[0, 0]");

        let a_minus_c = a.difference(span, &c).unwrap();
        assert!(!a_minus_c.is_empty());
        assert_eq!(
            a_minus_c.to_string(),
            "[0, -3]..[4, 0] & \"\
                #####\
                #####\
                #####\
                .####\
            \"",
        );

        let c_minus_a = c.difference(span, &a).unwrap();
        assert!(!c_minus_a.is_empty());
        assert_eq!(c_minus_a, b);
    }

    #[test]
    fn test_vector_set_neighborhood_shapes_2d() {
        let span = crate::utils::nonsense_span();

        // Test Moore neighborhood.
        let nbhd = VectorSet::moore(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 49);
        assert_eq!(nbhd.to_string(), "[-3, -3]..[3, 3]");

        // Test von Neumann neighborhood.
        let nbhd = VectorSet::vn(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 25);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                ...#...\
                ..###..\
                .#####.\
                #######\
                .#####.\
                ..###..\
                ...#...\
            \"",
        );

        // Test circular neighborhood.
        let nbhd = VectorSet::circular(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 37);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                ..###..\
                .#####.\
                #######\
                #######\
                #######\
                .#####.\
                ..###..\
            \"",
        );

        // Test L² neighborhood.
        let nbhd = VectorSet::l2(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 29);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                ...#...\
                .#####.\
                .#####.\
                #######\
                .#####.\
                .#####.\
                ...#...\
            \"",
        );

        // Test checkerboard neighborhood.
        let nbhd = VectorSet::checkerboard(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 25);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                .#.#.#.\
                #.#.#.#\
                .#.#.#.\
                #.###.#\
                .#.#.#.\
                #.#.#.#\
                .#.#.#.\
            \"",
        );

        // Test hash neighborhood.
        let nbhd = VectorSet::hash(span, 3, span).unwrap();
        assert_eq!(nbhd.len(), 25);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                ..#.#..\
                ..#.#..\
                #######\
                ..###..\
                #######\
                ..#.#..\
                ..#.#..\
            \"",
        );

        // Test cross neighborhood.
        let nbhd = VectorSet::cross(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 13);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                ...#...\
                ...#...\
                ...#...\
                #######\
                ...#...\
                ...#...\
                ...#...\
            \"",
        );

        // Test saltire neighborhood.
        let nbhd = VectorSet::saltire(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 13);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                #.....#\
                .#...#.\
                ..#.#..\
                ...#...\
                ..#.#..\
                .#...#.\
                #.....#\
            \"",
        );

        // Test star neighborhood.
        let nbhd = VectorSet::star(span, 2, 3, span).unwrap();
        assert_eq!(nbhd.len(), 25);
        assert_eq!(
            nbhd.to_string(),
            "[-3, -3]..[3, 3] & \"\
                #..#..#\
                .#.#.#.\
                ..###..\
                #######\
                ..###..\
                .#.#.#.\
                #..#..#\
            \"",
        );
    }

    #[test]
    fn test_vector_set_neighborhood_shapes_3d() {
        let span = crate::utils::nonsense_span();

        // Test Moore neighborhood.
        let nbhd = VectorSet::moore(span, 3, 2, span).unwrap();
        assert_eq!(nbhd.len(), 125);
        assert_eq!(nbhd.to_string(), "[-2, -2, -2]..[2, 2, 2]");

        // Test von Neumann neighborhood.
        let nbhd = VectorSet::vn(span, 3, 2, span).unwrap();
        assert_eq!(nbhd.len(), 25);
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
                ............#............\
                .......#...###...#.......\
                ..#...###.#####.###...#..\
                .......#...###...#.......\
                ............#............\
            \"",
        );

        // Test circular neighborhood.
        let nbhd = VectorSet::circular(span, 3, 2, span).unwrap();
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
                ......###..###..###......\
                .###.###############.###.\
                .###.###############.###.\
                .###.###############.###.\
                ......###..###..###......\
            \"",
        );

        // Test L² neighborhood.
        let nbhd = VectorSet::l2(span, 3, 2, span).unwrap();
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
                ............#............\
                ......###..###..###......\
                ..#...###.#####.###...#..\
                ......###..###..###......\
                ............#............\
            \"",
        );

        // Test checkerboard neighborhood.
        let nbhd = VectorSet::checkerboard(span, 3, 2, span).unwrap();
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
                .#.#.#.#.#.#.#.#.#.#.#.#.\
                #.#.#.#.#.#.#.#.#.#.#.#.#\
                .#.#.#.#.#.###.#.#.#.#.#.\
                #.#.#.#.#.#.#.#.#.#.#.#.#\
                .#.#.#.#.#.#.#.#.#.#.#.#.\
            \"",
        );

        // Test cross neighborhood.
        let nbhd = VectorSet::cross(span, 3, 2, span).unwrap();
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
                ............#............\
                ............#............\
                ..#....#..#####..#....#..\
                ............#............\
                ............#............\
            \"",
        );

        // Test saltire neighborhood.
        let nbhd = VectorSet::saltire(span, 3, 2, span).unwrap();
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
                #...#...............#...#\
                ......#.#.......#.#......\
                ............#............\
                ......#.#.......#.#......\
                #...#...............#...#\
            \"",
        );

        // Test star neighborhood.
        let nbhd = VectorSet::star(span, 3, 2, span).unwrap();
        assert_eq!(
            nbhd.to_string(),
            "[-2, -2, -2]..[2, 2, 2] & \"\
            #.#.#.....#.#.#.....#.#.#\
            ......###..###..###......\
            #.#.#.###.#####.###.#.#.#\
            ......###..###..###......\
            #.#.#.....#.#.#.....#.#.#\
            \"",
        );
    }
}
