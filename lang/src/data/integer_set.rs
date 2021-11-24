use codemap::Span;
use itertools::Itertools;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::errors::{Error, Result};

use super::{LangInt, MAX_INTEGER_SET_SIZE};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct IntegerSet {
    bounds: Option<(LangInt, LangInt)>,
    mask: Option<Vec<bool>>,
}
impl fmt::Display for IntegerSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((min, max)) = self.bounds() {
            if self.mask().is_some() {
                write!(f, "{{{}}}", self.iter().map(|i| i.to_string()).join(", "))
            } else {
                write!(f, "{}..{}", min, max)
            }
        } else {
            write!(f, "intset()")
        }
    }
}
impl Hash for IntegerSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        todo!()
    }
}
impl IntegerSet {
    /// Constructs an empty integer set.
    pub fn empty() -> Self {
        Self::default()
    }
    /// Constructs an integer set containing a single integer.
    pub fn single_integer(i: LangInt) -> Self {
        Self {
            bounds: Some((i, i)),
            mask: None,
        }
    }
    /// Constructs an integer set from a list of integers.
    pub fn from_list(span: Span, xs: &[LangInt]) -> Result<Self> {
        let min = xs.iter().min();
        let max = xs.iter().max();
        if let Some((min, max)) = min.zip(max) {
            let mut ret = Self::range(span, *min, *max)?;
            let mut mask = vec![false; ret.bounds_len() as usize];
            for x in xs {
                mask[(x - min) as usize] = true;
            }
            ret.mask = Some(mask);
            Ok(ret.canonicalize())
        } else {
            Ok(Self::empty())
        }
    }
    /// Constructs a contiguous integer set.
    pub fn range(span: Span, a: LangInt, b: LangInt) -> Result<Self> {
        let min = std::cmp::min(a, b);
        let max = std::cmp::max(a, b);
        let bounds = Some((min, max));
        check_integer_set_bounds(span, bounds)?;
        Ok(Self { bounds, mask: None })
    }
    /// Constructs an integer set using a predicate.
    pub fn with_mask_from_fn(
        span: Span,
        bounds: Option<(LangInt, LangInt)>,
        mask_fn: impl FnMut(LangInt) -> bool,
    ) -> Result<Self> {
        check_integer_set_bounds(span, bounds)?;
        let mask = bounds.map(|(min, max)| (min..=max).map(mask_fn).collect());
        Ok(Self { bounds, mask }.canonicalize())
    }
    /// Filters the integer set using a predicate.
    pub fn filter(&self, span: Span, mut filter_fn: impl FnMut(LangInt) -> bool) -> Result<Self> {
        Self::with_mask_from_fn(span, self.bounds(), |i| self.contains(i) && filter_fn(i))
    }
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn canonicalize(self) -> Self {
        if let Some((old_min, _old_max)) = self.bounds() {
            if let Some(mask) = self.mask() {
                let i = mask.iter().enumerate().find(|(_, b)| **b).map(|(i, _)| i);
                let j = mask.iter().enumerate().rfind(|(_, b)| **b).map(|(i, _)| i);
                if let Some((i, j)) = i.zip(j) {
                    let bounds = Some((old_min + i as LangInt, old_min + j as LangInt));
                    let mask = if mask[i..=j].iter().all(|&b| b) {
                        None
                    } else {
                        Some(mask[i..=j].to_vec())
                    };
                    Self { bounds, mask }
                } else {
                    Self::empty()
                }
            } else {
                // If the set is equivalent to its bounding range, it is already
                // canonical.
                self
            }
        } else {
            Self::empty()
        }
    }

    /// Returns an iterator over all the integers in the set.
    pub fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = LangInt> {
        (&self).into_iter()
    }

    /// Returns the minimum bounding range of the integer set, or `None` if the
    /// set is empty.
    pub fn bounds(&self) -> Option<(LangInt, LangInt)> {
        self.bounds
    }
    /// Returns the length of the minimum bounding range of the integer set.
    pub fn bounds_len(&self) -> usize {
        match self.bounds() {
            Some((min, max)) => (max - min + 1) as usize,
            None => 0,
        }
    }
    /// Returns the mask with length matching the size of `bounds`, or `None` if
    /// the set is empty or the whole range.
    pub fn mask(&self) -> Option<&[bool]> {
        self.mask.as_ref().map(Vec::as_slice)
    }
    /// Returns whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.bounds.is_none()
    }
    /// Returns the number of integers in the set.
    pub fn len(&self) -> usize {
        if let Some(mask) = self.mask() {
            mask.iter().filter(|&&in_| in_).count()
        } else {
            self.bounds_len()
        }
    }
    /// Returns whether the set contains an integer.
    pub fn contains(&self, i: LangInt) -> bool {
        match self.bounds() {
            None => false,
            Some((min, max)) => {
                (min..=max).contains(&i)
                    && match self.mask() {
                        None => true,
                        Some(mask) => mask[(i - min) as usize],
                    }
            }
        }
    }

    /// Returns the set containing all integers present in both `self` and
    /// `other`.
    pub fn intersection(&self, span: Span, other: &Self) -> Result<Self> {
        let bounds = self.bounds_intersection(other);
        Self::with_mask_from_fn(span, bounds, |i| self.contains(i) && other.contains(i))
    }
    fn bounds_intersection(&self, other: &Self) -> Option<(LangInt, LangInt)> {
        self.bounds()
            .zip(other.bounds())
            .map(|((min1, max1), (min2, max2))| {
                (std::cmp::max(min1, min2), std::cmp::min(max1, max2))
            })
    }

    /// Returns the set containing all integers present in `self` or `other`.
    pub fn union(&self, span: Span, other: &Self) -> Result<Self> {
        let bounds = self.bounds_union(other);
        Self::with_mask_from_fn(span, bounds, |i| self.contains(i) || other.contains(i))
    }
    fn bounds_union(&self, other: &Self) -> Option<(LangInt, LangInt)> {
        self.bounds()
            .zip(other.bounds())
            .map(|((min1, max1), (min2, max2))| {
                (std::cmp::min(min1, min2), std::cmp::max(max1, max2))
            })
    }

    /// Returns the set containing all integers present in `self` but not
    /// `other`.
    pub fn difference(&self, span: Span, other: &Self) -> Result<Self> {
        Self::with_mask_from_fn(span, self.bounds(), |i| {
            self.contains(i) && !other.contains(i)
        })
    }

    /// Returns the set containing all integers present in `self` or `other`,
    /// but not both.
    pub fn symmetric_difference(&self, span: Span, other: &Self) -> Result<Self> {
        let bounds = self.bounds_union(other);
        Self::with_mask_from_fn(span, bounds, |i| self.contains(i) ^ other.contains(i))
    }

    /// Offsets every integer in the set by a fixed delta.
    pub fn offset(&self, span: Span, delta: LangInt) -> Result<Self> {
        let mut ret = self.clone();
        if let Some((min, max)) = &mut ret.bounds {
            *min = min
                .checked_add(delta)
                .ok_or_else(|| Error::integer_overflow(span))?;
            *max = max
                .checked_add(delta)
                .ok_or_else(|| Error::integer_overflow(span))?;
        }
        Ok(ret)
    }
}

impl<'a> IntoIterator for &'a IntegerSet {
    type Item = LangInt;
    type IntoIter = Box<dyn 'a + Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        match self.bounds() {
            None => Box::new(std::iter::empty()),
            Some((min, max)) => match self.mask() {
                None => Box::new(min..=max),
                Some(mask) => Box::new(
                    (min..=max)
                        .zip_eq(mask)
                        .filter(|(_i, &in_set)| in_set)
                        .map(|(i, _in_set)| i),
                ),
            },
        }
    }
}

/// Checks that the bounds of the integer set are sufficiently small and returns
/// an error if they are not.
fn check_integer_set_bounds(span: Span, bounds: Option<(LangInt, LangInt)>) -> Result<()> {
    if let Some((min, max)) = bounds {
        if max.saturating_sub(min) > MAX_INTEGER_SET_SIZE as LangInt - 1 {
            return Err(Error::invalid_integer_set_size(span));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_set_ops() {
        let span = crate::utils::dummy_span();

        let a = IntegerSet::range(span, -3, 4).unwrap();
        assert_eq!(a.len(), 8);
        assert_eq!(a.to_string(), "-3..4");
        itertools::assert_equal(&a, -3..=4);

        let b = IntegerSet::with_mask_from_fn(span, Some((0, 10)), |i| i % 2 == 0).unwrap();
        assert_eq!(b.len(), 6);
        assert_eq!(b.to_string(), "{0, 2, 4, 6, 8, 10}");
        itertools::assert_equal(&b, (0..=10).filter(|&i| i % 2 == 0));

        {
            let and = a.intersection(span, &b).unwrap();
            assert_eq!(and.len(), 3);
            assert_eq!(and.to_string(), "{0, 2, 4}");
            itertools::assert_equal(&and, [0, 2, 4]);
        }
        {
            let or = a.union(span, &b).unwrap();
            assert_eq!(or.len(), 11);
            assert_eq!(or.to_string(), "{-3, -2, -1, 0, 1, 2, 3, 4, 6, 8, 10}");
            itertools::assert_equal(&or, [-3, -2, -1, 0, 1, 2, 3, 4, 6, 8, 10]);
        }
        {
            let diff = a.difference(span, &b).unwrap();
            assert_eq!(diff.len(), 5);
            assert_eq!(diff.to_string(), "{-3, -2, -1, 1, 3}");
            itertools::assert_equal(&diff, [-3, -2, -1, 1, 3]);
        }
        {
            let diff = b.difference(span, &a).unwrap();
            assert_eq!(diff.len(), 3);
            assert_eq!(diff.to_string(), "{6, 8, 10}");
            itertools::assert_equal(&diff, [6, 8, 10]);
        }
        {
            let xor = a.symmetric_difference(span, &b).unwrap();
            assert_eq!(xor.len(), 8);
            assert_eq!(xor.to_string(), "{-3, -2, -1, 1, 3, 6, 8, 10}");
            itertools::assert_equal(&xor, [-3, -2, -1, 1, 3, 6, 8, 10]);
        }

        let c = IntegerSet::range(span, 0, 10).unwrap();
        assert_eq!(c.len(), 11);
        assert_eq!(c.to_string(), "0..10");
        itertools::assert_equal(&c, 0..=10);

        {
            let and = a.intersection(span, &c).unwrap();
            assert_eq!(and.len(), 5);
            assert_eq!(and.to_string(), "0..4");
            itertools::assert_equal(&and, 0..=4);
        }
        {
            let or = a.union(span, &c).unwrap();
            assert_eq!(or.len(), 14);
            assert_eq!(or.to_string(), "-3..10");
            itertools::assert_equal(&or, -3..=10);
        }
        {
            let diff = a.difference(span, &c).unwrap();
            assert_eq!(diff.len(), 3);
            assert_eq!(diff.to_string(), "-3..-1");
            itertools::assert_equal(&diff, -3..=-1);
        }
        {
            let diff = c.difference(span, &a).unwrap();
            assert_eq!(diff.len(), 6);
            assert_eq!(diff.to_string(), "5..10");
            itertools::assert_equal(&diff, 5..=10);
        }
        {
            let xor = a.symmetric_difference(span, &c).unwrap();
            assert_eq!(xor.len(), 9);
            assert_eq!(xor.to_string(), "{-3, -2, -1, 5, 6, 7, 8, 9, 10}");
            itertools::assert_equal(&xor, [-3, -2, -1, 5, 6, 7, 8, 9, 10]);
        }
    }
}
