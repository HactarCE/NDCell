use super::super::*;

/// A (hyper)rectangular region of cells.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RectRegion<D: Dim> {
    min: CellCoords<D>,
    max: CellCoords<D>,
}

impl<D: Dim> RectRegion<D> {
    /// Creates a RectRegion from a single cell.
    pub fn single_cell(pos: CellCoords<D>) -> Self {
        Self { min: pos, max: pos }
    }
    /// Creates a RectRegion spanning two corner cells.
    pub fn span(corner1: CellCoords<D>, corner2: CellCoords<D>) -> Self {
        let mut ret = Self {
            min: corner1,
            max: corner2,
        };
        ret.fix_min_max();
        ret
    }
    /// Construct a RectRegion centered on a given cell and with a given radius.
    pub fn centered(center: CellCoords<D>, radius: usize) -> Self {
        let mut min = center;
        let mut max = center;
        for ax in D::axes() {
            min[ax] -= radius as isize;
            max[ax] += radius as isize;
        }
        Self { min, max }
    }
    /// Get the most negative coordinates in the region.
    pub fn get_min(&self) -> CellCoords<D> {
        self.min
    }
    /// Get the most positive coordinates in the region.
    pub fn get_max(&self) -> CellCoords<D> {
        self.max
    }
    /// Replace the minimum coordinates in the region.
    pub fn set_min(&mut self, min: CellCoords<D>) {
        self.min = min;
        self.fix_min_max();
    }
    /// Replace the maximum coordinates in the region.
    pub fn set_max(&mut self, max: CellCoords<D>) {
        self.max = max;
        self.fix_min_max();
    }
    /// Swap minimum and maximum coordinates where necessary to maintain that
    /// self.min[ax] < self.max[ax] for all axes.
    fn fix_min_max(&mut self) {
        for ax in D::axes() {
            if self.min[ax] > self.max[ax] {
                std::mem::swap(&mut self.min[ax], &mut self.max[ax]);
            }
        }
    }
}

/// An iterator over all of the coordinates in a RectRegion.
pub struct RectRegionIter<D: Dim> {
    rect_region: RectRegion<D>,
    next_coords: CellCoords<D>,
    end: bool,
}

impl<D: Dim> Iterator for RectRegionIter<D> {
    type Item = CellCoords<D>;
    fn next(&mut self) -> Option<CellCoords<D>> {
        let coords = self.next_coords;
        let rect = self.rect_region;
        // If we've already hit the maximum, then there's nothing left.
        if self.end {
            return None;
        }
        // Basically, implement addition, pretending each axis is a digit.
        // For each digit ...
        for ax in D::axes() {
            // ... if it is below the maximum ...
            if coords[ax] < rect.max[ax] {
                // ... then increment this digit and return.
                self.next_coords[ax] += 1;
                return Some(coords);
            } else {
                // ... otherwise, overflow this digit and carry to the next one.
                self.next_coords[ax] = rect.min[ax];
            }
        }
        // We just tried to overflow the last digit, so we've hit the end.
        self.end = true;
        Some(coords)
    }
}

impl<D: Dim> IntoIterator for RectRegion<D> {
    type IntoIter = RectRegionIter<D>;
    type Item = CellCoords<D>;
    fn into_iter(self) -> RectRegionIter<D> {
        RectRegionIter {
            rect_region: self,
            next_coords: self.min,
            end: false,
        }
    }
}

impl<D: Dim> Region<D> for RectRegion<D> {
    fn contains(&self, coords: CellCoords<D>) -> bool {
        for ax in D::axes() {
            if coords[ax] < self.min[ax] || self.max[ax] < coords[ax] {
                return false;
            }
        }
        true
    }
    fn is_empty(&self) -> bool {
        // At a minimum, a RectRegion contains a single cell.
        false
    }
    fn len(&self) -> usize {
        let mut product: usize = 1;
        for ax in D::axes() {
            product *= (self.max[ax] - self.min[ax] + 1) as usize;
        }
        product
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::collections::HashSet;

    fn test_iter_validity<D: Dim>(rect: RectRegion<D>) {
        // Test that the iterator agrees with len().
        let cells: Vec<CellCoords<D>> = rect.into_iter().collect();
        assert_eq!(rect.len(), cells.len());
        // Test that there are no duplicates.
        {
            let cell_set: HashSet<CellCoords<D>> = rect.into_iter().collect();
            assert_eq!(rect.len(), cell_set.len());
        }
        for cell in cells {
            assert!(rect.contains(cell));
        }
    }

    proptest! {
        /// Test RectRegion::single_cell() and various methods.
        #[test]
        fn test_rect_region_single_cell(
            pos in cell_coords_strategy(-50..=50isize),
            offset in cell_coords_strategy(-4..=4isize),
        ) {
            let rect = RectRegion::single_cell(pos);
            // Test contains().
            assert!(rect.contains(pos));
            assert_eq!(offset.is_zero(), rect.contains(pos + offset));
            // Test is_empty().
            assert!(!rect.is_empty());
            // Test len().
            assert_eq!(1, rect.len());
            // Test iteration.
            test_iter_validity(rect);
        }

        /// Test RectRegion::span() and various methods.
        #[test]
        fn test_rect_region_span(
            corner1 in cell_coords_strategy(-5..=5isize),
            corner2 in cell_coords_strategy(-5..=5isize),
        ) {
            let rect = RectRegion::span(corner1, corner2);
            // There's no nice way to test contains() here; we'll leave that to
            // the other methods.
            // Test is_empty().
            assert!(!rect.is_empty());
            // Test len() and iteration.
            test_iter_validity(rect);
        }

        /// Test RectRegion::centered() and various methods.
        #[test]
        fn test_rect_region_centered(
            center in cell_coords_strategy(-50..=50isize),
            radius in 0..2usize,
            test_offset in cell_coords_strategy(-3..3isize),
        ) {
            let rect = RectRegion::centered(center, radius);
            // Test is_empty().
            assert!(!rect.is_empty());
            // Test contains()
            {
                let mut contains = true;
                for ax in Coords3D::axes() {
                    if test_offset[ax].abs() > radius as isize {
                        contains = false;
                    }
                }
                assert_eq!(contains, rect.contains(center + test_offset));
            }
            // Test len().
            assert_eq!((radius * 2 + 1).pow(3), rect.len());
            // Test len() and iteration.
            test_iter_validity(rect);
        }
    }
}
