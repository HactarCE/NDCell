use super::super::*;

/// A (hyper)rectangular region of cells.
#[derive(Copy, Clone)]
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
    pub fn range(corner1: CellCoords<D>, corner2: CellCoords<D>) -> Self {
        let mut ret = Self {
            min: corner1,
            max: corner2,
        };
        ret.fix_min_max();
        ret
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
                self.next_coords[ax] = 0;
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
