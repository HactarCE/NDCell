use super::*;
use proptest::prelude::*;
use std::collections::HashSet;

fn test_iter_validity<D: Dim>(rect: IRect<D>) {
    // Test that the iterator agrees with count().
    let cells: Vec<IVec<D>> = rect.clone().into_iter().collect();
    assert_eq!(rect.count() as usize, cells.len());
    // Test that there are no duplicates.
    {
        let cell_set: HashSet<IVec<D>> = rect.clone().into_iter().collect();
        assert_eq!(rect.count() as usize, cell_set.len());
    }
    for pos in cells {
        assert!(rect.contains(&pos));
    }
}

proptest! {
    /// Tests NdRect::single_cell() and various methods.
    #[test]
    fn test_rect_region_single_cell(
        pos: IVec3D,
        offset in IVec3D::arbitrary_with(Some(4)),
    ) {
        let rect = NdRect::single_cell(pos);
        // Test contains().
        assert!(rect.contains(&pos));
        assert_eq!(offset.is_zero(), rect.contains(&pos + &offset));
        // Test min() and max().
        assert_eq!(pos, rect.min());
        assert_eq!(pos, rect.max());
        // Test size().
        assert_eq!(NdVec::one(), rect.size());
        // Test count().
        assert_eq!(1, rect.count());
        // Test iteration.
        test_iter_validity(rect);
    }

    /// Tests NdRect::span() and various methods.
    #[test]
    fn test_rect_region_span(
        corner1 in IVec3D::arbitrary_with(Some(5)),
        corner2 in IVec3D::arbitrary_with(Some(5)),
    ) {
        let rect = NdRect::span(corner1, corner2);
        // There's no nice way to test contains() here; we'll leave that to
        // the other methods.
        // Test count() and iteration.
        test_iter_validity(rect);
    }

    /// Tests NdRect::centered() and various methods.
    #[test]
    fn test_rect_region_centered(
        center: IVec3D,
        radius in 0..2_isize,
        test_offset in IVec3D::arbitrary_with(Some(3)),
    ) {
        let rect = NdRect::centered(center, radius);
        // Test contains()
        {
            let mut contains = true;
            for &ax in Dim3D::axes() {
                if test_offset[ax].abs() > radius as isize {
                    contains = false;
                }
            }
            assert_eq!(contains, rect.contains(&center + &test_offset));
        }
        // Test count().
        assert_eq!((radius * 2 + 1).pow(3), rect.count());
        // Test count() and iteration.
        test_iter_validity(rect);
    }

    /// Tests addition and subtract of NdRect and NdVec.
    #[test]
    fn test_rect_ops(
        corner1: IVec3D,
        corner2: IVec3D,
        offset: IVec3D,
    ) {
        assert_eq!(
            NdRect::span(corner1, corner2) + offset,
            NdRect::span(corner1 + offset, corner2 + offset),
        );
        assert_eq!(
            NdRect::span(corner1, corner2) - offset,
            NdRect::span(corner1 - offset, corner2 - offset),
        );
    }
}

#[cfg(test)]
mod tests;
