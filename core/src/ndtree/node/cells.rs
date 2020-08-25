//! Helper functions for manipulating power-of-2 hypercubes of cells.

use itertools::Itertools;

use super::Layer;
use crate::dim::Dim;

/// Subdivide a power-of-2 hypercube of cells into 2^NDIM hypercubes of half the
/// size. If the hypercube contains only one cell, returns `Err()` containing that cell.
///
/// # Panics
///
/// This function panics if the length of `cells` is not (2^n)^NDIM for some n.
pub fn subdivide<'a, D: Dim>(cells: &'a [u8]) -> Result<impl 'a + Iterator<Item = Box<[u8]>>, u8> {
    // Return early if there is only one cell.
    if let [cell] = cells {
        return Err(*cell);
    }

    let old_layer = Layer::from_num_cells::<D>(cells.len()).unwrap();
    let new_layer = old_layer.child_layer();

    let layer_1_rect = Layer(1).rect::<D>().unwrap();
    let old_strides = old_layer.leaf_strides();
    let new_rect = new_layer.rect().unwrap();
    Ok(layer_1_rect.iter().map(move |outer| {
        let child_base = outer * new_layer.len().unwrap();
        let child = (new_rect.clone() + child_base)
            .iter()
            .map(|pos| cells[(pos * old_strides.clone()).sum()])
            .collect_vec();
        debug_assert_eq!(child.len(), new_layer.num_cells::<D>().unwrap());
        child.into_boxed_slice()
    }))
}

/// Creates a power-of-2 hypercube of cells from 2^NDIM hypercubes of half the
/// size.
///
/// # Panics
///
/// This function panics if the length of `cells` is not (2^n)^NDIM for some n.
pub fn join<D: Dim>(children: &[&[u8]]) -> Box<[u8]> {
    assert_eq!(D::BRANCHING_FACTOR, children.len());
    let old_layer = Layer::from_num_cells::<D>(children[0].len()).unwrap();
    for child in &children[1..] {
        assert_eq!(child.len(), children[0].len())
    }
    let new_layer = old_layer.parent_layer();

    let layer_1_rect = Layer(1).rect::<D>().unwrap();
    let old_len = old_layer.len().unwrap();
    let mut ret = vec![0_u8; children.len() * children[0].len()];
    for (child, offset) in children.iter().zip(&layer_1_rect) {
        let offset = offset * old_len;
        for (&cell, new_pos) in child.iter().zip(&(old_layer.rect().unwrap() + offset)) {
            ret[(new_pos * new_layer.leaf_strides()).sum()] = cell;
        }
    }
    ret.into_boxed_slice()
}

/// Creates a (2^(n+1))^NDIM hypercube of cells from a (2^n)^NDIM hypercube of
/// cells by centering it and filling the edges with state 0. If the hypercube
/// contains only one cell, returns `Err()` containing that cell.
///
/// # Panics
///
/// This function panics if the length of `cells` is not (2^n)^NDIM for some n.
pub fn expand_centered<D: Dim>(cells: &[u8]) -> Result<Box<[u8]>, u8> {
    // Return early if there is only one cell.
    if let [cell] = cells {
        return Err(*cell);
    }

    let old_layer = Layer::from_num_cells::<D>(cells.len()).unwrap();
    let new_layer = old_layer.parent_layer();

    let offset = new_layer.len().unwrap() / 4;
    let old_rect = old_layer.rect::<D>().unwrap();
    let mut ret = vec![0_u8; new_layer.num_cells::<D>().unwrap()].into_boxed_slice();
    for (&cell, new_pos) in cells.iter().zip(&(old_rect + offset)) {
        ret[(new_pos * new_layer.leaf_strides()).sum()] = cell;
    }
    Ok(ret)
}

/// Creates a (2^(n-1))^NDIM hypercube of cells from a (2^n)^NDIM hypercube of
/// cells using the center 2^n. If the hypercube is smaller than 4^NDIM, returns
/// `Err(())`.
///
/// # Panics
///
/// This function panics if the length of `cells` is not (2^n)^NDIM for some n.
pub fn shrink_centered<D: Dim>(cells: &[u8]) -> Result<Box<[u8]>, ()> {
    let old_layer = Layer::from_num_cells::<D>(cells.len()).unwrap();
    if old_layer < Layer(2) {
        return Err(());
    }
    let new_layer = old_layer.child_layer();

    let offset = old_layer.len().unwrap() / 4;
    let new_rect = new_layer.rect::<D>().unwrap();
    let mut ret = vec![0_u8; new_layer.num_cells::<D>().unwrap()].into_boxed_slice();
    for (new_cell, old_pos) in ret.iter_mut().zip(&(new_rect + offset)) {
        *new_cell = cells[(old_pos * old_layer.leaf_strides()).sum()];
    }
    Ok(ret)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::dim::{Dim2D, Dim3D};

    /// Tests `cells::subdivide()`.
    #[test]
    fn test_cells_subdivide() {
        // Test 1x1x1 (should fail).
        assert_eq!(
            Err(60),
            subdivide::<Dim3D>(&[60]).map(|iter| iter.collect_vec()),
        );

        // Test 2x2x2.
        assert_eq!(
            (0..8).map(|x| vec![x]).collect_vec(),
            subdivide::<Dim3D>(&(0..8).collect_vec())
                .unwrap()
                .map(|x| x.into_vec())
                .collect_vec(),
        );

        // Test 4x4x4.
        let octant = vec![00, 01, 04, 05, 16, 17, 20, 21];
        assert_eq!(
            vec![
                octant.clone(),
                octant.iter().map(|&x| x + 2).collect(),
                octant.iter().map(|&x| x + 8).collect(),
                octant.iter().map(|&x| x + 8 + 2).collect(),
                octant.iter().map(|&x| x + 32).collect(),
                octant.iter().map(|&x| x + 32 + 2).collect(),
                octant.iter().map(|&x| x + 32 + 8).collect(),
                octant.iter().map(|&x| x + 32 + 8 + 2).collect(),
            ],
            subdivide::<Dim3D>(&(0..64).collect_vec())
                .unwrap()
                .map(|x| x.into_vec())
                .collect_vec(),
        );

        // Test 8x8.
        let quadrant = vec![
            00, 01, 02, 03, //
            08, 09, 10, 11, //
            16, 17, 18, 19, //
            24, 25, 26, 27, //
        ];
        assert_eq!(
            vec![
                quadrant.clone(),
                quadrant.iter().map(|&x| x + 4).collect(),
                quadrant.iter().map(|&x| x + 32).collect(),
                quadrant.iter().map(|&x| x + 32 + 4).collect(),
            ],
            subdivide::<Dim2D>(&(0..64).collect_vec())
                .unwrap()
                .map(|x| x.into_vec())
                .collect_vec(),
        );
    }

    fn test_cells_join_single_case<D: Dim>(layer: Layer) {
        let num_cells = layer.num_cells::<D>().unwrap();
        if num_cells >= 256 {
            panic!("too many cells for testing");
        }
        let cells = (0..num_cells as u8).collect_vec().into_boxed_slice();
        let subdivided = subdivide::<D>(&cells).unwrap().collect_vec();
        assert_eq!(
            cells,
            join::<D>(&subdivided.iter().map(|x| &**x).collect_vec()),
        );
    }

    /// Tests `cells::join()`.
    #[test]
    fn test_cells_join() {
        // Test 2x2x2.
        test_cells_join_single_case::<Dim3D>(Layer(1));
        // Test 4x4x4.
        test_cells_join_single_case::<Dim3D>(Layer(2));
        // Test 8x8.
        test_cells_join_single_case::<Dim2D>(Layer(3));
    }

    /// Tests `cells::expand_centered()`.
    #[test]
    fn test_cells_expand_centered() {
        // Test 1x1x1 (should fail).
        assert_eq!(Err(60), expand_centered::<Dim3D>(&[60]));

        // Test 2x2x2.
        let mut expected = vec![0; 16];
        expected.extend_from_slice(&[
            0, 0, 0, 0, //
            0, 1, 2, 0, //
            0, 3, 4, 0, //
            0, 0, 0, 0, //
            //
            0, 0, 0, 0, //
            0, 5, 6, 0, //
            0, 7, 8, 0, //
            0, 0, 0, 0, //
        ]);
        expected.extend_from_slice(&[0; 16]);
        assert_eq!(
            Ok(expected.into_boxed_slice()),
            expand_centered::<Dim3D>(&(1..=8).collect_vec())
        );

        // Test 4x4x4.
        let layer = vec![
            00, 00, 00, 00, 00, 00, 00, 00, //
            00, 00, 00, 00, 00, 00, 00, 00, //
            00, 00, 01, 02, 03, 04, 00, 00, //
            00, 00, 05, 06, 07, 08, 00, 00, //
            00, 00, 09, 10, 11, 12, 00, 00, //
            00, 00, 13, 14, 15, 16, 00, 00, //
            00, 00, 00, 00, 00, 00, 00, 00, //
            00, 00, 00, 00, 00, 00, 00, 00, //
        ];
        let mut expected = vec![0; 128];
        expected.extend(layer.iter().copied());
        expected.extend(layer.iter().map(|&x| if x == 0 { 0 } else { x + 16 }));
        expected.extend(layer.iter().map(|&x| if x == 0 { 0 } else { x + 32 }));
        expected.extend(layer.iter().map(|&x| if x == 0 { 0 } else { x + 48 }));
        expected.extend_from_slice(&[0; 128]);
        assert_eq!(
            Ok(expected.into_boxed_slice()),
            expand_centered::<Dim3D>(&(1..=64).collect_vec())
        );

        // Test 8x8.
        let row = vec![0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0];
        let mut expected = vec![0; 64];
        for i in 0..8 {
            expected.extend(row.iter().map(|&x| if x == 0 { 0 } else { x + i * 8 }));
        }
        expected.extend_from_slice(&[0; 64]);
        assert_eq!(
            Ok(expected.into_boxed_slice()),
            expand_centered::<Dim2D>(&(1..=64).collect_vec())
        );
    }

    fn test_cells_shrink_centered_single_case<D: Dim>(layer: Layer) {
        let num_cells = layer.num_cells::<D>().unwrap();
        if num_cells >= 256 {
            panic!("too many cells for testing");
        }
        let cells = (1..=num_cells as u8).collect_vec().into_boxed_slice();
        assert_eq!(
            cells,
            shrink_centered::<D>(&expand_centered::<D>(&*cells).unwrap()).unwrap(),
        );
    }

    /// Tests `cells::shrink_centered()`.
    #[test]
    fn test_cells_shrink_centered() {
        // Test 1x1x1 (should fail).
        assert_eq!(Err(()), shrink_centered::<Dim3D>(&[60]));

        // Test 2x2x2 (should fail).
        assert_eq!(Err(()), shrink_centered::<Dim3D>(&(0..8).collect_vec()));

        // Test 4x4x4.
        test_cells_shrink_centered_single_case::<Dim3D>(Layer(2));

        // Test 8x8.
        test_cells_shrink_centered_single_case::<Dim2D>(Layer(3));
    }
}
