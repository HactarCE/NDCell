use proptest::prelude::*;
use std::collections::HashMap;

use super::*;
use crate::axis::Axis::{X, Y};
use crate::ndrect::IRect;
use crate::ndvec::{IVec2D, NdVec};
use crate::num::BigUint;

fn assert_ndtree_valid(
    expected_cells: &HashMap<IVec2D, u8>,
    ndtree: &mut NdTree2D,
    cells_to_check: &Vec<IVec2D>,
) {
    let node_access = ndtree.node_access();
    assert_eq!(
        &BigUint::from(
            expected_cells
                .iter()
                .filter(|(_, &cell_state)| cell_state != 0)
                .count()
        ),
        ndtree.root().population()
    );
    for pos in cells_to_check {
        assert_eq!(
            *expected_cells.get(pos).unwrap_or(&0),
            ndtree.get_cell(&node_access, &pos.to_bigvec())
        );
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        max_shrink_iters: 4096,
        ..Default::default()
    })]

    /// Tests set_cell() and get_cell() by comparing against a HashMap.
    #[test]
    fn test_ndtree_set_get(
        cells_to_set: Vec<(IVec2D, u8)>,
        mut cells_to_get: Vec<IVec2D>,
    ) {
        let mut ndtree = NdTree::with_state_count(256);
        let mut hashmap = HashMap::new();
        let cache = ndtree.cache();
        let node_access = cache.node_access();
        for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&node_access, &pos.to_bigvec(), state);
            cells_to_get.push(pos);
        }
        assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
        // Test that expansion preserves population and positions.
        let old_layer = ndtree.layer();
        while ndtree.layer() < Layer(5) {
            ndtree.expand();
            assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
        }
        // Test that shrinking actually shrinks.
        ndtree.shrink();
        assert!(ndtree.layer() <= old_layer);
        // Test that shrinking preserves population and positions.
        assert_ndtree_valid(&hashmap, &mut ndtree, &cells_to_get);
    }

    /// Tests that NdTreeCache automatically caches identical nodes.

    // TODO: does this work consistently now?
    // #[ignore]
    #[test]
    fn test_ndtree_cache(
        cells_to_set: Vec<(IVec2D, u8)>,
    ) {
        prop_assume!(!cells_to_set.is_empty());
        let mut ndtree = NdTree::with_state_count(256);
        let cache = ndtree.cache();
        let node_access = cache.node_access();
        for (pos, state) in cells_to_set {
            ndtree.set_cell(&node_access, &(pos - 128).to_bigvec(), state);
            ndtree.set_cell(&node_access, &(pos + 128).to_bigvec(), state);
        }
        let slice = ndtree.slice(&node_access);
        let children = slice.subdivide().unwrap();
        let subnode1 = &children[0];
        let subnode2 = &children[children.len() - 1];
        assert_eq!(subnode1, subnode2);
        assert!(std::ptr::eq(subnode1.root.as_raw(), subnode2.root.as_raw()));
    }

    /// Tests NdTree::get_slice_containing().
    #[test]
    fn test_ndtree_get_slice_containing(
        cells_to_set: Vec<(IVec2D, u8)>,
        center: IVec2D,
        x_radius in 0..20_isize,
        y_radius in 0..20_isize,
    ) {
        let mut ndtree = NdTree::with_state_count(256);
        let mut hashmap = HashMap::new();
        let cache = ndtree.cache();
        let node_access = cache.node_access();
        for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&node_access, &pos.to_bigvec(), state);
        }
        let half_diag = NdVec([x_radius, y_radius]);
        let rect = IRect::span(center - half_diag, center + half_diag).to_bigrect();
        let slice = ndtree.get_slice_containing(&rect);
        let slice_rect = slice.rect();
        assert!(slice_rect.contains(&rect));
        assert!(
            slice.root.layer() == Layer(1)
            || slice.root.big_len() < rect.len(X) * 2
            || slice.root.big_len() < rect.len(Y) * 2
        );
        for (pos, state) in hashmap {
            if slice_rect.contains(&pos.to_bigvec()) {
                if let Some(cell_state) = slice.get_cell(&pos.to_bigvec()) {
                    assert_eq!(state, cell_state);
                }
            }
        }
    }
}
