use proptest::prelude::*;
use std::collections::HashSet;

use super::*;
use crate::axis::{X, Y};
use crate::ndrect::{proptest_irect2d, IRect2D};
use crate::ndvec::{proptest_ivec2d, IVec2D, NdVec};
use crate::num::BigUint;
use crate::HashMap;

fn assert_ndtree_valid(
    expected_cells: &HashMap<IVec2D, u8>,
    ndtree: &NdTree2D,
    cells_to_check: &Vec<IVec2D>,
) {
    assert_eq!(
        BigUint::from(
            expected_cells
                .iter()
                .filter(|(_, &cell_state)| cell_state != 0)
                .count()
        ),
        ndtree.root_ref().population()
    );
    for pos in cells_to_check {
        assert_eq!(
            *expected_cells.get(pos).unwrap_or(&0),
            ndtree.get_cell(&pos.to_bigvec())
        );
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        max_shrink_iters: 4096,
        ..Default::default()
    })]

    /// Tests `NdTree::set_cell()` and `NdTree::get_cell()` by comparing against
    /// a HashMap.
    #[test]
    fn test_ndtree_set_get(
        cells_to_set in proptest_cells_to_set(),
        mut cells_to_get in proptest_cells_to_get(),
    ) {
        let mut ndtree = NdTree::default();
        let mut hashmap = HashMap::default();
        for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&pos.to_bigvec(), state);
            cells_to_get.push(pos);
        }
        assert_ndtree_valid(&hashmap, &ndtree,  &cells_to_get);
        // Test that expansion preserves population and positions.
        let old_layer = ndtree.layer();
        while ndtree.layer() < Layer(5) {
            ndtree.expand();
            assert_ndtree_valid(&hashmap, &ndtree,  &cells_to_get);
        }
        // Test that shrinking actually shrinks.
        ndtree.shrink();
        assert!(ndtree.layer() <= old_layer);
        // Test that shrinking preserves population and positions.
        assert_ndtree_valid(&hashmap, &ndtree,  &cells_to_get);
    }

    /// Tests that identical nodes use the same underlying structure.
    #[test]
    fn test_ndtree_node_pool(
        cells_to_set in proptest_cells_to_set(),
    ) {
        prop_assume!(!cells_to_set.is_empty());

        let mut ndtree = NdTree::default();
        for (pos, state) in cells_to_set {
            ndtree.set_cell(&(pos - 128).to_bigvec(), state);
            ndtree.set_cell(&(pos + 128).to_bigvec(), state);
        }
        let slice = ndtree.as_slice();
        let children = slice.subdivide().unwrap();
        let subnode1 = &children[0].root;
        let subnode2 = &children[children.len() - 1].root;
        assert_eq!(subnode1, subnode2);
    }

    /// Tests `NdTree::slice_containing()`.
    #[test]
    fn test_ndtree_slice_containing(
        cells_to_set in proptest_cells_to_set(),
        center in proptest_ivec2d(-100..=100),
        x_radius in 0..20_isize,
        y_radius in 0..20_isize,
    ) {
        let mut ndtree = NdTree::default();
        let mut hashmap = HashMap::default();
        for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&pos.to_bigvec(), state);
        }
        let half_diag = NdVec([x_radius, y_radius]);
        let rect = IRect2D::span(center - half_diag, center + half_diag).to_bigrect();
        let slice = ndtree.slice_containing(&rect);
        let slice_rect = slice.rect();
        // Check that the slice contains the rectangle.
        assert!(slice_rect.contains(&rect));
        // Check that the slice containis the correct cells.
        for (pos, state) in hashmap {
            if slice_rect.contains(&pos.to_bigvec()) {
                if let Some(cell_state) = slice.get_cell(&pos.to_bigvec()) {
                    assert_eq!(state, cell_state);
                }
            }
        }
        // Check that the slice is as small as it can be.
        println!(
            "{:?}, rect={}; should contain {}",
            slice.root.layer(),
            slice.rect(),
            rect,
        );
        assert!(
            slice.root.layer().is_leaf::<Dim2D>()
            || slice.root.big_len() <= (rect.len(X) - 2) * 4
            || slice.root.big_len() <= (rect.len(Y) - 2) * 4
        );
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(16))]

    /// Tests `NdTree::recenter()`.
    #[test]
    fn test_ndtree_recenter(
        cells_to_set in proptest_cells_to_set(),
        old_offset in proptest_ivec2d(-100..=100),
        new_center in proptest_ivec2d(-100..=100),
    ) {
        let mut ndtree = NdTree::default();
        ndtree.set_center(old_offset.to_bigvec());

        let mut hashmap = HashMap::default();
        for (pos, cell) in cells_to_set {
            ndtree.set_cell(&pos.to_bigvec(), cell);
            hashmap.insert(pos, cell);
        }

        ndtree.recenter(&new_center.to_bigvec());

        for (pos, cell) in hashmap {
            assert_eq!(cell, ndtree.get_cell(&pos.to_bigvec()));
        }
    }

    /// Tests `NdTree::clear_region()`.
    #[test]
    fn test_ndtree_clear_region(
        cells_to_set in proptest_cells_to_set(),
        ndtree_center in proptest_ivec2d(-100..=100),
        clear_rect in proptest_irect2d(-100..=100),
    ) {
        let mut ndtree = NdTree::with_center(ndtree_center.to_bigvec());
        let mut hashmap = HashMap::default();
        for (pos, state) in cells_to_set {
            ndtree.set_cell(&pos.to_bigvec(), state);
            hashmap.insert(pos, state);
        }

        ndtree.clear_region(Region::Rect(clear_rect.to_bigrect()));

        for (pos, cell_state) in hashmap {
            let expected = if clear_rect.contains(&pos) {
                0_u8
            } else {
                cell_state
            };
            assert_eq!(expected, ndtree.get_cell(&pos.to_bigvec()));
        }
    }

    /// Tests `NdTree::clear_region()`.
    #[test]
    fn test_ndtree_get_region(
        cells_to_set in proptest_cells_to_set(),
        ndtree_center in proptest_ivec2d(-100..=100),
        get_rect in proptest_irect2d(-100..=100),
    ) {
        let mut ndtree = NdTree::with_center(ndtree_center.to_bigvec());
        let mut hashmap = HashMap::default();
        for (pos, state) in cells_to_set {
            ndtree.set_cell(&pos.to_bigvec(), state);
            hashmap.insert(pos, state);
        }

        let result = ndtree.get_region(Region::Rect(get_rect.to_bigrect()));

        for (pos, cell_state) in hashmap {
            let expected = if get_rect.contains(&pos) {
                cell_state
            } else {
                0_u8
            };
            assert_eq!(expected, result.get_cell(&pos.to_bigvec()));
        }
    }

    /// Tests `NdTree::paste_custom()`.
    #[test]
    fn test_ndtree_paste(
        cells_to_set in proptest_cells_to_set(),
        cells_to_paste in proptest_cells_to_set(),
        paste_offset in proptest_ivec2d(-100..=100),
        paste_rect in proptest_irect2d(-100..=100),
    ) {
        let mut all_cell_positions = HashSet::new();

        let mut ndtree = NdTree::default();
        let mut hashmap = HashMap::default();
        for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&pos.to_bigvec(), state);
            all_cell_positions.insert(pos);
        }

        let mut ndtree_to_paste = NdTree::with_node_pool(ndtree.pool().new_ref());
        ndtree_to_paste.set_offset(paste_offset.to_bigvec());
        let mut hashmap_pasted = HashMap::default();
        for (pos, state) in cells_to_paste {
            hashmap_pasted.insert(pos, state);
            ndtree_to_paste.set_cell(&pos.to_bigvec(), state);
            all_cell_positions.insert(pos);
        }

        // Paste using bitwise XOR.
        ndtree.paste_custom(
            ndtree_to_paste,
            Region::Rect(paste_rect.to_bigrect()),
            |original_node, pasted_node| {
                if original_node.is_empty() {
                    Some(pasted_node)
                } else if pasted_node.is_empty() {
                    Some(original_node)
                } else {
                    None
                }
            },
            |original_cell, pasted_cell| original_cell ^ pasted_cell,
        );

        for pos in &all_cell_positions {
            let original = *hashmap.get(pos).unwrap_or(&0);
            let pasted = if paste_rect.contains(pos) {
                *hashmap_pasted.get(pos).unwrap_or(&0)
            } else {
                0
            };
            let expected = original ^ pasted;
            assert_eq!(expected, ndtree.get_cell(&pos.to_bigvec()));
        }
    }
}

fn proptest_cells_to_set() -> impl Strategy<Value = Vec<(IVec2D, u8)>> {
    prop::collection::vec(
        (proptest_ivec2d(-100..100), 0..=255_u8),
        proptest::collection::SizeRange::default(),
    )
}

fn proptest_cells_to_get() -> impl Strategy<Value = Vec<IVec2D>> {
    prop::collection::vec(
        proptest_ivec2d(-100..100),
        proptest::collection::SizeRange::default(),
    )
}
