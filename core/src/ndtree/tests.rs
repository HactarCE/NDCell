use proptest::prelude::*;

use super::*;
use crate::axis::{X, Y};
use crate::ndrect::IRect;
use crate::ndvec::{proptest_ivec2d, IVec2D, NdVec};
use crate::num::BigUint;
use crate::HashMap;

fn assert_ndtree_valid(
    expected_cells: &HashMap<IVec2D, u8>,
    ndtree: &NdTree2D,
    node_cache: &NodeCache<Dim2D>,
    cells_to_check: &Vec<IVec2D>,
) {
    assert_eq!(
        BigUint::from(
            expected_cells
                .iter()
                .filter(|(_, &cell_state)| cell_state != 0)
                .count()
        ),
        ndtree.root().as_ref(node_cache).population()
    );
    for pos in cells_to_check {
        assert_eq!(
            *expected_cells.get(pos).unwrap_or(&0),
            ndtree.get_cell(node_cache, &pos.to_bigvec())
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
        let _node_cache = Arc::clone(ndtree.cache());
        let node_cache = _node_cache.read();
        let mut hashmap = HashMap::default();
        for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&node_cache, &pos.to_bigvec(), state);
            cells_to_get.push(pos);
        }
        assert_ndtree_valid(&hashmap, &ndtree, &node_cache, &cells_to_get);
        // Test that expansion preserves population and positions.
        let old_layer = ndtree.layer();
        while ndtree.layer() < Layer(5) {
            ndtree.expand(&node_cache);
            assert_ndtree_valid(&hashmap, &ndtree, &node_cache, &cells_to_get);
        }
        // Test that shrinking actually shrinks.
        ndtree.shrink(&node_cache);
        assert!(ndtree.layer() <= old_layer);
        // Test that shrinking preserves population and positions.
        assert_ndtree_valid(&hashmap, &ndtree, &node_cache, &cells_to_get);
    }

    /// Tests that identical nodes use the same underlying structure.
    #[test]
    fn test_ndtree_cache(
        cells_to_set in proptest_cells_to_set(),
    ) {
        prop_assume!(!cells_to_set.is_empty());

        let mut ndtree = NdTree::default();
        let _node_cache = Arc::clone(ndtree.cache());
        let node_cache = _node_cache.read();
        for (pos, state) in cells_to_set {
            ndtree.set_cell(&node_cache, &(pos - 128).to_bigvec(), state);
            ndtree.set_cell(&node_cache, &(pos + 128).to_bigvec(), state);
        }
        let slice = ndtree.slice(&node_cache);
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
        let _node_cache = Arc::clone(ndtree.cache());
        let node_cache = _node_cache.read();
        let mut hashmap = HashMap::default();
            for (pos, state) in cells_to_set {
            hashmap.insert(pos, state);
            ndtree.set_cell(&node_cache, &pos.to_bigvec(), state);
        }
        let half_diag = NdVec([x_radius, y_radius]);
        let rect = IRect::span(center - half_diag, center + half_diag).to_bigrect();
        let slice = ndtree.slice_containing(&node_cache, &rect);
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
