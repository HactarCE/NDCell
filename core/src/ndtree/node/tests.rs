use proptest::prelude::*;
use std::convert::TryFrom;
use std::hash::{BuildHasher, Hash, Hasher};

use super::*;
use crate::dim::{Dim2D, Dim3D};
use crate::ndrect::{BigRect3D, CanContain, NdRect};
use crate::ndvec::{proptest_bigvec, BigVec3D, IVec3D, NdVec};

const TEST_LAYER: Layer = Layer(5); // 32x32

fn cell_state_from_hash(anything: impl Hash) -> u8 {
    let mut h = crate::FastHashBuilder::default().build_hasher();
    anything.hash(&mut h);
    (h.finish() % 256) as u8
}

fn print_node(node: NodeRef<'_, Dim2D>) {
    let len = node.layer().len().unwrap();
    for row in 0..len {
        for col in 0..len {
            if col != 0 {
                print!(" ");
            }
            let cell = node.cell_at_pos(&NdVec([col.into(), row.into()]));
            if cell == 0 {
                print!(" .");
            } else {
                print!("{:2x}", cell)
            }
        }
        println!();
    }
    println!();
}

#[test]
fn test_ndtree_node_set_get() {
    let _node_cache = NodeCache::<Dim2D>::new();
    let node_cache = _node_cache.read();
    let node = node_cache.get_from_fn(TEST_LAYER, cell_state_from_hash);
    print_node(node);

    for pos in &node.big_rect() {
        let mut cell = cell_state_from_hash(pos.to_uvec());
        assert_eq!(cell, node.cell_at_pos(&pos));
        cell ^= 0x80_u8;
        assert_eq!(cell, node.set_cell(&pos, cell).cell_at_pos(&pos));
    }
}

#[test]
fn test_ndtree_node_recursive_modify() {
    let _node_cache = NodeCache::<Dim2D>::new();
    let node_cache = _node_cache.read();
    let mut node = node_cache.get_from_fn(TEST_LAYER, cell_state_from_hash);

    let rect = NdRect::span(NdVec::big([-10, 4]), NdVec::big([7, 30]));
    // Remove cells outside of the rectangle.
    node = node.recursive_modify(
        |offset, node| {
            let self_rect = node.big_rect() + offset;
            if !rect.intersects(&self_rect) {
                Some(node.cache().get_empty(node.layer()))
            } else if rect.contains(&self_rect) {
                Some(node)
            } else {
                None
            }
        },
        |pos, cell| {
            if rect.contains(pos) {
                cell
            } else {
                0_u8
            }
        },
    );

    for pos in &node.big_rect() {
        if rect.contains(&pos) {
            if rect.contains(&pos) {
                let cell = cell_state_from_hash(pos.to_uvec());
                assert_eq!(cell, node.cell_at_pos(&pos));
            } else {
                assert_eq!(0_u8, node.cell_at_pos(&pos));
            }
        }
    }
}

proptest! {
    #[test]
    fn test_ndtree_node_rect_is_empty_with_no_cells(
        rect in rect_near_node(TEST_LAYER),
    ) {
        // Empty node should always return false.
        let _node_cache = NodeCache::new();
        let node_cache = _node_cache.read();
        let node = node_cache.get_empty(TEST_LAYER);
        assert!(node.is_empty());
        assert!(node.rect_is_empty(&rect));
    }

    #[test]
    fn test_ndtree_node_rect_is_empty_with_one_cell(
        rect in rect_near_node(TEST_LAYER),
        pos in pos_in_node(TEST_LAYER),
        cell_state in 1..=255_u8,
    ) {
        let _node_cache = NodeCache::new();
        let node_cache = _node_cache.read();
        let node = node_cache.get_empty(TEST_LAYER).set_cell(&pos, cell_state);
        assert!(!node.is_empty());
        if rect.contains(&pos) {
            assert!(!node.rect_is_empty(&rect));
        } else {
            assert!(node.rect_is_empty(&rect));
        }
    }

    #[test]
    fn test_ndtree_node_rect_is_empty_with_many_cells(
        rect in rect_near_node(TEST_LAYER),
        cells_to_set in cells_to_set(TEST_LAYER, 1_u8),
    ) {
        let _node_cache = NodeCache::<Dim3D>::new();
        let node_cache = _node_cache.read();
        let mut node = node_cache.get_empty(TEST_LAYER);
        let node_contains_any = !cells_to_set.is_empty();
        let rect_contains_any = cells_to_set.iter().any(|(pos, _)| rect.contains(pos));
        for (pos, cell_state) in cells_to_set {
            node = node.set_cell(&pos, cell_state);
        }


        assert_eq!(node_contains_any, !node.is_empty());
        assert_eq!(rect_contains_any, !node.rect_is_empty(&rect));
    }
}

#[test]
fn test_ndtree_node_shrink_nonzero_rect_with_empty() {
    let _node_cache = NodeCache::<Dim3D>::new();
    let node_cache = _node_cache.read();
    for i in 0..10 {
        let node = node_cache.get_empty(Layer(i));
        // Node with no cells should have no minimum nonzero rectangle.
        assert_eq!(None, node.min_nonzero_rect());
    }
}

#[test]
fn test_ndtree_node_shrink_nonzero_rect_with_single_cell() {
    let pos: IVec3D = NdVec([3, 5, 14]);

    let _node_cache = NodeCache::<Dim3D>::new();
    let node_cache = _node_cache.read();
    let node = node_cache
        .get_empty(TEST_LAYER)
        .set_cell(&pos.to_bigvec(), 6);
    assert_eq!(
        Some(NdRect::span(pos, pos).to_bigrect()),
        node.min_nonzero_rect(),
    );
}

proptest! {
    #[test]
    fn test_ndtree_node_shrink_nonzero_rect_with_many_cells(
        cells_to_set in cells_to_set(TEST_LAYER, 1_u8),
        starting_rect in rect_near_node(TEST_LAYER),
    ) {
        let _node_cache = NodeCache::<Dim3D>::new();
        let node_cache = _node_cache.read();
        let mut node = node_cache.get_empty(TEST_LAYER);
        let mut expected = None; // Option<(min, max)>
        for (pos, cell_state) in cells_to_set {
            node = node.set_cell(&pos, cell_state);
            if starting_rect.contains(&pos) {
                expected = Some(match &expected {
                    None => (pos.clone(), pos),
                    Some((min, max)) => (NdVec::min(min, &pos), NdVec::max(max, &pos)),
                });
            }
        }
        let expected = expected.map(|(min, max)| NdRect::span(min, max));
        assert_eq!(expected, node.shrink_nonzero_rect(&starting_rect));
    }

    #[test]
    fn test_ndtree_node_copy_from_other_cache(
        cells_to_set in cells_to_set(TEST_LAYER, 1_u8),
    ) {
        let _node_cache = NodeCache::<Dim3D>::new();
        let node_cache = _node_cache.read();
        let mut node = node_cache.get_empty(TEST_LAYER);
        for (pos, cell_state) in &cells_to_set {
            node = node.set_cell(pos, *cell_state);
        }

        let _new_node_cache = NodeCache::<Dim3D>::new();
        let new_node_cache = _new_node_cache.read();
        let new_node = new_node_cache.copy_from_other_cache(node);

        assert_eq!(new_node.to_string(), node.to_string());
        for (pos, _) in cells_to_set {
            assert_eq!(node.cell_at_pos(&pos), new_node.cell_at_pos(&pos));
        }
    }
}

fn pos_in_node(layer: Layer) -> impl Strategy<Value = BigVec3D> {
    let len = layer
        .len()
        .and_then(|x| i32::try_from(x).ok())
        .expect("Cannot generate arbitrary values beyond i32");
    proptest_bigvec(0..len)
}

fn rect_near_node(layer: Layer) -> impl Strategy<Value = BigRect3D> {
    let len = layer
        .len()
        .and_then(|x| i32::try_from(x).ok())
        .expect("Cannot generate arbitrary values beyond i32");
    let min = -len / 2;
    let max = len + len / 2;
    (proptest_bigvec(min..max), proptest_bigvec(min..max)).prop_map(|(a, b)| NdRect::span(a, b))
}

fn cells_to_set(layer: Layer, min_cell: u8) -> impl Strategy<Value = Vec<(BigVec3D, u8)>> {
    prop::collection::vec((pos_in_node(layer), min_cell..=255_u8), 1..=20)
}
