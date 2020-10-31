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
