use super::*;
use crate::dim::Dim2D;
use crate::ndvec::NdVec;

#[test]
fn test_hashlife_node_step_size() {
    let _1x1 = Layer(0);
    let _2x2 = Layer(1);
    let _4x4 = Layer(2);
    let _8x8 = Layer(3);
    let _16x16 = Layer(4);
    let _32x32 = Layer(5);
    let _64x64 = Layer(6);

    for max_log2_step_size in 0..=5 {
        println!("Testing max_log2_step_size={}", max_log2_step_size);

        let p = HashLifeParams::with_log2_step_size(max_log2_step_size);
        let m = 1 << max_log2_step_size;

        for r in 0..=1 {
            assert_eq!(Some(m.min(0)), p.node_step_size(_1x1, r));
            assert_eq!(Some(m.min(0)), p.node_step_size(_2x2, r));
            assert_eq!(Some(m.min(1)), p.node_step_size(_4x4, r));
            assert_eq!(Some(m.min(2)), p.node_step_size(_8x8, r));
            assert_eq!(Some(m.min(4)), p.node_step_size(_16x16, r));
            assert_eq!(Some(m.min(8)), p.node_step_size(_32x32, r));
            assert_eq!(Some(m.min(16)), p.node_step_size(_64x64, r));
        }

        assert_eq!(Some(m.min(0)), p.node_step_size(_1x1, 2));
        assert_eq!(Some(m.min(0)), p.node_step_size(_2x2, 2));
        assert_eq!(Some(m.min(0)), p.node_step_size(_4x4, 2));
        assert_eq!(Some(m.min(1)), p.node_step_size(_8x8, 2));
        assert_eq!(Some(m.min(2)), p.node_step_size(_16x16, 2));
        assert_eq!(Some(m.min(4)), p.node_step_size(_32x32, 2));
        assert_eq!(Some(m.min(8)), p.node_step_size(_64x64, 2));

        for r in 3..=4 {
            assert_eq!(Some(m.min(0)), p.node_step_size(_1x1, r));
            assert_eq!(Some(m.min(0)), p.node_step_size(_2x2, r));
            assert_eq!(Some(m.min(0)), p.node_step_size(_4x4, r));
            assert_eq!(Some(m.min(0)), p.node_step_size(_8x8, r));
            assert_eq!(Some(m.min(1)), p.node_step_size(_16x16, r));
            assert_eq!(Some(m.min(2)), p.node_step_size(_32x32, r));
            assert_eq!(Some(m.min(4)), p.node_step_size(_64x64, r));
        }

        assert_eq!(Some(m.min(0)), p.node_step_size(_1x1, 5));
        assert_eq!(Some(m.min(0)), p.node_step_size(_2x2, 5));
        assert_eq!(Some(m.min(0)), p.node_step_size(_4x4, 5));
        assert_eq!(Some(m.min(0)), p.node_step_size(_8x8, 5));
        assert_eq!(Some(m.min(0)), p.node_step_size(_16x16, 5));
        assert_eq!(Some(m.min(1)), p.node_step_size(_32x32, 5));
        assert_eq!(Some(m.min(2)), p.node_step_size(_64x64, 5));
    }
}

#[test]
fn test_ndtree_node_set_get() {
    let _node_cache = NodeCache::<Dim2D>::new();
    let node_cache = _node_cache.read();
    let mut node = node_cache.get_empty(Layer(3));
    // print_node(node);
    let len = node.layer().len().unwrap();
    for x in 0..len {
        for y in 0..len {
            let cell = ((x + y) % 9 + 1) as u8;
            let pos = NdVec([x.into(), y.into()]);
            // let old_node = node;
            node = node.set_cell(&pos, cell);
            // println!("old");
            // print_node(old_node);
            // println!("new");
            // print_node(node);
            assert_eq!(cell, node.cell_at_pos(&pos));
        }
    }
}

// fn print_node<'n>(node: NodeRef<'n, Dim2D>) {
//     let len = node.layer().len().unwrap();
//     for row in 0..len {
//         for col in 0..len {
//             if col != 0 {
//                 print!(" ");
//             }
//             let cell = node.cell_at_pos(&NdVec([col.into(), row.into()]));
//             if cell == 0 {
//                 print!(".");
//             } else {
//                 print!("{}", cell)
//             }
//         }
//         println!();
//     }
//     println!();
// }
