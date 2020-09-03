use super::*;
use crate::dim::Dim2D;
use crate::ndvec::NdVec;

#[test]
fn test_ndtree_node_set_get() {
    let _node_cache = NodeCache::<Dim2D>::new();
    let node_cache = _node_cache.read().unwrap();
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
