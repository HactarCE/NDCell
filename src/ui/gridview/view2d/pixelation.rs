use std::collections::HashMap;

use super::*;

type Color = [f32; 4];

struct QuadtreeTextureInProgress<C: CellType, P: Fn(&NdTreeBranch<C, Dim2D>) -> Color> {
    pixelator: P,
    nodes: Vec<[i32; 4]>,
    node_lookup: HashMap<NdTreeBranch<C, Dim2D>, i32, NodeHasher>,
}
impl<C: CellType, P: Fn(&NdTreeBranch<C, Dim2D>) -> Color> QuadtreeTextureInProgress<C, P> {
    fn new(node: &NdCachedNode<C, Dim2D>, layers: usize, pixelator: P) -> Self {
        let mut ret = Self {
            pixelator,
            nodes: vec![],
            node_lookup: HashMap::default(),
        };
        // Reserve a space in the texture for the root node.
        ret.nodes.push([0, 0, 0, 0]);
        for branch in &node.branches {
            ret.add_branch(&branch, layers - 1);
        }
        ret
    }
    fn add_branch(&mut self, branch: &NdTreeBranch<C, Dim2D>, layers: usize) -> usize {
        let node_idx = self.nodes.len();
        if layers == 0 {
            let color = (self.pixelator)(branch);
            // Keep the same bits, just treat it like an [i32; 4] instead of an
            // [f32; 4].
            self.nodes.push(unsafe { std::mem::transmute(color) });
        } else {
            if let NdTreeBranch::Node(node) = branch {
                // Reserve a space in the texture for this node.
                self.nodes.push([0; 4]);
                for branch_idx in 0..4 {
                    self.nodes[node_idx][branch_idx] =
                        self.add_branch(&node.branches[branch_idx], layers - 1) as i32;
                }
            } else {
                panic!("Quadtree is not deep enough to encode as texture");
            }
        }
        node_idx
    }
}
