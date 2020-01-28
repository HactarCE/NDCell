//! Functions for creating an OpenGL texture that encodes a quadtree.

use glium::texture::{ClientFormat, RawImage1d};
use std::borrow::Cow;

use super::*;

/// A container for an OpenGL texture that encodes a square pixel pattern using
/// a quadtree.
pub struct GlQuadtree<'a> {
    pub raw_image: RawImage1d<'a, u32>,
    pub layers: usize,
    pub root_idx: usize,
}

impl<'a> GlQuadtree<'a> {
    /// Constructs a GlQuadtree from a node and a function to turn a node into a
    /// solid color.
    pub fn from_node<C: CellType, D: Dim, F: FnMut(&NdTreeBranch<C, D>) -> [u8; 4]>(
        node: &NdCachedNode<C, D>,
        min_layer: usize,
        mut pixelator: F,
    ) -> Self {
        let indexed_tree = IndexedNdTree::from_node(node, min_layer);
        let pixel_vec: Vec<u32> = indexed_tree
            .get_nodes()
            .iter()
            .flatten()
            .map(|indexed_branch| match indexed_branch {
                IndexedNdTreeBranch::Leaf(branch) => encode_u8_4(pixelator(&branch)),
                IndexedNdTreeBranch::Pointer(idx) => *idx as u32,
            })
            .collect();
        let raw_image: RawImage1d<u32> = RawImage1d {
            data: Cow::Owned(pixel_vec),
            width: indexed_tree.get_nodes().len() as u32,
            format: ClientFormat::U32U32U32U32,
        };
        Self {
            raw_image,
            layers: indexed_tree.get_layer_count(),
            root_idx: indexed_tree.get_root_idx(),
        }
    }
}

/// Encodes a [u8; 4] as a u32 with the MSB holding the first byte and LSB
/// holding the last byte.
fn encode_u8_4(arr: [u8; 4]) -> u32 {
    let b0 = arr[0] as u32;
    let b1 = arr[1] as u32;
    let b2 = arr[2] as u32;
    let b3 = arr[3] as u32;
    return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
}
