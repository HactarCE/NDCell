//! Utilities for constructing and caching an OpenGL texture that encodes a
//! quadtree.

use crate::ui::DISPLAY;
use glium::texture::unsigned_texture1d::UnsignedTexture1d;
use glium::texture::{ClientFormat, RawImage1d};
use std::borrow::Cow;

use super::*;

#[derive(Default)]
pub struct CachedGlQuadtree<C: CellType> {
    cached: Option<GlQuadtree>,
    current_node: Option<NdCachedNode<C, Dim2D>>,
    current_min_layer: usize,
}
impl<C: CellType> CachedGlQuadtree<C> {
    pub fn set_node<F: FnMut(&NdTreeBranch<C, Dim2D>) -> [u8; 4]>(
        &mut self,
        node: NdCachedNode<C, Dim2D>,
        min_layer: usize,
        pixelator: F,
    ) {
        // Only recompute quadtree if necessary.
        if self.current_min_layer == min_layer && Some(&node) == self.current_node.as_ref() {
            return;
        }
        self.cached = Some(GlQuadtree::from_node(&node, min_layer, pixelator));
        self.current_node = Some(node);
        self.current_min_layer = min_layer;
    }
    pub fn from_node<F: FnMut(&NdTreeBranch<C, Dim2D>) -> [u8; 4]>(
        &mut self,
        node: NdCachedNode<C, Dim2D>,
        min_layer: usize,
        pixelator: F,
    ) -> &GlQuadtree {
        self.set_node(node, min_layer, pixelator);
        self.unwrap()
    }
    pub fn reset(&mut self) {
        *self = Self::default();
    }
    pub fn unwrap(&self) -> &GlQuadtree {
        self.cached.as_ref().unwrap()
    }
}

/// A container for an OpenGL texture that encodes a square pixel pattern using
/// a quadtree.
pub struct GlQuadtree {
    pub texture: UnsignedTexture1d,
    pub layers: usize,
    pub root_idx: usize,
}

impl GlQuadtree {
    /// Constructs a GlQuadtree from a node and a function to turn a node into a
    /// solid color.
    pub fn from_node<C: CellType, F: FnMut(&NdTreeBranch<C, Dim2D>) -> [u8; 4]>(
        node: &NdCachedNode<C, Dim2D>,
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
            texture: UnsignedTexture1d::new(&**DISPLAY, raw_image)
                .expect("Failed to create texture"),
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
