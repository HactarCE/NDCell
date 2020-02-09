//! Utilities for constructing and caching an OpenGL texture that encodes a
//! quadtree.

use crate::ui::DISPLAY;
use glium::texture::unsigned_texture2d::UnsignedTexture2d;
use glium::texture::{ClientFormat, RawImage2d};
use std::borrow::Cow;
use std::sync::Once;

use super::*;

// Nearly all GPUs have a lower GL_MAX_TEXTURE_SIZE than 1024:
// https://opengl.gpuinfo.org/displaycapability.php?name=GL_MAX_TEXTURE_SIZE
const WARN_TEXTURE_SIZE_THRESHOLD: usize = 1024;

static WARN_TEXTURE_SIZE: Once = Once::new();

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
    pub texture: UnsignedTexture2d,
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
        let mut pixel_vec: Vec<u32> = indexed_tree
            .get_nodes()
            .iter()
            .flatten()
            .map(|indexed_branch| match indexed_branch {
                IndexedNdTreeBranch::Leaf(branch) => encode_u8_4(pixelator(&branch)),
                IndexedNdTreeBranch::Pointer(idx) => *idx as u32,
            })
            .collect();
        // A simple 1D texture may exceed GL_MAX_TEXTURE_SIZE (which,
        // unfortunately, we can't get because Glium does not expose it), so
        // instead make a square texture that is as small as possible. Even this
        // might exceed the maximum texture size, but that's less likely.
        let width = ((pixel_vec.len() / 4) as f64).sqrt().ceil() as usize;
        if width > WARN_TEXTURE_SIZE_THRESHOLD {
            WARN_TEXTURE_SIZE.call_once(|| warn!(
                "Texture encoding quadtree has exceeded {}x{}; this may cause rendering problems in older GPUs",
                WARN_TEXTURE_SIZE_THRESHOLD,
                WARN_TEXTURE_SIZE_THRESHOLD
            ));
        }
        assert!(width * width * 4 >= pixel_vec.len());
        pixel_vec.resize(width * width * 4, 0);
        let raw_image: RawImage2d<u32> = RawImage2d {
            data: Cow::Owned(pixel_vec),
            width: width as u32,
            height: width as u32,
            format: ClientFormat::U32U32U32U32,
        };
        let texture =
            UnsignedTexture2d::new(&**DISPLAY, raw_image).expect("Failed to create texture");
        Self {
            texture,
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
