//! Utilities for constructing and caching an OpenGL texture that encodes a
//! quadtree.

use glium::texture::unsigned_texture2d::UnsignedTexture2d;
use glium::texture::{ClientFormat, RawImage2d};
use std::borrow::Cow;
use std::sync::Once;

use ndcell_core::ndtree::indexed::*;

use super::*;
use crate::DISPLAY;

// Nearly all GPUs have a lower GL_MAX_TEXTURE_SIZE than 1024:
// https://opengl.gpuinfo.org/displaycapability.php?name=GL_MAX_TEXTURE_SIZE
const WARN_TEXTURE_SIZE_THRESHOLD: usize = 1024;

static WARN_TEXTURE_SIZE: Once = Once::new();

#[derive(Default)]
pub struct CachedGlQuadtree {
    cached: Option<GlQuadtree>,
    current_node: Option<ArcNode<Dim2D>>,
    current_min_layer: Layer,
}
impl CachedGlQuadtree {
    pub fn set_node(
        &mut self,
        node: ArcNode<Dim2D>,
        min_layer: Layer,
        pixelator: impl FnMut(NodeRef<'_, Dim2D>) -> [u8; 4],
    ) {
        // Only recompute quadtree if necessary.
        if self.current_min_layer == min_layer && Some(&node) == self.current_node.as_ref() {
            return;
        }
        self.cached = Some(GlQuadtree::from_node(
            node.as_ref(&node.cache().read()),
            min_layer,
            pixelator,
        ));
        self.current_node = Some(node);
        self.current_min_layer = min_layer;
    }
    pub fn from_node(
        &mut self,
        node: ArcNode<Dim2D>,
        min_layer: Layer,
        pixelator: impl FnMut(NodeRef<'_, Dim2D>) -> [u8; 4],
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
    pub fn from_node<'n>(
        node: NodeRef<'n, Dim2D>,
        min_layer: Layer,
        mut pixelator: impl FnMut(NodeRef<'n, Dim2D>) -> [u8; 4],
    ) -> Self {
        // Use the parent layer because we want to store four pixels (each
        // representing a node at `min_layer`) inside one index.
        let indexed_tree = IndexedNdTree::from_node(node, min_layer.parent_layer(), |node| node);
        let mut pixel_vec: Vec<u32> = indexed_tree
            .nodes()
            .iter()
            .flat_map(|indexed_node| match indexed_node {
                IndexedNdTreeNode::Leaf(node, _) => node
                    .subdivide()
                    .unwrap()
                    .into_iter()
                    .map(&mut pixelator)
                    .map(u32::from_le_bytes) // shader expects little-endian
                    .collect_vec(),
                IndexedNdTreeNode::NonLeaf(indices, _) => {
                    indices.into_iter().map(|&i| i as u32).collect_vec()
                }
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
                WARN_TEXTURE_SIZE_THRESHOLD,
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
            layers: indexed_tree.layers(),
            root_idx: indexed_tree.root_idx(),
        }
    }
}
