//! Quadtrees encoded in OpenGL textures.

use anyhow::{Context, Result};
use glium::texture::unsigned_texture2d::UnsignedTexture2d;
use glium::texture::{ClientFormat, RawImage2d};
use itertools::Itertools;
use log::warn;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Once;

use ndcell_core::prelude::*;

use crate::DISPLAY;

/// Texture size threshold beyond which to write a warning to the log.
///
/// All GPUs on gpuinfo.org that have a `GL_MAX_TEXTURE_SIZE` less than 4096 use
/// older versions of OpenGL/GLSL that NDCell doesn't support anyway:
/// https://opengl.gpuinfo.org/displaycapability.php?name=GL_MAX_TEXTURE_SIZE
const WARN_TEXTURE_SIZE_THRESHOLD: usize = 4096;

// Only warn once.
static WARN_TEXTURE_SIZE: Once = Once::new();

// TODO: does all this caching stuff actually help perf?

/// Several cached quadtrees encoded in OpenGL textures, which dropped if unused
/// for one frame.
#[derive(Default)]
pub struct GlQuadtreeCache {
    used: HashMap<(ArcNode<Dim2D>, Layer), GlQuadtree>,
    unused: HashMap<(ArcNode<Dim2D>, Layer), GlQuadtree>,
}
impl GlQuadtreeCache {
    pub fn gl_quadtree_from_node(
        &mut self,
        node: ArcNode<Dim2D>,
        min_layer: Layer,
        pixelator: impl FnMut(NodeRef<'_, Dim2D>) -> [u8; 4],
    ) -> Result<&GlQuadtree> {
        let key = (node, min_layer);

        // There's some unnecessary mutation of the `HashMap` here, but this
        // code only runs a handful of times per frame so it doesn't matter.

        let ret = if let Some(ret) = self.unused.remove(&key) {
            // We do not need to regenerate the texture.
            ret
        } else if let Some(ret) = self.used.remove(&key) {
            // We do not need to regenerate the texture.
            ret
        } else {
            // We DO need to regenerate the texture.
            let node_ref = key.0.as_ref_with_guard();
            GlQuadtree::from_node(&node_ref, min_layer, pixelator)?
        };
        Ok(self.used.entry(key).or_insert(ret))
    }

    /// Invalidates the cache.
    pub fn invalidate_all(&mut self) {
        self.used.clear();
        self.unused.clear();
    }

    pub fn post_frame_clean_cache(&mut self) {
        self.unused.clear();
        std::mem::swap(&mut self.used, &mut self.unused);
    }
}

/// Quadtree encoded in an OpenGL texture.
pub struct GlQuadtree {
    pub texture: UnsignedTexture2d,
    pub layers: usize,
    pub root_idx: usize,
}
impl GlQuadtree {
    /// Constructs a GlQuadtree from a node and a function to turn a node into a
    /// solid color.
    pub fn from_node<'n>(
        node: impl NodeRefTrait<'n, D = Dim2D>,
        min_layer: Layer,
        mut pixelator: impl FnMut(NodeRef<'n, Dim2D>) -> [u8; 4],
    ) -> Result<Self> {
        // Use the parent layer because we want to store four pixels (each
        // representing a node at `min_layer`) inside one index.
        let indexed_tree = FlatNdTree2D::from_node(node, min_layer.parent_layer(), |node| node);
        let mut pixel_vec: Vec<u32> = indexed_tree
            .nodes()
            .iter()
            .flat_map(|indexed_node| match indexed_node {
                FlatNdTreeNode::Leaf(node, _) => node
                    .subdivide()
                    .unwrap()
                    .into_iter()
                    .map(&mut pixelator)
                    .map(u32::from_le_bytes) // shader expects little-endian
                    .collect_vec(),
                FlatNdTreeNode::NonLeaf(indices, _) => {
                    indices.into_iter().map(|&i| i as u32).collect_vec()
                }
            })
            .collect();
        // A simple 1D texture may exceed `GL_MAX_TEXTURE_SIZE` (which,
        // unfortunately, we can't check because Glium does not expose it), so
        // instead make a square texture that is as small as possible. Even this
        // might exceed the maximum texture size, but that's less likely. If it
        // does happen, we'll propogate the error up.
        let width = ((pixel_vec.len() / 4) as f64).sqrt().ceil() as usize;
        if width > WARN_TEXTURE_SIZE_THRESHOLD {
            WARN_TEXTURE_SIZE.call_once(|| {
                warn!(
                    "Texture encoding quadtree has exceeded {}x{}; this may crash older graphics cards",
                    WARN_TEXTURE_SIZE_THRESHOLD,
                    WARN_TEXTURE_SIZE_THRESHOLD,
                )
            });
        }
        assert!(width * width * 4 >= pixel_vec.len());
        pixel_vec.resize(width * width * 4, 0);
        let raw_image: RawImage2d<'_, u32> = RawImage2d {
            data: Cow::Owned(pixel_vec),
            width: width as u32,
            height: width as u32,
            format: ClientFormat::U32U32U32U32,
        };
        let texture = UnsignedTexture2d::new(&**DISPLAY, raw_image)
            .with_context(|| format!("Quadtree texture too big: {}x{}", width, width))?;
        Ok(Self {
            texture,
            layers: indexed_tree.layers(),
            root_idx: indexed_tree.root_idx(),
        })
    }
}
