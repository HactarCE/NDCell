//! Quadtrees and octrees encoded in OpenGL textures.

use anyhow::{Context, Result};
use glium::texture::unsigned_texture2d::UnsignedTexture2d;
use glium::texture::{ClientFormat, RawImage2d};
use itertools::Itertools;
use log::warn;
use palette::{Mix, Pixel, Srgba};
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Once;

use ndcell_core::prelude::*;

use crate::{CONFIG, DISPLAY};

/// Texture size threshold beyond which to write a warning to the log.
///
/// All GPUs on gpuinfo.org that have a `GL_MAX_TEXTURE_SIZE` less than 4096 use
/// older versions of OpenGL/GLSL that NDCell doesn't support anyway:
/// https://opengl.gpuinfo.org/displaycapability.php?name=GL_MAX_TEXTURE_SIZE
const WARN_TEXTURE_SIZE_THRESHOLD: usize = 4096;

// Only warn once.
static WARN_TEXTURE_SIZE: Once = Once::new();

// TODO: does all this caching stuff actually help perf?

pub type GlQuadtreeCache = GlNdTreeCache<Dim2D>;
pub type GlOctreeCache = GlNdTreeCache<Dim3D>;

/// Several cached ND-trees encoded in OpenGL textures, which are dropped if
/// unused for one frame.
#[derive(Default)]
pub struct GlNdTreeCache<D: Dim> {
    used: HashMap<(ArcNode<D>, Layer), GlNdTree>,
    unused: HashMap<(ArcNode<D>, Layer), GlNdTree>,
}
impl<D: Dim> GlNdTreeCache<D> {
    #[optick_attr::profile]
    pub fn gl_ndtree_from_node(&mut self, node: ArcNode<D>, min_layer: Layer) -> Result<&GlNdTree> {
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
            GlNdTree::from_node(&node_ref, min_layer)?
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

/// ND-tree encoded in an OpenGL texture.
pub struct GlNdTree {
    pub texture: UnsignedTexture2d,
    pub layers: i32,
    pub root_idx: u32,
}
impl GlNdTree {
    /// Constructs a `GlNdTree` from a node and a function to turn a node into a
    /// solid color.
    pub fn from_node<'n, N: NodeRefTrait<'n>>(node: N, min_layer: Layer) -> Result<Self> {
        // Use the parent layer because we want to store four pixels (each
        // representing a node at `min_layer`) inside one index.
        let flat_ndtree = FlatNdTree::from_node(node, min_layer.parent_layer(), |node| node);
        let layer_count = 1 + flat_ndtree.layers();
        assert_eq!(layer_count, (node.layer() - min_layer).to_usize());
        let mut pixel_data: Vec<u32> = flat_ndtree
            .nodes()
            .iter()
            .flat_map(|indexed_node| match indexed_node {
                FlatNdTreeNode::Leaf(node, _) => node
                    .subdivide()
                    .unwrap()
                    .into_iter()
                    .map(ndtree_node_color) // `NodeRef` to `Srgba`
                    .map(|color| color.into_format::<u8, u8>().into_raw()) // `Srgba` to `[u8; 4]`
                    .map(u32::from_be_bytes) // `[u8; 4]` to `u32` (shader expects big-endian)
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
        const UINTS_PER_PIXEL: usize = 4;
        let pixel_count = pixel_data.len().div_ceil(&UINTS_PER_PIXEL);
        let pixel_width = (pixel_count as f64).sqrt().ceil() as usize;
        if pixel_width > WARN_TEXTURE_SIZE_THRESHOLD {
            WARN_TEXTURE_SIZE.call_once(|| {
                warn!(
                    "Texture encoding ND-tree has exceeded {}x{}; this may crash older graphics cards",
                    WARN_TEXTURE_SIZE_THRESHOLD,
                    WARN_TEXTURE_SIZE_THRESHOLD,
                )
            });
        }
        assert!(pixel_width * pixel_width * UINTS_PER_PIXEL >= pixel_data.len());
        pixel_data.resize(pixel_width * pixel_width * UINTS_PER_PIXEL, 0);
        let raw_image: RawImage2d<'_, u32> = RawImage2d {
            data: Cow::Owned(pixel_data),
            width: pixel_width as u32,
            height: pixel_width as u32,
            format: ClientFormat::U32U32U32U32, // UNITS_PER_PIXEL = 4
        };
        let texture = UnsignedTexture2d::new(&**DISPLAY, raw_image)
            .with_context(|| format!("ND-tree texture too big: {}x{}", pixel_width, pixel_width))?;

        Ok(Self {
            texture,
            layers: layer_count as i32,
            root_idx: flat_ndtree.root_idx() as u32,
        })
    }
}

/// Returns the color to represent an ND-tree node.
pub fn ndtree_node_color<D: Dim>(node: NodeRef<'_, D>) -> Srgba {
    if let Some(cell_state) = node.single_state() {
        CONFIG.lock().gfx.cell_colors[cell_state as usize] // TODO: is locking the mutex a perf issue?
    } else {
        // Multiply then divide by 255 to keep some precision.
        let population_ratio = (node.population() * 255_usize / node.big_num_cells())
            .to_f32()
            .unwrap()
            / 255.0;
        // Bias so that 50% is the minimum brightness if there are any
        // live cells.
        let mix_factor = (population_ratio / 2.0) + 0.5;

        // Mix colors for state #0 and #1 using proportion of live cells.
        Srgba::from_linear(Mix::mix(
            &crate::colors::cells::DEAD.into_linear(),
            &crate::colors::cells::LIVE.into_linear(),
            mix_factor,
        ))
    }
}
