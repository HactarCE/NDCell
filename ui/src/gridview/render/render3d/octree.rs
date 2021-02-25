/// Octree rendering parameters.
///
/// Fields are arranged to eliminate padding.
#[derive(Debug, Default, Copy, Clone)]
pub(super) struct OctreeParams {
    /// Render cell transformation matrix.
    pub matrix: [[f32; 4]; 4],
    /// Inverse render cell transformation matrix.
    pub inv_matrix: [[f32; 4]; 4],

    /// Integer position of octree base.
    pub octree_base: [i32; 3],
    /// Number of octree layers.
    pub layer_count: i32,
    /// Index of root node.
    pub root_idx: u32,
}

implement_uniform_block!(
    OctreeParams,
    matrix,
    inv_matrix,
    octree_base,
    layer_count,
    root_idx,
);
