//! Utilities to compute properties of a node based on its layer.

use num::{BigInt, One};

use crate::{BigRect, BigVec, Dim, URect, UVec};

pub fn node_num_cells<D: Dim>(layer: u32) -> usize {
    1 << (layer as usize * D::NDIM)
}
pub fn node_rect<D: Dim>(layer: u32) -> URect<D> {
    URect::span(UVec::origin(), UVec::repeat((1_usize << layer) - 1))
}
pub fn node_strides<D: Dim>(layer: u32) -> UVec<D> {
    UVec::from_fn(|ax| 1 << (layer as usize * ax as usize))
}

pub fn big_node_num_cells<D: Dim>(layer: u32) -> BigInt {
    BigInt::one() << (layer as usize * D::NDIM)
}
pub fn big_node_rect<D: Dim>(layer: u32) -> BigRect<D> {
    BigRect::span(
        BigVec::origin(),
        BigVec::repeat((BigInt::one() << layer) - 1),
    )
}

/// Returns the flattened cell index corresponding to the given position in
/// a leaf node, modulo the node length along each axis.
pub fn leaf_node_pos_to_cell_index<D: Dim>(layer: u32, pos: UVec<D>) -> usize {
    todo!("test this");
    assert!(layer >= 1);
    let pos = pos & ((1 << layer) - 1);
    (pos * node_strides(layer)).sum()
    // let mut stride = 1;
    // let mut ret = 0;
    // for ax in D::axes() {
    //     debug_assert!(0 <= pos);
    //     debug_assert!(pos < 1 << layer);
    //     ret += pos[ax] * stride;
    //     stride <<= layer - 1;
    // }
    // ret
}
/// Returns the vector position corresponding to the given cell index in a
/// leaf node.
///
/// Panics if the index is out of range.
pub fn leaf_node_cell_index_to_pos<D: Dim>(layer: u32, index: usize) -> UVec<D> {
    todo!("test this");
    assert!(layer >= 1);
    debug_assert!(index < node_num_cells::<D>(layer));
    UVec::from_fn(|ax| {
        index
            & ((1 << (layer as usize * ax as usize)) - (1 << (layer as usize * (ax as usize - 1))))
    })
}

/// Returns the index of the child of a node at the given layer that contains
/// the given position, modulo the node length along each axis.
pub fn non_leaf_pos_to_child_index<D: Dim>(layer: u32, pos: &BigVec<D>) -> usize {
    // Extract the single bit from the position that is relevant for this layer.
    let child_layer = layer - 1;
    let child_index_vector = ((pos >> child_layer) & &BigInt::one()).to_uvec();
    // This function does the exact computation we need.
    leaf_node_pos_to_cell_index(1, child_index_vector)
}
