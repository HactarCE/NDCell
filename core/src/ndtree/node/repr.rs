use itertools::Itertools;
use size_hint::HintSize;
use std::marker::PhantomData;

use crate::dim::Dim;

// Let's set an arbitrary limit of 64 bytes for each leaf node.
//
// This results in the following chart for the sizes of leaf nodes, given the
// number of bits per cell state:
//
// bits     1D      2D      3D      4D
// 1        512     16x16   8x8     4x4
// 2        256     16x16   4x4     4x4
// 4        128     8x8     4x4     2x2
// 8        64      8x8     4x4     2x2
//
// 5D and 6D are always 2x2. Even if this value is lowered, 2x2 (or rather, 2^N)
// is always the minimum size of a leaf node.
const MAX_BYTES_PER_LEAF: usize = 64;

/// Precomputed description of the format used for NdTree nodes.
///
/// This can be shared for a whole node cache so that individual nodes and node
/// pointers can omit this information.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NodeRepr<D: Dim> {
    /// Layer at which nodes contain cells rather than smaller nodes. In other
    /// words, the minimum node layer.
    pub base_layer: u32,
    /// Number of cell states.
    pub state_count: usize,
    /// Number of bits used to store each cell, which is a function of the
    /// number of cell states.
    pub bits_per_cell: u8,
    /// Phantom data for polymorphism.
    _marker: PhantomData<D>,
}
impl<D: Dim> NodeRepr<D> {
    /// Computes the node representation for a simulation with the given number
    /// of dimensions and states.
    pub fn new(state_count: usize) -> Self {
        let base_layer = Self::base_layer(D::NDIM, state_count);
        let bits_per_cell = Self::bits_per_cell(state_count);
        Self {
            base_layer,
            state_count,
            bits_per_cell,
            _marker: PhantomData,
        }
    }

    pub fn pack_cells(&self, cells: Box<[u8]>) -> Box<[u8]> {
        match self.bits_per_cell {
            1 => cells
                .into_iter()
                .copied()
                .chunks(8)
                .into_iter()
                .map(|chunk| {
                    (0..8)
                        .step_by(1)
                        .zip(chunk)
                        .map(|(i, cell)| cell << i)
                        .sum()
                })
                .collect_vec()
                .into_boxed_slice(),
            2 => cells
                .into_iter()
                .copied()
                .chunks(4)
                .into_iter()
                .map(|chunk| {
                    (0..8)
                        .step_by(2)
                        .zip(chunk)
                        .map(|(i, cell)| cell << i)
                        .sum()
                })
                .collect_vec()
                .into_boxed_slice(),
            4 => cells
                .into_iter()
                .copied()
                .chunks(2)
                .into_iter()
                .map(|chunk| {
                    (0..8)
                        .step_by(4)
                        .zip(chunk)
                        .map(|(i, cell)| cell << i)
                        .sum()
                })
                .collect_vec()
                .into_boxed_slice(),
            8 => cells,
            _ => panic!("Invalid cell representation"),
        }
    }
    pub fn unpack_cells<'a>(&self, layer: u32, packed_cells: &'a [u8]) -> NodeCells<'a> {
        NodeCells {
            bits_per_cell: self.bits_per_cell,
            cell_count: super::math::node_num_cells::<D>(layer),
            raw_cells: packed_cells,
        }
    }

    /// Returns the number of bits needed to represent a cell state, given the
    /// total number of cell states.
    fn bits_per_cell(state_count: usize) -> u8 {
        // Use fewer bits for automata for smaller state counts.
        match state_count {
            0..=2 => 1,
            3..=4 => 2,
            5..=16 => 4,
            _ => 8,
        }
    }
    /// Returns the layer of leaf nodes in a simulation with the given number of
    /// dimensions and states.
    fn base_layer(ndim: usize, state_count: usize) -> u32 {
        // Fit as many cells as possible without exceeding MAX_BYTES_PER_LEAF.
        let max_cells = MAX_BYTES_PER_LEAF * 8 / Self::bits_per_cell(state_count) as usize;
        (2..)
            .take_while(|&base_layer| 1 << (base_layer * ndim) < max_cells)
            .last()
            // If all the options are too big, use a 2x2 (layer = 1) base node.
            .unwrap_or(1) as u32
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NodeCells<'a> {
    bits_per_cell: u8,
    cell_count: usize,
    raw_cells: &'a [u8],
}
impl<'a> NodeCells<'a> {
    pub fn get(self, index: usize) -> u8 {
        match self.bits_per_cell {
            1 => (self.raw_cells[index >> 3] >> (index & 0b_111)) & 0b_0001,
            2 => (self.raw_cells[index >> 2] >> (index & 0b_011)) & 0b_0011,
            4 => (self.raw_cells[index >> 1] >> (index & 0b_001)) & 0b_1111,
            8 => (self.raw_cells[index]),
            _ => panic!("Invalid cell representation"),
        }
    }
    pub fn iter(self) -> impl 'a + Iterator<Item = u8> {
        self.raw_cells
            .iter()
            .flat_map(move |&byte| match self.bits_per_cell {
                1 => (0..8).map(|i| byte & (0b_0001 << (i << 0))).collect_vec(),
                2 => (0..4).map(|i| byte & (0b_0011 << (i << 1))).collect_vec(),
                4 => (0..2).map(|i| byte & (0b_1111 << (i << 2))).collect_vec(),
                8 => std::iter::once(byte).collect_vec(),
                _ => panic!("Invalid cell representation"),
            })
            .hint_size(self.cell_count)
    }
    pub fn into_boxed_slice(self) -> Box<[u8]> {
        self.iter().collect()
    }
}
