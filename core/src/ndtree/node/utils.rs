use itertools::Itertools;
use size_hint::HintSize;

use crate::{Dim, URect, UVec};

// TODO: remove this module -- maybe merge with math?

pub fn pack_cells(cells: Box<[u8]>, bits_per_cell: u8) -> Box<[u8]> {
    match bits_per_cell {
        1 => cells
            .into_iter()
            .batching(|iter| {
                Some(
                    (0..8)
                        .step_by(1)
                        .map(|i| iter.next().copied().unwrap_or(0) << i)
                        .sum(),
                )
            })
            .hint_size(cells.len() * 8)
            .collect_vec()
            .into_boxed_slice(),
        2 => cells
            .into_iter()
            .batching(|iter| {
                Some(
                    (0..8)
                        .step_by(2)
                        .map(|i| iter.next().copied().unwrap_or(0) << i)
                        .sum(),
                )
            })
            .hint_size(cells.len() * 4)
            .collect_vec()
            .into_boxed_slice(),
        4 => cells
            .into_iter()
            .batching(|iter| {
                Some(
                    (0..8)
                        .step_by(4)
                        .map(|i| iter.next().copied().unwrap_or(0) << i)
                        .sum(),
                )
            })
            .hint_size(cells.len() * 2)
            .collect_vec()
            .into_boxed_slice(),
        8 => cells,
        _ => panic!("Invalid cell representation"),
    }
}

/// Subdivide a power-of-2 hypercube of cells into 2^NDIM hypercubes of half the
/// size. If the hypercube contains only one cell, that cell is returned wrapped
/// in an `Err`.
///
/// Panics if the cells array is not of length (2^len)^NDIM.
pub fn subdivide_cell_square<'a, D: Dim>(
    cells: &'a [u8],
) -> Result<impl 'a + Iterator<Item = Box<[u8]>>, u8> {
    todo!("Test this");

    // Return early if there is only one cell.
    if let [cell] = cells {
        return Err(*cell);
    }

    debug_assert_eq!(cells.len(), cells.len().next_power_of_two());
    let old_len_log_2 = cells.len().trailing_zeros() as usize / D::NDIM as usize;
    debug_assert_eq!(cells.len(), 1 << (old_len_log_2 * D::NDIM as usize));
    debug_assert!(old_len_log_2 > 0);
    let new_len_log_2 = old_len_log_2 - 1;

    let old_strides = strides_for_square::<D>(old_len_log_2);
    let old_rect = square_urect(new_len_log_2);
    Ok(square_urect(1).iter().map(move |outer| {
        let subsquare_base = outer * (1 << new_len_log_2);
        let subsquare = (old_rect.clone() + subsquare_base)
            .iter()
            .map(|pos| cells[(pos * old_strides.clone()).sum()])
            .collect_vec();
        debug_assert_eq!(subsquare.len(), 1 << (new_len_log_2 * D::NDIM));
        subsquare.into_boxed_slice()
    }))
}

/// Creates a power-of-2 hypercube of cells from 2^NDIM hypercubes of half the
/// size.
pub fn join_cell_squares<D: Dim>(squares: &[Box<[u8]>]) -> Box<[u8]> {
    todo!("Test this");

    debug_assert_eq!(squares[0].len(), squares[0].len().next_power_of_two());
    let old_len_log_2 = squares[0].len().trailing_zeros() as usize / D::NDIM as usize;
    let old_len = 1 << old_len_log_2;
    debug_assert_eq!(squares[0].len(), 1 << (old_len_log_2 * D::NDIM as usize));
    let new_len_log_2 = old_len_log_2 + 1;

    let mut ret = vec![0_u8; squares.len() * squares[0].len()];
    let new_strides = strides_for_square::<D>(new_len_log_2);
    for (square, offset) in squares.iter().zip(square_urect(1).iter()) {
        for (&cell, new_pos) in square
            .iter()
            .zip((square_urect(old_len_log_2) + offset).iter())
        {
            ret[(new_pos * new_strides).sum()] = cell;
        }
    }
    ret.into_boxed_slice()
}

/// Center a power-of-2 hypercube of cells in a larger power-of-2 hypercube of
/// cells, filling the blank spaces with state 0.
pub fn centered_cell_square<D: Dim>(cells: &[u8], new_len_log_2: usize) -> Box<[u8]> {
    todo!("Test this");

    debug_assert_eq!(cells.len(), cells.len().next_power_of_two());
    let old_len_log_2 = cells.len().trailing_zeros() as usize / D::NDIM as usize;
    debug_assert_eq!(cells.len(), 1 << (old_len_log_2 * D::NDIM as usize));
    debug_assert!(old_len_log_2 <= new_len_log_2);

    let mut ret = vec![0_u8; 1 << (new_len_log_2 * D::NDIM)].into_boxed_slice();
    let offset = ((1 << old_len_log_2) - (1 << new_len_log_2)) / 2;
    let new_strides = strides_for_square::<D>(new_len_log_2);
    for (&cell, new_pos) in cells
        .iter()
        .zip((square_urect(old_len_log_2) + offset).iter())
    {
        ret[(new_pos * new_strides).sum()] = cell;
    }
    ret
}

/// Returns the strides used to index a power-of-2 hypercube of a cells.
pub fn strides_for_square<D: Dim>(len_log_2: usize) -> UVec<D> {
    UVec::from_fn(|ax| 1 << (len_log_2 * ax as usize))
}

/// Returns the `NdRect` bounding a power-of-2 hypercube of cells, with the
/// minimum corner at the origin.
pub fn square_urect<D: Dim>(len_log_2: usize) -> URect<D> {
    URect::span(UVec::origin(), UVec::repeat((1_usize << len_log_2) - 1))
}
