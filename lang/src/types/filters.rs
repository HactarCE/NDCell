use itertools::Itertools;
use std::convert::TryInto;

use super::{LangCellState, LangUint, INT_BITS};
use crate::MAX_STATES;

/// Length of LangInt array used to represent a cell state filter.
pub const CELL_STATE_FILTER_ARRAY_LEN: usize = MAX_STATES / INT_BITS as usize;

/// Compile-time assertion that MAX_STATES is divisible by INT_BITS, based on
/// the static_assertions crate:
/// https://docs.rs/static_assertions/1.1.0/src/static_assertions/const_assert.rs.html#52.
const _: [(); MAX_STATES % INT_BITS as usize] = [];

/// Cell state filter value.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct CellStateFilter([LangUint; CELL_STATE_FILTER_ARRAY_LEN]);
impl CellStateFilter {
    /// Returns a new CellStateFilter that does not match any cells.
    pub fn new() -> Self {
        Self::default()
    }
    /// Returns a new CellStateFilter that matches only the given cell.
    pub fn single_cell_state(cell_state: LangCellState) -> Self {
        Self::from_cell_states(std::iter::once(&cell_state))
    }
    pub fn from_cell_states<'a>(cell_states: impl IntoIterator<Item = &'a LangCellState>) -> Self {
        let mut ret = Self::new();
        for &cell_state in cell_states {
            ret.set_bit(cell_state, true);
        }
        ret
    }

    /// Constructs a new CellStateFilter from a raw array of integers.
    pub fn from_ints(ints: [LangUint; CELL_STATE_FILTER_ARRAY_LEN]) -> Self {
        Self(ints)
    }
    /// Returns a reference to the array of integers backing this filter.
    pub fn as_ints(&self) -> &[LangUint] {
        &self.0
    }
    /// Returns a mutable reference to the array of integers backing this
    /// filter.
    pub fn as_ints_mut(&mut self) -> &mut [LangUint] {
        &mut self.0
    }

    /// Returns the given bit of this cell state filter.
    pub fn get_bit(self, cell_state: LangCellState) -> bool {
        let (array_idx, bit_idx) = Self::byte_and_bit_idx(cell_state);
        self.0[array_idx] & (1 << bit_idx) != 0
    }
    /// Sets the given bit of this cell state filter to the given value.
    pub fn set_bit(&mut self, cell_state: LangCellState, value: bool) {
        let (array_idx, bit_idx) = Self::byte_and_bit_idx(cell_state);
        if value {
            self.0[array_idx] |= 1 << bit_idx;
        } else {
            self.0[array_idx] &= !(1 << bit_idx);
        }
    }

    /// Returns the array and bit indices of the bit corresponding to the given
    /// cell state.
    fn byte_and_bit_idx(cell_state: LangCellState) -> (usize, usize) {
        (
            cell_state as usize / INT_BITS as usize,
            cell_state as usize % INT_BITS as usize,
        )
    }
}

impl std::ops::BitAnd for CellStateFilter {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(
            self.0
                .iter()
                .zip(&rhs.0)
                .map(|(&l, &r)| l & r)
                .collect_vec()
                .as_slice()
                .try_into()
                .unwrap(),
        )
    }
}
impl std::ops::BitOr for CellStateFilter {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(
            self.0
                .iter()
                .zip(&rhs.0)
                .map(|(&l, &r)| l | r)
                .collect_vec()
                .as_slice()
                .try_into()
                .unwrap(),
        )
    }
}
impl std::ops::BitXor for CellStateFilter {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(
            self.0
                .iter()
                .zip(&rhs.0)
                .map(|(&l, &r)| l ^ r)
                .collect_vec()
                .as_slice()
                .try_into()
                .unwrap(),
        )
    }
}
impl std::ops::Not for CellStateFilter {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(
            self.0
                .iter()
                .map(|&x| !x)
                .collect_vec()
                .as_slice()
                .try_into()
                .unwrap(),
        )
    }
}
