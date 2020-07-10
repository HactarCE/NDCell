use itertools::Itertools;

use super::{LangCellState, LangUint, INT_BITS};

/// Cell state filter value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CellStateFilter {
    /// Number of states in the automaton.
    state_count: usize,
    /// Bits of the filter (each bit corresponds to a cell state).
    bits: Vec<LangUint>,
}
impl CellStateFilter {
    /// Returns a new CellStateFilter that does not match any cells.
    pub fn none(state_count: usize) -> Self {
        let bits = vec![0; Self::vec_len_for_state_count(state_count)];
        Self { state_count, bits }
    }
    /// Returns a new CellStateFilter that matches only the given cell.
    pub fn single_cell_state(state_count: usize, cell_state: LangCellState) -> Self {
        Self::from_cell_states(state_count, std::iter::once(&cell_state))
    }
    /// Returns a new CellStateFilter that matches only the given cells.
    pub fn from_cell_states<'a>(
        state_count: usize,
        cell_states: impl IntoIterator<Item = &'a LangCellState>,
    ) -> Self {
        let mut ret = Self::none(state_count);
        for &cell_state in cell_states {
            ret.set_bit(cell_state, true);
        }
        ret
    }
    /// Constructs a new CellStateFilter from a raw array of integers.
    pub fn from_bits(state_count: usize, bits: Vec<LangUint>) -> Self {
        assert_eq!(bits.len(), Self::vec_len_for_state_count(state_count));
        Self { state_count, bits }
    }

    /// Returns a reference to the array of integers backing this filter.
    pub fn as_bits(&self) -> &[LangUint] {
        &self.bits
    }
    /// Returns a mutable reference to the array of integers backing this
    /// filter.
    pub fn as_bits_mut(&mut self) -> &mut [LangUint] {
        &mut self.bits
    }

    pub fn state_count(&self) -> usize {
        self.state_count
    }

    /// Returns the given bit of this cell state filter.
    pub fn get_bit(&self, cell_state: LangCellState) -> bool {
        let (array_idx, bit_idx) = Self::byte_and_bit_idx(cell_state);
        self.bits[array_idx] & (1 << bit_idx) != 0
    }
    /// Sets the given bit of this cell state filter to the given value.
    pub fn set_bit(&mut self, cell_state: LangCellState, value: bool) {
        let (array_idx, bit_idx) = Self::byte_and_bit_idx(cell_state);
        if value {
            self.bits[array_idx] |= 1 << bit_idx;
        } else {
            self.bits[array_idx] &= !(1 << bit_idx);
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

    /// Returns the vector length required for a cell state filter given the
    /// number of states in the automaton.
    pub fn vec_len_for_state_count(state_count: usize) -> usize {
        crate::utils::div_ceil(state_count, INT_BITS as usize)
    }
}

impl std::ops::BitAnd for CellStateFilter {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        assert_eq!(self.state_count(), rhs.state_count());
        Self::from_bits(
            self.state_count(),
            self.as_bits()
                .iter()
                .zip(rhs.as_bits())
                .map(|(&l, &r)| l & r)
                .collect_vec(),
        )
    }
}
impl std::ops::BitOr for CellStateFilter {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        assert_eq!(self.state_count(), rhs.state_count());
        Self::from_bits(
            self.state_count(),
            self.as_bits()
                .iter()
                .zip(rhs.as_bits())
                .map(|(&l, &r)| l | r)
                .collect_vec(),
        )
    }
}
impl std::ops::BitXor for CellStateFilter {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        assert_eq!(self.state_count(), rhs.state_count());
        Self::from_bits(
            self.state_count(),
            self.as_bits()
                .iter()
                .zip(rhs.as_bits())
                .map(|(&l, &r)| l ^ r)
                .collect_vec(),
        )
    }
}
impl std::ops::Not for CellStateFilter {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self::from_bits(
            self.state_count(),
            self.as_bits().iter().map(|&x| !x).collect_vec(),
        )
    }
}
