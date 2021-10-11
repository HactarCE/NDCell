use codemap::Span;
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use ndcell_core::ndarray::Array6D;
use ndcell_core::ndvec::{IVec6D, UVec6D};

use super::{LangCell, LangInt, VectorSet};
use crate::errors::{Error, Result};
use crate::llvm;

/// Masked N-dimensional cell array.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CellArray {
    shape: Arc<VectorSet>,
    cells: Arc<Array6D<LangCell>>,
}
impl fmt::Display for CellArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}).fill(", self.shape)?;

        // Write a comma-separated list of cell values.
        let mut cells_iter = self.cells_iter();
        if let Some(c) = cells_iter.next() {
            write!(f, "#{}", c)?;
            // If all cells are equal, stop after the first cell.
            if !self.cells_iter().all_equal() {
                for c in cells_iter {
                    write!(f, ", #{}", c)?;
                }
            }
        }

        write!(f, ")")?;
        Ok(())
    }
}
impl CellArray {
    /// Constructs a new cell array with all cells having the same value.
    pub fn from_cell(shape: Arc<VectorSet>, cell: LangCell) -> Self {
        let cells = Arc::new(Array6D::from_flat_slice(
            shape.bounds_size(),
            vec![cell; shape.bounds_len()],
        ));
        Self { shape, cells }
    }
    /// Constructs a new cell array from a list of cells within the mask.
    /// Returns a user-friendly error if the number of cells does not match the
    /// shape.
    pub fn from_cells(
        span: Span,
        shape: Arc<VectorSet>,
        masked_cells: &[LangCell],
    ) -> Result<Self> {
        // Check that the number of cells is correct.
        if masked_cells.len() != shape.len() {
            return Err(Error::wrong_cell_count(
                span,
                shape.len(),
                masked_cells.len(),
            ));
        }

        let mut masked_cells = masked_cells.iter().copied();
        let cells_array_data = match shape.mask_iter() {
            Some(mask_iter) => mask_iter
                .map(|in_| {
                    if in_ {
                        // This `0_u8` should never be used.
                        masked_cells.next().unwrap_or(0_u8)
                    } else {
                        0_u8
                    }
                })
                .collect_vec(),
            None => masked_cells.collect_vec(),
        };

        let cells = Arc::new(Array6D::from_flat_slice(
            shape.bounds_size(),
            cells_array_data,
        ));
        Ok(Self { shape, cells })
    }

    /// Returns the shape of the cell array.
    pub fn shape(&self) -> &Arc<VectorSet> {
        &self.shape
    }
    /// Returns the raw array of cell values.
    pub fn cells_array(&self) -> &Array6D<LangCell> {
        &self.cells
    }
    /// Returns a mutable reference to the raw array of cell values.
    pub fn cells_array_mut(&mut self) -> &mut Array6D<LangCell> {
        Arc::make_mut(&mut self.cells)
    }
    /// Returns the number of dimensions.
    pub fn ndim(&self) -> usize {
        self.shape.vec_len()
    }

    /// Returns the cell state at a position, or `None` if the position is
    /// outside the mask.
    pub fn get_cell(&self, pos: &[LangInt]) -> Option<LangCell> {
        let uvec = self.cell_array_uvec(vec_to_ivec6d(pos)?)?;
        Some(self.cells[uvec])
    }
    /// Returns a mutable reference to the cell state at a position, or `None`
    /// if the position is outside the mask.
    pub fn get_cell_mut(&mut self, pos: &[LangInt]) -> Option<&mut LangCell> {
        let uvec = self.cell_array_uvec(vec_to_ivec6d(pos)?)?;
        Some(&mut self.cells_array_mut()[uvec])
    }
    /// Returns a pointer to the origin in this array, which may be outside the
    /// allocation, or a null pointer if the array is empty.
    pub fn get_origin_ptr(&mut self) -> *mut LangCell {
        if let Some(bounds) = self.shape().bounds() {
            let pos = -bounds.min();
            let size = bounds.size();

            // TODO: extract strides calculation into something generic in ndcell_core
            let mut offset = 0;
            let mut stride = 1;
            for &ax in ndcell_core::axis::AXES {
                offset += pos[ax] * stride;
                stride *= size[ax];
            }

            let base_ptr = self.cells_array_mut().as_flat_slice_mut().as_mut_ptr();
            base_ptr.wrapping_offset(offset)
        } else {
            std::ptr::null_mut()
        }
    }

    fn cell_array_uvec(&self, pos: IVec6D) -> Option<UVec6D> {
        self.shape()
            .contains(pos)
            .then(|| self.cell_array_uvec_unchecked(pos))
    }
    fn cell_array_uvec_unchecked(&self, pos: IVec6D) -> UVec6D {
        (pos - self.shape().bounds().unwrap().min()).to_uvec()
    }

    /// Returns an iterator over the masked cells in this array, or None if the
    /// array is empty.
    pub fn cells_iter<'a>(&'a self) -> impl 'a + Iterator<Item = LangCell> {
        let mut mask_iter = self.shape().mask_iter();
        let cells_iter = self.cells.as_flat_slice().iter().copied();
        cells_iter.filter(move |_| match &mut mask_iter {
            Some(m) => m.next().unwrap_or(false),
            None => true,
        })
    }
    /// Returns whether the cell array contains at least one nonzero cell.
    pub fn any_nonzero(&self) -> bool {
        self.cells_iter().any(|c| c != 0_u8)
    }
}

fn vec_to_ivec6d(v: &[LangInt]) -> Option<IVec6D> {
    IVec6D::try_from_fn(|ax| (*v.get(ax as usize).unwrap_or(&0)).try_into().ok())
}
fn ivec6d_to_vec(vec_len: usize, v: IVec6D) -> Vec<LangInt> {
    (0..vec_len.clamp(1, 6))
        .map(|i| v.0[i] as LangInt)
        .collect()
}

/// LLVM representation of a masked N-dimensional cell array.
#[derive(Debug, Clone)]
pub struct LlvmCellArray {
    shape: Arc<VectorSet>,
    cells: Option<llvm::NdArrayValue>,
    mask: Option<llvm::NdArrayValue>,
}
impl LlvmCellArray {
    /// Constructs an empty cell array.
    pub fn empty(span: Span, ndim: usize) -> Result<Self> {
        Ok(Self {
            shape: Arc::new(VectorSet::empty(span, ndim)?),
            cells: None,
            mask: None,
        })
    }
    /// Constructs a new cell array.
    pub fn new(
        module: &mut llvm::Module,
        shape: Arc<VectorSet>,
        cells: llvm::NdArrayValue,
    ) -> Self {
        let cells = Some(cells);
        let mask = shape.mask().as_ref().map(|m| {
            llvm::NdArrayValue::new_const(
                module,
                shape.vec_len(),
                shape.bounds().unwrap(),
                llvm::bool_type(),
                &m.as_flat_slice()
                    .iter()
                    .copied()
                    .map(llvm::const_bool)
                    .collect_vec(),
                "cell_array_mask",
            )
        });
        Self { shape, cells, mask }
    }
    /// Constructs a new cell array with static contents.
    pub fn new_const(module: &mut llvm::Module, a: &CellArray) -> Self {
        let shape = Arc::clone(a.shape());
        if let Some(bounds) = shape.bounds() {
            let cell_values = &a
                .cells_array()
                .as_flat_slice()
                .iter()
                .map(|&i| llvm::const_cell(i))
                .collect_vec();
            let cells = llvm::NdArrayValue::new_const(
                module,
                a.ndim(),
                bounds,
                llvm::cell_type(),
                cell_values,
                "const_cell_array",
            );
            Self::new(module, shape, cells)
        } else {
            Self {
                shape,
                cells: None,
                mask: None,
            }
        }
    }

    /// Returns the shape of the cell array.
    pub fn shape(&self) -> &Arc<VectorSet> {
        &self.shape
    }
    /// Returns the raw array of mask booleans, or `None` if a mask is not
    /// necessary.
    pub fn mask(&self) -> Option<llvm::NdArrayValue> {
        self.mask
    }
    /// Returns the raw array of cell values.
    pub fn cells(&self) -> Option<llvm::NdArrayValue> {
        self.cells
    }
    /// Returns the number of dimensions.
    pub fn ndim(&self) -> usize {
        self.shape.vec_len()
    }

    /// Apply a new mask to the cell array.
    ///
    /// Only positions included in the existing cell array and the new mask are
    /// included in the result, so the resulting cell array will contain the
    /// intersection of the two.
    pub fn remask(
        &self,
        module: &mut llvm::Module,
        span: Span,
        new_mask: &VectorSet,
    ) -> Result<Self> {
        if let Some(cells) = self.cells() {
            let shape = Arc::new(self.shape.union(span, new_mask)?);
            Ok(Self::new(module, shape, cells))
        } else {
            Ok(self.clone()) // empty
        }
    }
}
