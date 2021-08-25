use codemap::Span;
use itertools::Itertools;
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use ndcell_core::prelude::IVec6D;
use ndcell_core::{ndarray::Array6D, ndvec::UVec6D};

use super::{LangCell, LangInt, VectorSet};
use crate::errors::Result;
use crate::llvm;

/// Masked N-dimensional cell array.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CellArray {
    shape: Arc<VectorSet>,
    cells: Array6D<LangCell>,
}
impl fmt::Display for CellArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("display CellArrary")
    }
}
impl CellArray {
    /// Constructs a new cell array with all cells having the same value.
    pub fn from_cell(shape: Arc<VectorSet>, cell: LangCell) -> Self {
        let cells = Array6D::from_flat_slice(shape.bounds_size(), vec![cell; shape.bounds_len()]);
        Self { shape, cells }
    }

    /// Returns the shape of the cell array.
    pub fn shape(&self) -> &Arc<VectorSet> {
        &self.shape
    }
    /// Returns the raw array of cell values.
    pub fn cells_array(&self) -> &Array6D<LangCell> {
        &self.cells
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
    /// Returns a mutable reference to the cell statee at a position, or `None`
    /// if the position is outside the mask.
    pub fn get_cell_mut(&mut self, pos: &[LangInt]) -> Option<&mut LangCell> {
        let uvec = self.cell_array_uvec(vec_to_ivec6d(pos)?)?;
        Some(&mut self.cells[uvec])
    }

    fn cell_array_uvec(&self, pos: IVec6D) -> Option<UVec6D> {
        self.shape()
            .contains(pos)
            .then(|| self.cell_array_uvec_unchecked(pos))
    }
    fn cell_array_uvec_unchecked(&self, pos: IVec6D) -> UVec6D {
        (pos - self.shape().bounds().unwrap().min()).to_uvec()
    }

    /// Returns whether the cell array contains at least one nonzero cell.
    pub fn any_nonzero(&self) -> bool {
        let mut cells_iter = self.cells.as_flat_slice().iter();
        match self.shape().mask_iter() {
            Some(m) => m.zip(cells_iter).any(|(in_, &c)| in_ && c != 0_u8),
            None => cells_iter.any(|&c| c != 0_u8),
        }
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
