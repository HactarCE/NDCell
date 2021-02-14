use ndcell_core::prelude::*;

use crate::CONFIG;

pub type Selection2D = Selection<Dim2D>;
pub type Selection3D = Selection<Dim3D>;

#[derive(Debug, Clone)]
pub struct Selection<D: Dim> {
    pub rect: BigRect<D>,
    pub cells: Option<NdTree<D>>,
}
impl<D: Dim> Selection<D> {
    pub fn restore_history_entry(
        mut current: &mut Option<Self>,
        entry: Option<Self>,
    ) -> Option<Self> {
        let current_has_cells = current.as_ref().and_then(|s| s.cells.as_ref()).is_some();
        let entry_has_cells = entry.as_ref().and_then(|s| s.cells.as_ref()).is_some();

        if CONFIG.lock().hist.record_select || entry_has_cells {
            std::mem::replace(&mut current, entry)
        } else if current_has_cells {
            std::mem::replace(&mut current, None)
        } else {
            current.clone()
        }
    }
}
impl<D: Dim> From<BigRect<D>> for Selection<D> {
    fn from(rect: BigRect<D>) -> Self {
        Self { rect, cells: None }
    }
}
impl<D: Dim> Selection<D> {
    pub fn from_str(
        s: &str,
        node_pool: &SharedNodePool<D>,
    ) -> Result<Option<Self>, Vec<CaFormatError>> {
        let mut errors: Vec<CaFormatError> = vec![];
        // TODO: find a nice way to do this without duplicating code from
        // ndcell_core::io::convert
        match Self::from_str_with_format::<Rle>(s, node_pool) {
            Ok(ok) => return Ok(ok),
            Err(e) => errors.push(e.into()),
        }
        match Self::from_str_with_format::<Macrocell>(s, node_pool) {
            Ok(ok) => return Ok(ok),
            Err(e) => errors.push(e.into()),
        }
        Err(errors)
    }
    pub fn from_str_with_format<F: CaFormatTrait>(
        s: &str,
        node_pool: &SharedNodePool<D>,
    ) -> Result<Option<Self>, F::Err> {
        let pattern: F = s.parse()?;
        if let Some(rect) = pattern.region().bounding_rect() {
            let cells = Some(pattern.to_ndtree(node_pool.new_ref())?);
            Ok(Some(Self { rect, cells }))
        } else {
            Ok(None)
        }
    }

    #[must_use = "This method returns a new value instead of mutating its input"]
    pub fn move_by(&self, delta: BigVec<D>) -> Self {
        Self {
            rect: self.rect.clone() + &delta,
            cells: self.cells.clone().map(|mut ndtree| {
                ndtree.set_base_pos(ndtree.base_pos() + delta);
                ndtree
            }),
        }
    }
}

/// Resizes a selection along a global `resize_delta`.
///
/// `resize_vector` is a vector where the sign of each component indicates the
/// direction to resize along that axis, with zero for axes where the selection
/// rectangle is not resized.
pub fn resize_selection_relative<D: Dim>(
    initial_selection: &BigRect<D>,
    resize_delta: &FixedVec<D>,
    resize_vector: &IVec<D>,
) -> BigRect<D> {
    // `pos1` stays fixed; `pos2` varies.
    let mut pos1 = initial_selection.min();
    let mut pos2 = initial_selection.max();
    for &ax in D::axes() {
        if resize_vector[ax] < 0 {
            std::mem::swap(&mut pos1[ax], &mut pos2[ax]);
        }
    }

    for &ax in D::axes() {
        if resize_vector[ax] != 0 {
            pos2[ax] += resize_delta[ax].round();

            // Clamp to `pos1`; prevent the selection from turning "inside out."
            if resize_vector[ax] > 0 && pos1[ax] > pos2[ax]
                || resize_vector[ax] < 0 && pos1[ax] < pos2[ax]
            {
                pos2[ax] = pos1[ax].clone()
            }
        }
    }

    NdRect::span(pos1, pos2)
}

/// Resizes a selection given cursor movement from `drag_start_pos` to
/// `drag_end_pos`, using the absolute `drag_start_pos` to infer which axes to
/// modify.
pub fn resize_selection_absolute<D: Dim>(
    initial_selection: &BigRect<D>,
    drag_start_pos: &FixedVec<D>,
    drag_end_render_cell: &BigRect<D>,
) -> BigRect<D> {
    // Farthest corner stays fixed.
    let pos1 = initial_selection.farthest_corner(drag_start_pos);
    // Closest corner varies.
    let mut pos2 = initial_selection.closest_corner(drag_start_pos);

    let axes = absolute_selection_resize_axes(&initial_selection, drag_start_pos);
    let drag_end_min = drag_end_render_cell.min();
    let drag_end_max = drag_end_render_cell.max();
    for ax in axes {
        pos2[ax] = if drag_end_max[ax] > pos1[ax] {
            drag_end_max[ax].clone()
        } else {
            drag_end_min[ax].clone()
        };
    }

    NdRect::span(pos1, pos2)
}

/// Returns a set of axes along which to resize a selection, given the initial
/// cursor position.
pub fn absolute_selection_resize_axes<D: Dim>(
    initial_selection: &BigRect<D>,
    drag_start_pos: &FixedVec<D>,
) -> AxisSet {
    let initial_selection = initial_selection.to_fixedrect();

    // Measure signed distance from the nearest edge of the selection along each
    // axis; distance is zero at the selection edge, increases moving away from
    // the selection box, and decreases moving toward the center of the
    // selection box.
    let edge_distances: FixedVec<D> = NdVec::max(
        &(initial_selection.min() - drag_start_pos),
        &(drag_start_pos - initial_selection.max()),
    );

    if edge_distances < NdVec::origin() {
        // If all edge distances are negative, the cursor is inside the
        // selection. In this case, resize along whichever is most positive.
        AxisSet::single(edge_distances.max_axis())
    } else {
        // If any edge distance is positive, resize along the positive ones.
        AxisSet::from_fn(D::NDIM, |ax| edge_distances[ax].is_positive())
    }
}
