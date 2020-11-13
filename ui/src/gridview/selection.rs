use ndcell_core::prelude::*;

use crate::config::Config;

pub type Selection2D = Selection<Dim2D>;
pub type Selection3D = Selection<Dim3D>;

#[derive(Debug, Clone)]
pub struct Selection<D: Dim> {
    pub rect: BigRect<D>,
    pub cells: Option<NdTree<D>>,
}
impl<D: Dim> Selection<D> {
    pub fn restore_history_entry(
        config: &Config,
        mut current: &mut Option<Self>,
        entry: Option<Self>,
    ) -> Option<Self> {
        let current_has_cells = current.as_ref().and_then(|s| s.cells.as_ref()).is_some();
        let entry_has_cells = entry.as_ref().and_then(|s| s.cells.as_ref()).is_some();

        if config.hist.record_select || entry_has_cells {
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

/// Resizes a selection given cursor movement from `drag_start_pos` to
/// `drag_end_pos` based on the distance covered during the drag.
pub fn resize_selection_relative<D: Dim>(
    initial_selection: &BigRect<D>,
    drag_start_pos: &FixedVec<D>,
    drag_end_pos: &FixedVec<D>,
    axes: AxisSet,
) -> BigRect<D> {
    // Farthest corner stays fixed.
    let pos1 = initial_selection.farthest_corner(drag_start_pos);
    // Closest corner varies.
    let mut pos2 = initial_selection.closest_corner(drag_start_pos);

    // Use delta from original cursor position to new cursor
    // position.
    let delta = drag_end_pos - drag_start_pos;
    for axis in axes {
        pos2[axis] += delta[axis].round().0;
    }

    NdRect::span(pos1, pos2)
}

/// Resizes a selection given cursor movement from `drag_start_pos` to
/// `drag_end_pos`, using the absolute `drag_start_pos` to infer which axes to
/// modify.
pub fn resize_selection_absolute<D: Dim>(
    initial_selection: &BigRect<D>,
    drag_start_pos: &FixedVec<D>,
    drag_end_pos: &FixedVec<D>,
) -> BigRect<D> {
    // Farthest corner stays fixed.
    let pos1 = initial_selection.farthest_corner(drag_start_pos);
    // Closest corner varies.
    let mut pos2 = initial_selection.closest_corner(drag_start_pos);

    let axes = absolute_selection_resize_axes(&initial_selection, drag_start_pos);
    for axis in axes {
        pos2[axis] = drag_end_pos[axis].floor().0;
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
        AxisSet::single(edge_distances.max_axis(|_, val| val))
    } else {
        // If any edge distance is positive, resize along the positive ones.
        AxisSet::from_fn(D::NDIM, |ax| edge_distances[ax].is_positive())
    }
}
