//! Octree traversal algorithm based on An Efficient Parametric Algorithm for
//! Octree Traversal by J. Revelles, C. Ureña, M. Lastra.
//!
//! This is also implemented in GLSL in the octree fragment shader.

// TODO: rewrite using cgmath so that division by zero is safe

use ndcell_core::prelude::*;

use crate::Face;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Hit {
    pub start: FVec3D,
    pub delta: FVec3D,

    pub t0: R64,
    pub t1: R64,

    pub pos_int: IVec3D,
    pub pos_float: FVec3D,
    pub face: Face,
}
impl Hit {
    pub fn add_base_pos(mut self, base_pos: IVec3D) -> Self {
        self.start += base_pos.to_fvec();
        self.pos_int += base_pos;
        self.pos_float += base_pos.to_fvec();
        self
    }
}

/// Computes a 3D raycast into an octree. `start` and `delta` are both in units
/// of `min_layer` nodes, and `start` is relative to the lower corner of `node`.
pub fn intersect_octree(
    mut start: FVec3D,
    mut delta: FVec3D,
    min_layer: Layer,
    node: NodeRef<'_, Dim3D>,
) -> Option<Hit> {
    let node_len = r64((node.layer() - min_layer).big_len().to_f64().unwrap());

    let original_start = start;
    let original_delta = delta;

    // Check each component of the delta vector to see if it's negative. If it
    // is, then mirror the ray along that axis so that the delta vector is
    // positive and also mirror the quadtree along that axis using
    // `invert_mask`, which will flip bits of child indices.
    let mut invert_mask = 0;
    for &ax in Dim3D::axes() {
        if delta[ax].is_negative() {
            invert_mask |= ax.bit();
            start[ax] = -start[ax] + node_len;
            delta[ax] = -delta[ax];
        }
    }
    // Please don't crash on division by zero!
    ensure_nonzero_delta(&mut delta);

    // At what `t` does the ray enter the root node (considering only one axis
    // at a time)?
    let t0: FVec3D = -start / delta;
    // At what `t` does the ray exit the root node (considering only one axis at
    // a time)?
    let t1: FVec3D = (-start + node_len) / delta;

    // At what `t` has the ray entered the root node along all axes?
    let max_t0: R64 = *t0.max_component();
    // At what `t` has the ray entered the root node along all axes?
    let min_t1: R64 = *t1.min_component();
    // If we enter AFTER we exit, or exit at a negative `t` ...
    if max_t0 >= min_t1 || r64(0.0) > min_t1 {
        // ... then the ray does not intersect with the root node.
        return None;
    } else {
        // Otherwise, the ray does intersect with the root node.
        raycast_node_child(start, delta, t0, t1, min_layer, node, invert_mask).map(|(t0, t1)| {
            let tm = (t0 + t1) / 2.0;

            let max_t0: R64 = *t0.max_component();
            let min_t1: R64 = *t1.min_component();

            let pos_int = (original_start + original_delta * tm).floor().to_ivec();
            let pos_float = original_start + original_delta * max_t0;
            let entry_axis = entry_axis(t0);
            let face = if invert_mask & entry_axis.bit() == 0 {
                Face::negative(entry_axis) // ray is positive; hit negative face of cell
            } else {
                Face::positive(entry_axis) // ray is negative; hit positive face of cell
            };

            Hit {
                start,
                delta,

                t0: max_t0,
                t1: min_t1,

                pos_int,
                pos_float,
                face,
            }
        })
    }
}

fn raycast_node_child(
    start: FVec3D,
    delta: FVec3D,
    t0: FVec3D,
    t1: FVec3D,
    min_layer: Layer,
    node: NodeRef<'_, Dim3D>,
    invert_mask: usize,
) -> Option<(FVec3D, FVec3D)> {
    if *t1.min_component() < r64(0.0) {
        // This node is completely behind the ray; skip it.
        return None;
    } else if *t0.max_component() < r64(0.0) && node.layer() <= min_layer {
        // This is a leaf node and the ray starts inside it; skip it.
        return None;
    }

    if node.is_empty() {
        // This is an empty node; skip it.
        return None;
    }

    // At what `t` does the ray cross the middle of the current node
    // (considering only one axis at a time)?
    let tm: FVec3D = (t0 + t1) / 2.0;

    if node.layer() <= min_layer {
        // This is a nonzero leaf node, so return a hit.
        return Some((t0, t1));
    }

    let children = node.subdivide().unwrap();
    let mut child_index = entry_child(t0, tm);
    loop {
        // Compute the parameter `t` values for th  `child_index`.
        let mut child_t0 = t0;
        let mut child_t1 = tm;
        for &ax in Dim3D::axes() {
            if child_index & ax.bit() != 0 {
                child_t0[ax] = tm[ax];
                child_t1[ax] = t1[ax];
            }
        }

        // Recurse!
        match raycast_node_child(
            start,
            delta,
            child_t0,
            child_t1,
            min_layer,
            children[child_index ^ invert_mask],
            invert_mask,
        ) {
            Some(hit) => return Some(hit),
            None => (),
        }

        // Compute the sibling of `child_index` to visit after this one.
        let exit_axis = exit_axis(child_t1);
        if child_index & exit_axis.bit() != 0 {
            // The ray has exited the current node.
            return None;
        } else {
            // Advance along `exit_axis` to get the next child to visit.
            child_index |= exit_axis.bit();
        }
    }
}

/// Computes the intersection of a 3D ray and a plane.
pub fn intersect_plane(
    start: FVec3D,
    delta: FVec3D,
    perpendicular_axis: Axis,
    perpendicular_coordinate: R64,
) -> Option<Hit> {
    if delta[perpendicular_axis].is_zero() {
        return None; // The delta vector is parallel to the plane.
    }

    let t = (perpendicular_coordinate - start[perpendicular_axis]) / delta[perpendicular_axis];
    if t <= 0.0 {
        return None; // The plane is behind the vector.
    }

    let face = if delta[perpendicular_axis] > 0.0 {
        Face::negative(perpendicular_axis)
    } else {
        Face::positive(perpendicular_axis)
    };
    let pos_float = start + delta * t;
    let pos_int = (pos_float - face.normal_fvec() / 2.0).floor().to_ivec();

    Some(Hit {
        start,
        delta,

        t0: t,
        t1: t,

        pos_int,
        pos_float,
        face,
    })
}

/// Computes the intersection of a 3D ray and a cuboid.
pub fn intersect_cuboid(start: FVec3D, mut delta: FVec3D, cuboid: FRect3D) -> Option<Hit> {
    let mut entry_corner = cuboid.min();
    let mut exit_corner = cuboid.max();

    for &ax in Dim3D::axes() {
        if delta[ax].is_negative() {
            std::mem::swap(&mut entry_corner[ax], &mut exit_corner[ax]);
        }
    }
    // Please don't crash on division by zero!
    ensure_nonzero_delta(&mut delta);

    // At what `t` does the ray enter the cuboid (considering only one axis at a
    // time)?
    let t0 = (entry_corner - start) / delta;
    // At what `t` does the ray exit the cuboid (considering only one axis at
    // a time)?
    let t1 = (exit_corner - start) / delta;

    // At what `t` has the ray entered the cuboid along all axes?
    let max_t0: R64 = *t0.max_component();
    // At what `t` has the ray entered the cuboid along all axes?
    let min_t1: R64 = *t1.min_component();

    // If we enter AFTER we exit, or exit at a negative `t` ...
    if max_t0 >= min_t1 || r64(0.0) > min_t1 {
        // ... then the ray does not intersect with the cuboid.
        return None;
    } else {
        // Otherwise, the ray does intersect with the cuboid.
        let pos_int = (start + delta * (max_t0 + 0.5)).floor().to_ivec();
        let pos_float = start + delta * max_t0;
        let entry_axis = entry_axis(t0);
        let face = if delta[entry_axis].is_positive() {
            Face::negative(entry_axis) // ray is positive; hit negative face of cell
        } else {
            Face::positive(entry_axis) // ray is negative; hit positive face of cell
        };

        Some(Hit {
            start,
            delta,
            t0: max_t0,
            t1: min_t1,
            pos_int,
            pos_float,
            face,
        })
    }
}

// Given the parameters `t0` at which the ray enters a node along each axis and
// `tm` at which the ray crosses the middle of a node along each axis, returns
// the child index of the first child of that node intersected by the ray.
fn entry_child(t0: FVec3D, tm: FVec3D) -> usize {
    let max_t0 = t0.max_component(); // when the ray actually enters the node
    let mut child_index = 0;
    for &ax in Dim3D::axes() {
        if *max_t0 > tm[ax] {
            child_index |= ax.bit();
        }
    }
    return child_index;
}

// Given the parameters `t0` at which the ray enters a node along each axis,
// returns the axis along which the ray enters the node.
fn entry_axis(t0: FVec3D) -> Axis {
    t0.max_axis()
}

// Given the parameters `t1` at which the ray exits a node along each axis,
// returns the axis along which the ray exits the node.
fn exit_axis(t1: FVec3D) -> Axis {
    t1.min_axis()
}

/// Ensure that each component of the delta vector is nonzero by adjusting them
/// slightly if necessary.
fn ensure_nonzero_delta(delta: &mut FVec3D) {
    const EPSILON: f64 = std::f32::EPSILON as f64;
    for &ax in Dim3D::axes() {
        if delta[ax].abs() < EPSILON {
            delta[ax] = r64(EPSILON);
        }
    }
}
