use std::collections::HashSet;

use super::*;

fn get_non_default_set<D: Dim>(slice: NdTreeSlice<'_, D>) -> HashSet<BigVec<D>> {
    let mut ret = HashSet::new();
    if slice.root.is_empty() {
        return ret;
    }
    match slice.subdivide() {
        Ok(subslices) => {
            for subslice in subslices {
                ret.extend(get_non_default_set(subslice));
            }
        }
        Err((cell, pos)) => {
            if cell != 0 {
                ret.insert(pos.clone());
            }
        }
    }
    ret
}

fn make_cell_coords_set<D: Dim>(coords_vec: Vec<IVec<D>>) -> HashSet<BigVec<D>> {
    coords_vec.iter().map(IVec::to_bigvec).collect()
}

#[test]
fn test_cgol() {
    let mut grid = NdTree::new();
    let rule = rule::LIFE;
    let mut sim = Simulation::from(rule);

    // Make a glider.
    let cache = grid.cache();
    let node_access = cache.node_access();
    grid.set_cell(&node_access, &NdVec::big([3, 3]), 1);
    grid.set_cell(&node_access, &NdVec::big([4, 3]), 1);
    grid.set_cell(&node_access, &NdVec::big([5, 3]), 1);
    grid.set_cell(&node_access, &NdVec::big([5, 2]), 1);
    grid.set_cell(&node_access, &NdVec::big([4, 1]), 1);
    println!("{}", grid);
    println!();

    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([3, 3]),
            NdVec([4, 3]),
            NdVec([5, 3]),
            NdVec([5, 2]),
            NdVec([4, 1])
        ]),
        get_non_default_set(grid.slice(&grid.cache().node_access()))
    );
    // Simulate it for a few steps.
    sim.step(&mut grid, &1.into());
    println!("{}", grid);
    println!();
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([4, 4]),
            NdVec([4, 3]),
            NdVec([5, 3]),
            NdVec([5, 2]),
            NdVec([3, 2])
        ]),
        get_non_default_set(grid.slice(&grid.cache().node_access()))
    );
    sim.step(&mut grid, &1.into());
    println!("{}", grid);
    println!();
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([4, 4]),
            NdVec([5, 4]),
            NdVec([5, 3]),
            NdVec([5, 2]),
            NdVec([3, 3])
        ]),
        get_non_default_set(grid.slice(&grid.cache().node_access()))
    );
    // Simulate it for a much bigger step.
    sim.step(&mut grid, &64.into());
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([20, 20]),
            NdVec([21, 20]),
            NdVec([21, 19]),
            NdVec([21, 18]),
            NdVec([19, 19])
        ]),
        get_non_default_set(grid.slice(&grid.cache().node_access()))
    );
    // And an even bigger one.
    sim.step(&mut grid, &1024.into());
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([276, 276]),
            NdVec([277, 276]),
            NdVec([277, 275]),
            NdVec([277, 274]),
            NdVec([275, 275])
        ]),
        get_non_default_set(grid.slice(&grid.cache().node_access()))
    );
}
