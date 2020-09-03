use std::collections::HashSet;
use std::sync::Arc;

use crate::prelude::*;

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
        Err((leaf, pos)) => {
            for (offset, &cell) in leaf.rect().iter().zip(leaf.cells()) {
                if cell != 0 {
                    ret.insert(pos + offset);
                }
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
    let mut grid = NdTree::default();
    let _node_cache = Arc::clone(grid.cache());
    let node_cache = _node_cache.read().unwrap();
    let rule = crate::sim::rule::LIFE;
    let mut sim = Simulation::from(rule);

    // Make a glider.
    grid.set_cell(&*node_cache, &NdVec::big([3, 3]), 1);
    grid.set_cell(&*node_cache, &NdVec::big([4, 3]), 1);
    grid.set_cell(&*node_cache, &NdVec::big([5, 3]), 1);
    grid.set_cell(&*node_cache, &NdVec::big([5, 2]), 1);
    grid.set_cell(&*node_cache, &NdVec::big([4, 1]), 1);
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
        get_non_default_set(grid.slice(&*node_cache))
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
        get_non_default_set(grid.slice(&*node_cache))
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
        get_non_default_set(grid.slice(&*node_cache))
    );
    // Simulate it for a much bigger step.
    sim.step(&mut grid, &64.into());
    println!("Simulated for 64");
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([20, 20]),
            NdVec([21, 20]),
            NdVec([21, 19]),
            NdVec([21, 18]),
            NdVec([19, 19])
        ]),
        get_non_default_set(grid.slice(&*node_cache))
    );
    // And an even bigger one.
    sim.step(&mut grid, &1024.into());
    println!("Simulated for 1024");
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([276, 276]),
            NdVec([277, 276]),
            NdVec([277, 275]),
            NdVec([277, 274]),
            NdVec([275, 275])
        ]),
        get_non_default_set(grid.slice(&*node_cache))
    );
}
