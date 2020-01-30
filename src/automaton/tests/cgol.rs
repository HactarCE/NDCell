use std::collections::HashSet;
use std::rc::Rc;

use super::*;

fn get_non_default_set<C: CellType, D: Dim>(slice: &NdTreeSlice<C, D>) -> HashSet<BigVec<D>> {
    let mut ret = HashSet::new();
    for (branch_idx, branch) in slice.root.branch_iter() {
        let branch_offset = &slice.offset + slice.root.branch_offset(branch_idx);
        match branch {
            NdTreeBranch::Leaf(cell_state) => {
                if *cell_state != C::default() {
                    ret.insert(branch_offset);
                }
            }
            NdTreeBranch::Node(node) => ret.extend(get_non_default_set(&NdTreeSlice {
                root: node.clone(),
                offset: branch_offset,
            })),
        }
    }
    ret
}

fn make_cell_coords_set<D: Dim>(coords_vec: Vec<IVec<D>>) -> HashSet<BigVec<D>> {
    coords_vec.iter().map(NdVec::convert).collect()
}

#[test]
fn test_cgol() {
    // TODO: load RLE instead of manually setting individual cells.
    let mut grid = NdTree::new();
    let rule = rule::LIFE;
    let mut sim = Simulation::new(Rc::new(rule));

    // Make a glider
    grid.set_cell(&NdVec::big([3, 3]), 1);
    grid.set_cell(&NdVec::big([4, 3]), 1);
    grid.set_cell(&NdVec::big([5, 3]), 1);
    grid.set_cell(&NdVec::big([5, 2]), 1);
    grid.set_cell(&NdVec::big([4, 1]), 1);
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
        get_non_default_set(&grid.slice)
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
        get_non_default_set(&grid.slice)
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
        get_non_default_set(&grid.slice)
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
        get_non_default_set(&grid.slice)
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
        get_non_default_set(&grid.slice)
    );
}
