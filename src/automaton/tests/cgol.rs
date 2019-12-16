use std::collections::HashSet;
use std::rc::Rc;

use super::*;

fn get_non_default_set<C: CellType, D: Dim>(slice: &NdTreeSlice<C, D>) -> HashSet<NdVec<D>> {
    let mut ret = HashSet::new();
    for (branch_idx, branch) in slice.root.branches.iter().enumerate() {
        let branch_offset = slice.offset + slice.root.branch_offset(branch_idx);
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

fn make_cell_coords_set<D: Dim>(coords_vec: Vec<D>) -> HashSet<NdVec<D>> {
    coords_vec.into_iter().map(Into::into).collect()
}

#[test]
fn test_cgol() {
    let mut grid = NdTree::new();
    let rule = rule::LIFE;
    let mut sim = Simulation::new(Rc::new(rule), 1);

    // Make a glider
    grid.set_cell([3, 3].into(), 1);
    grid.set_cell([4, 3].into(), 1);
    grid.set_cell([5, 3].into(), 1);
    grid.set_cell([5, 2].into(), 1);
    grid.set_cell([4, 1].into(), 1);
    println!("{}", grid);
    println!();

    assert_eq!(
        make_cell_coords_set(vec![[3, 3], [4, 3], [5, 3], [5, 2], [4, 1]]),
        get_non_default_set(&grid.slice)
    );
    // Simulate it for a few steps.
    sim.step(&mut grid);
    println!("{}", grid);
    println!();
    assert_eq!(
        make_cell_coords_set(vec![[4, 4], [4, 3], [5, 3], [5, 2], [3, 2]]),
        get_non_default_set(&grid.slice)
    );
    sim.step(&mut grid);
    println!("{}", grid);
    println!();
    assert_eq!(
        make_cell_coords_set(vec![[4, 4], [5, 4], [5, 3], [5, 2], [3, 3]]),
        get_non_default_set(&grid.slice)
    );
    sim.set_step_size(64);
    sim.step(&mut grid);
    assert_eq!(
        make_cell_coords_set(vec![[20, 20], [21, 20], [21, 19], [21, 18], [19, 19]]),
        get_non_default_set(&grid.slice)
    );
    sim.set_step_size(1024);
    sim.step(&mut grid);
    assert_eq!(
        make_cell_coords_set(vec![
            [276, 276],
            [277, 276],
            [277, 275],
            [277, 274],
            [275, 275]
        ]),
        get_non_default_set(&grid.slice)
    );
}
