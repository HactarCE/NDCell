use std::collections::HashSet;

use crate::prelude::*;

fn get_non_default_set<D: Dim>(slice: NdTreeSlice<'_, D>) -> HashSet<IVec<D>> {
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
                ret.insert(pos.to_ivec());
            }
        }
    }
    ret
}

fn make_cell_coords_set<D: Dim>(coords_vec: Vec<IVec<D>>) -> HashSet<IVec<D>> {
    coords_vec.iter().cloned().collect()
}

#[test]
fn test_cgol_glider() {
    let mut grid = NdTree::default();
    let rule = crate::sim::rule::LIFE;

    // Make a glider.
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
        get_non_default_set(grid.as_slice())
    );
    // Simulate it for a few steps.
    hashlife::step(&mut grid, &rule, &1.into());
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
        get_non_default_set(grid.as_slice())
    );
    hashlife::step(&mut grid, &rule, &1.into());
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
        get_non_default_set(grid.as_slice())
    );
    // Simulate it for a much bigger step.
    hashlife::step(&mut grid, &rule, &64.into());
    println!("Simulated for 64");
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([20, 20]),
            NdVec([21, 20]),
            NdVec([21, 19]),
            NdVec([21, 18]),
            NdVec([19, 19])
        ]),
        get_non_default_set(grid.as_slice())
    );
    // And an even bigger one.
    hashlife::step(&mut grid, &rule, &1024.into());
    println!("Simulated for 1024");
    assert_eq!(
        make_cell_coords_set(vec![
            NdVec([276, 276]),
            NdVec([277, 276]),
            NdVec([277, 275]),
            NdVec([277, 274]),
            NdVec([275, 275])
        ]),
        get_non_default_set(grid.as_slice())
    );
}

const GGG: &str = "x = 47, y = 14, rule = Life
16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
$26bobo!";

#[test]
fn regression_test_cgol_ggg() {
    let life = crate::sim::rule::LIFE.into_arc();
    let mut automaton = Rle::from_string_to_ndautomaton(GGG, life).unwrap();
    // Step 512 generations.
    for _ in 0..8 {
        automaton.step(&64.into());
    }
    assert_eq!(BigUint::from(119_usize), automaton.population());
}

#[test]
fn test_cgol_ggg_non_power_of_2() {
    let life = crate::sim::rule::LIFE.into_arc();
    let mut automaton = Rle::from_string_to_ndautomaton(GGG, life).unwrap();
    // Step 240 generations.
    for _ in 0..4 {
        automaton.step(&60.into());
    }
    assert_eq!(BigUint::from(78_usize), automaton.population());
    // Step 240 more generations.
    for _ in 0..4 {
        automaton.step(&60.into());
    }
    assert_eq!(BigUint::from(118_usize), automaton.population());
}
