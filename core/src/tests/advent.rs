//! Advent of Code 2020 day 17 solution

use std::fmt;

use crate::prelude::*;
use Axis::{X, Y};

#[derive(Debug)]
struct B3S23;
impl fmt::Display for B3S23 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "B3/S23")
    }
}
impl<D: Dim> NdRule<D> for B3S23 {
    fn radius(&self) -> usize {
        1
    }
    fn transition_function<'a>(&'a self) -> TransitionFunction<'a, D> {
        Box::new(|nbhd, rect| {
            crate::sim::rule::transition_cell_array(rect, |pos| {
                let this = nbhd[pos.clone()];
                let count = NdRect::centered(pos.clone(), 1)
                    .iter()
                    .map(|neighbor_pos| nbhd[neighbor_pos] as usize)
                    .sum::<usize>()
                    - this as usize;
                let ret = (count == 3 || (count == 2 && this != 0_u8)) as u8;
                ret
            })
        })
    }
    fn max_state(&self) -> u8 {
        1_u8
    }
}

#[test]
fn test_advent_of_code_2020_day_17() {
    // https://adventofcode.com/2020/day/17

    let example_pattern = "
        .#.
        ..#
        ###
    ";

    let puzzle_pattern = "
        #.#..###
        .#....##
        .###...#
        ..####..
        ....###.
        ##.#.#.#
        ..#..##.
        #.....##
    ";

    // Part 1 example (3D)
    test_pattern::<Dim3D>(example_pattern, 5, 112);
    // Part 1 (3D)
    test_pattern::<Dim3D>(puzzle_pattern, 30, 391);

    // Part 2 example (4D)
    test_pattern::<Dim4D>(example_pattern, 5, 848);
    // Part 2 (4D)
    test_pattern::<Dim4D>(puzzle_pattern, 30, 2264);
}

fn test_pattern<D: Dim>(plaintext: &str, initial_pop: usize, final_pop: usize) {
    let mut ndtree = NdTree::<D>::new();

    // Load pattern
    let mut pos = BigVec::origin();
    for row in plaintext.lines() {
        for ch in row.chars() {
            if ch == '#' {
                ndtree.set_cell(&pos, 1_u8);
            }
            pos[X] += 1;
        }
        pos[X].set_zero();
        pos[Y] += 1;
    }

    assert_eq!(BigUint::from(initial_pop), ndtree.root_ref().population());
    crate::sim::hashlife::step(&mut ndtree, &B3S23, &6.into());
    assert_eq!(BigUint::from(final_pop), ndtree.root_ref().population());
}
