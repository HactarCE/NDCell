use ndcell_core::prelude::*;

fn main() {
    println!("heya");
    // let gotts_dots = Automaton2D::from_rle(
    //     "x = 187, y = 39, rule = B3/S23
    //     o$o$o9$4bo3bo$5bobo$6bo2bo$9bo$9bo9$185bo$186bo$182bo3bo$183b4o$179bo$
    //     180b2o$179bo$183b4o$182bo3bo$186bo$185bo$175bo$176bo$170bo5bo$171b6o!",
    // )
    // .unwrap();
    // run_simulation(&gotts_dots, 64, 4);
}

fn run_simulation(
    automaton: &Automaton2D,
    total_steps: impl Into<BigInt>,
    step_size: impl Into<BigInt>,
) -> BigUint {
    let mut automaton = automaton.clone();
    let total_steps = total_steps.into();
    let step_size = step_size.into();
    while automaton.generation_count() < &total_steps {
        automaton.step(&step_size);
    }
    automaton.population()
}
