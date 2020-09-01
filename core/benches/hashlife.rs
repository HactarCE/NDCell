use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ndcell_core::prelude::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    let gotts_dots = Automaton2D::from_rle(
        "x = 187, y = 39, rule = B3/S23
        o$o$o9$4bo3bo$5bobo$6bo2bo$9bo$9bo9$185bo$186bo$182bo3bo$183b4o$179bo$
        180b2o$179bo$183b4o$182bo3bo$186bo$185bo$175bo$176bo$170bo5bo$171b6o!",
    )
    .unwrap();
    c.bench_function("Gott's Dots 64 gens", |b| {
        b.iter(|| run_simulation(black_box(&gotts_dots), 64, 4))
    });

    let ggg = Automaton2D::from_rle(
        "x = 47, y = 14, rule = Life
        16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
        2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
        $26bobo!",
    )
    .unwrap();
    c.bench_function("Gosper's Glider Gun 1024 gens", |b| {
        b.iter(|| run_simulation(black_box(&ggg), 1024, 4))
    });
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

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
