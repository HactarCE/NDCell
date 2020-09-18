use criterion::{criterion_group, criterion_main, Benchmark, Criterion};
use ndcell_core::ndarray::NdArray;
use ndcell_core::prelude::*;
use ndcell_core::sim::rule::LIFE;
use std::sync::Arc;

mod patterns;

use patterns::*;

criterion_group!(cgol, hashlife_benchmark, methuselah_benchmark);
criterion_main!(cgol);

fn hashlife_benchmark(c: &mut Criterion) {
    bench_sim_2d(c, LIFE, patterns::GGG, 1 << 30, 1 << 20, 178_957_013_usize);
    bench_sim_2d(c, LIFE, patterns::GOTTS_DOTS, 512, 64, 1352_usize);
    bench_sim_2d(c, LIFE, patterns::CATACRYST, 512, 64, 1556_usize);
}

fn methuselah_benchmark(c: &mut Criterion) {
    // "Acorn" stabalizes after ~5200 gens.
    bench_sim_2d(c, LIFE, patterns::ACORN, 5248, 64, 633_usize);
}

fn bench_sim_2d(
    c: &mut Criterion,
    rule: impl 'static + Rule<Dim2D>,
    pattern: Pattern,
    gens: impl Into<BigInt>,
    step_size: impl Into<BigInt>,
    expected_pop: impl Into<BigUint>,
) {
    let gens = gens.into();
    let step_size = step_size.into();
    let expected_pop = expected_pop.into();
    let rule = rule.into_arc();

    let ndarray = {
        let automaton = Automaton2D::from_rle(pattern.rle).expect("RLE parsing failed");
        let node_cache = automaton.tree.cache().read();
        NdArray::from(automaton.tree.root().as_ref(&node_cache))
    };

    c.bench(
        &format!("{}_{}_gens_by_{}", pattern.name, gens, step_size),
        Benchmark::new("sim", move |b| {
            b.iter(|| {
                // Create a new automaton from the array so that the cache is empty.
                let _cache = NodeCache::new();
                let cache = _cache.read();
                let root = cache.get_from_cells(ndarray.clone().into_flat_slice());
                let automaton = Automaton2D {
                    tree: NdTree::from_node_centered(root),
                    rule: Arc::clone(&rule),
                    generations: 0.into(),
                };
                assert_eq!(expected_pop, run_simulation(automaton, &gens, &step_size))
            })
        })
        .sample_size(10),
    );
}

fn run_simulation(mut automaton: Automaton2D, gens: &BigInt, step_size: &BigInt) -> BigUint {
    while automaton.generation_count() < gens {
        automaton.step(step_size);
    }
    automaton.population()
}
