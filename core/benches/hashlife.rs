use std::sync::Arc;

use criterion::{criterion_group, criterion_main, Criterion};
use ndcell_core::ndarray::NdArray;
use ndcell_core::prelude::*;
use ndcell_core::sim::rule::LIFE;

const GGG: &str = "x = 47, y = 14, rule = Life
16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
$26bobo!";

const GOTTS_DOTS: &str = "x = 187, y = 39, rule = B3/S23
o$o$o9$4bo3bo$5bobo$6bo2bo$9bo$9bo9$185bo$186bo$182bo3bo$183b4o$179bo$
180b2o$179bo$183b4o$182bo3bo$186bo$185bo$175bo$176bo$170bo5bo$171b6o!";

#[allow(unused)] // Catacryst is not very HashLife-friendly initially.
const CATACRYST: &str = "x = 2555, y = 1772, rule = B3/S23
2350bo$2350bo$2348b2o$2347bo$2347bo$2347bo$2347bo6$2377bo$2376b2o$
2374bo2bo2$2374bobo$2375bo133$2552b2o$2553b2o$2553bo29$2538bo$2537bobo
$2539bo$2540bo1489$1201bo$1201bo$1202b2o$1204bo$1204bo$1204bo$1204bo
17$1180b2o$1182bo$1182bo$1183b4o50$3bo$2b2o$o2bo2$obo$bo11$30bo$30bo$
28b2o$27bo$27bo$27bo$27bo!";

pub fn criterion_benchmark(c: &mut Criterion) {
    // bench_sim_2d(c, GGG, "Gosper's Glider Gun", LIFE, 64, 4, 53_usize);
    bench_sim_2d(c, GOTTS_DOTS, "Gott's Dots", LIFE, 64, 4, 209_usize);
    // bench_sim_2d(c, CATACRYST, "Catacryst", LIFE, 64, 4, 467_usize);

    bench_sim_2d(c, GGG, "Gosper's Glider Gun", LIFE, 256, 16, 77_usize);
    // bench_sim_2d(c, GOTTS_DOTS, "Gott's Dots", LIFE, 256, 16, 722_usize);
    // bench_sim_2d(c, CATACRYST, "Catacryst", LIFE, 256, 16, 941_usize);

    // bench_sim_2d(c, GGG, "Gosper's Glider Gun", LIFE, 1024, 64, 213_usize);
    // bench_sim_2d(c, GOTTS_DOTS, "Gott's Dots", LIFE, 1024, 64, 1946_usize);
    // bench_sim_2d(c, CATACRYST, "Catacryst", LIFE, 1024, 64, 2459_usize);

    // bench_sim_2d(c, GGG, "Gosper's Glider Gun", LIFE, 65536, 64, 10957_usize);
    // bench_sim_2d(c, GOTTS_DOTS, "Gott's Dots", LIFE, 16384, 64, 25828_usize);
    // bench_sim_2d(c, CATACRYST, "Catacryst", LIFE, 65536, 64, 72627_usize);
}

fn bench_sim_2d(
    c: &mut Criterion,
    rle: &str,
    pattern_name: &str,
    rule: impl 'static + Rule<Dim2D>,
    gens: impl Into<BigInt>,
    step_size: impl Into<BigInt>,
    expected_pop: impl Into<BigUint>,
) {
    let gens = gens.into();
    let step_size = step_size.into();
    let expected_pop = expected_pop.into();
    let sim = Arc::new(HashLife::from(rule));

    let ndarray = {
        let automaton = Automaton2D::from_rle(rle).expect("RLE parsing failed");
        let node_cache = automaton.tree.cache().read();
        NdArray::from(automaton.tree.root().as_ref(&node_cache))
    };

    c.bench_function(
        &format!("{} {} gens by {}", pattern_name, gens, step_size),
        |b| {
            b.iter(|| {
                // Create a new automaton from the array so that the cache is empty.
                let _cache = NodeCache::new();
                let cache = _cache.read();
                let root = cache.get_from_cells(ndarray.clone().into_flat_slice());
                let automaton = Automaton2D {
                    tree: NdTree::from_node(root),
                    sim: Arc::clone(&sim),
                    generations: 0.into(),
                };
                assert_eq!(expected_pop, run_simulation(automaton, &gens, &step_size))
            })
        },
    );
}

fn run_simulation(mut automaton: Automaton2D, gens: &BigInt, step_size: &BigInt) -> BigUint {
    while automaton.generation_count() < gens {
        automaton.step(step_size);
    }
    automaton.population()
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
