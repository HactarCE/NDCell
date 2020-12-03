use criterion::{criterion_group, criterion_main, BatchSize, Benchmark, Criterion};
use itertools::Itertools;
use ndcell_core::prelude::*;
use rand::Rng;

mod patterns;

use patterns::*;

criterion_group!(
    ndtree,
    recenter_benchmark,
    set_cells_benchmark,
    get_cells_benchmark,
);
criterion_main!(ndtree);

fn recenter_benchmark(c: &mut Criterion) {
    for &dx in &[0, 1, 2, 4, 8] {
        bench_recenter_2d(c, patterns::GGG, dx);
    }
}

fn bench_recenter_2d(c: &mut Criterion, pattern: Pattern, dx: isize) {
    let ndtree: NdTree2D = Rle::from_string_to_ndtree(pattern.rle).unwrap();
    c.bench(
        &format!("recenter_{}", pattern.name),
        Benchmark::new(&format!("{},{}", dx, 0), move |b| {
            b.iter(|| {
                let mut a = ndtree.clone();
                a.recenter(&NdVec::big([dx, 0]));
            })
        }),
    );
}

fn set_cells_benchmark(c: &mut Criterion) {
    c.bench(
        "set_cells",
        Benchmark::new("fresh_pool", |b| {
            let mut ndtree = NdTree3D::new();
            let mut rng = rand::thread_rng();
            b.iter_batched(
                || rand_pos_and_cell_state(&mut rng),
                move |(pos, cell_state)| ndtree.set_cell(&pos, cell_state),
                BatchSize::SmallInput,
            );
        }),
    );

    let mut rng = rand::thread_rng();
    let cells_to_set_vec = std::iter::from_fn(|| Some(rand_pos_and_cell_state(&mut rng)))
        .take(10000)
        .collect_vec();
    let node_pool = SharedNodePool::new();
    c.bench(
        "set_cells",
        Benchmark::new("used_pool", move |b| {
            let mut cells_to_set_iter = cells_to_set_vec.iter().cycle();

            let mut ndtree = NdTree3D::with_node_pool(node_pool.new_ref());
            b.iter(|| {
                let (pos, cell_state) = cells_to_set_iter.next().unwrap();
                ndtree.set_cell(pos, *cell_state);
            });
        }),
    );
}

fn get_cells_benchmark(c: &mut Criterion) {
    // Initialize with a bunch of random cells.
    let mut ndtree = NdTree3D::new();
    let mut rng = rand::thread_rng();
    let cells_to_set_vec = std::iter::from_fn(|| Some(rand_pos_and_cell_state(&mut rng)))
        .take(10000)
        .collect_vec();
    for (pos, cell_state) in &cells_to_set_vec {
        ndtree.set_cell(pos, *cell_state);
    }
    c.bench(
        "get_cells",
        Benchmark::new("", move |b| {
            let mut cells_to_get_iter = cells_to_set_vec.iter().cycle();
            b.iter(|| {
                let (pos, cell_state) = cells_to_get_iter.next().unwrap();
                assert_eq!(*cell_state, ndtree.get_cell(pos));
            });
        }),
    );
}

fn rand_pos(rng: &mut impl Rng) -> BigVec3D {
    let x = rng.gen::<i16>().into();
    let y = rng.gen::<i16>().into();
    let z = rng.gen::<i16>().into();
    NdVec([x, y, z])
}

fn rand_pos_and_cell_state(rng: &mut impl Rng) -> (BigVec3D, u8) {
    (rand_pos(rng), rng.gen())
}
