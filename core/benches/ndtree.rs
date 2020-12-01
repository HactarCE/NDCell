use criterion::{criterion_group, criterion_main, BatchSize, Benchmark, Criterion};
use itertools::Itertools;
use ndcell_core::prelude::*;
use rand::Rng;
use std::sync::Arc;

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
    let ndtree = NdTree2D::from_rle_str(pattern.rle).unwrap();
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
        &format!("set_cells"),
        Benchmark::new(&format!("fresh_cache__rwlock_read_each"), |b| {
            let mut ndtree = NdTree3D::new();
            let _cache = Arc::clone(ndtree.cache());
            let mut rng = rand::thread_rng();
            b.iter_batched(
                || rand_pos_and_cell_state(&mut rng),
                move |(pos, cell_state)| ndtree.set_cell(&_cache.read(), &pos, cell_state),
                BatchSize::SmallInput,
            );
        }),
    );

    c.bench(
        &format!("set_cells"),
        Benchmark::new(&format!("fresh_cache__rwlock_read_amortized"), |b| {
            let mut ndtree = NdTree3D::new();
            let _cache = Arc::clone(ndtree.cache());
            let cache = _cache.read();
            let mut rng = rand::thread_rng();
            b.iter_batched(
                || rand_pos_and_cell_state(&mut rng),
                |(pos, cell_state)| ndtree.set_cell(&cache, &pos, cell_state),
                BatchSize::SmallInput,
            );
        }),
    );

    let mut rng = rand::thread_rng();
    let cells_to_set_vec = std::iter::from_fn(|| Some(rand_pos_and_cell_state(&mut rng)))
        .take(10000)
        .collect_vec();
    let cache = NodeCache::new();
    c.bench(
        &format!("set_cells"),
        Benchmark::new(&format!("cached__rwlock_read_each"), move |b| {
            let mut cells_to_set_iter = cells_to_set_vec.iter().cycle();

            let mut ndtree = NdTree3D::with_cache(Arc::clone(&cache));
            let _cache = Arc::clone(ndtree.cache());
            b.iter(|| {
                let (pos, cell_state) = cells_to_set_iter.next().unwrap();
                ndtree.set_cell(&_cache.read(), pos, *cell_state);
            });
        }),
    );

    let mut rng = rand::thread_rng();
    let cells_to_set_vec = std::iter::from_fn(|| Some(rand_pos_and_cell_state(&mut rng)))
        .take(10000)
        .collect_vec();
    let cache = NodeCache::new();
    c.bench(
        &format!("set_cells"),
        Benchmark::new(&format!("cached__rwlock_read_amortized"), move |b| {
            let mut cells_to_set_iter = cells_to_set_vec.iter().cycle();

            let mut ndtree = NdTree3D::with_cache(Arc::clone(&cache));
            let _cache = Arc::clone(ndtree.cache());
            let cache = _cache.read();
            b.iter(|| {
                let (pos, cell_state) = cells_to_set_iter.next().unwrap();
                ndtree.set_cell(&cache, pos, *cell_state)
            });
        }),
    );
}

fn get_cells_benchmark(c: &mut Criterion) {
    // Initialize with a bunch of random cells.
    let mut ndtree = NdTree3D::new();
    let _cache = Arc::clone(ndtree.cache());
    let mut rng = rand::thread_rng();
    let cells_to_set_vec = std::iter::from_fn(|| Some(rand_pos_and_cell_state(&mut rng)))
        .take(10000)
        .collect_vec();
    for (pos, cell_state) in &cells_to_set_vec {
        ndtree.set_cell(&_cache.read(), pos, *cell_state);
    }
    let ndtree_orig = ndtree;

    let ndtree = ndtree_orig.clone();
    let cells_to_get_vec = cells_to_set_vec.clone();
    c.bench(
        &format!("get_cells"),
        Benchmark::new(&format!("rwlock_read_each"), move |b| {
            let mut cells_to_get_iter = cells_to_get_vec.iter().cycle();
            let _cache = Arc::clone(ndtree.cache());
            b.iter(|| {
                let (pos, cell_state) = cells_to_get_iter.next().unwrap();
                assert_eq!(*cell_state, ndtree.get_cell(&_cache.read(), pos));
            });
        }),
    );

    let ndtree = ndtree_orig.clone();
    let cells_to_get_vec = cells_to_set_vec.clone();
    c.bench(
        &format!("get_cells"),
        Benchmark::new(&format!("rwlock_read_amortized"), move |b| {
            let mut cells_to_get_iter = cells_to_get_vec.iter().cycle();
            let _cache = Arc::clone(ndtree.cache());
            let cache = _cache.read();
            b.iter(|| {
                let (pos, cell_state) = cells_to_get_iter.next().unwrap();
                assert_eq!(*cell_state, ndtree.get_cell(&cache, pos));
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
