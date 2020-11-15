use criterion::{criterion_group, criterion_main, Benchmark, Criterion};
use ndcell_core::prelude::*;

mod patterns;

use patterns::*;

criterion_group!(ndtree, recenter_benchmark);
criterion_main!(ndtree);

fn recenter_benchmark(c: &mut Criterion) {
    for &dx in &[0, 1, 2, 4, 8] {
        bench_recenter_2d(c, patterns::GGG, dx);
    }
}

fn bench_recenter_2d(c: &mut Criterion, pattern: Pattern, dx: isize) {
    let ndtree = NdTree::from_rle_str(pattern.rle).unwrap();
    c.bench(
        &format!("recenter_{}", pattern.name),
        Benchmark::new(&format!("{},{}", dx, 0), move |b| {
            let cache = ndtree.cache().read();
            b.iter(|| {
                let mut a = ndtree.clone();
                a.recenter(&cache, &NdVec::big([dx, 0]));
            })
        }),
    );
}
