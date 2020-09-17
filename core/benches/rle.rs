use criterion::{criterion_group, criterion_main, Criterion};
use ndcell_core::prelude::*;

mod patterns;

use patterns::Pattern;

criterion_group!(rle, from_rle_benchmark, to_rle_benchmark);
criterion_main!(rle);

fn from_rle_benchmark(c: &mut Criterion) {
    bench_from_rle_2d(c, patterns::GGG);
    bench_from_rle_2d(c, patterns::GOTTS_DOTS);
    bench_from_rle_2d(c, patterns::CATACRYST);
    bench_from_rle_2d(c, patterns::ACORN);
}

fn to_rle_benchmark(c: &mut Criterion) {
    bench_to_rle_2d(c, patterns::GGG);
    bench_to_rle_2d(c, patterns::GOTTS_DOTS);
    bench_to_rle_2d(c, patterns::CATACRYST);
    bench_to_rle_2d(c, patterns::ACORN);
}

fn bench_from_rle_2d(c: &mut Criterion, pattern: Pattern) {
    c.bench_function(&format!("{}_from_RLE", pattern.name), |b| {
        b.iter(|| Automaton2D::from_rle(pattern.rle).unwrap())
    });
}

fn bench_to_rle_2d(c: &mut Criterion, pattern: Pattern) {
    let from_rle = Automaton2D::from_rle(pattern.rle).unwrap();
    c.bench_function(&format!("{}_to_RLE", pattern.name), move |b| {
        b.iter(|| from_rle.to_rle())
    });
}