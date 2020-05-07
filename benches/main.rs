extern crate fpjson;

use criterion::{criterion_group, criterion_main, Criterion};
use fpjson::parse;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn bench_canada(c: &mut Criterion) {
    let input = File::open("benches/data/canada.json").unwrap();
    let buffered = BufReader::new(input);
    let mut json = String::new();
    for line in buffered.lines() {
        json.push_str(line.unwrap().as_str());
    }
    c.bench_function("parse canada.json", |b| {
        b.iter(|| parse(json.as_str()).unwrap())
    });
}

fn bench_citm_catalog(c: &mut Criterion) {
    let input = File::open("benches/data/citm_catalog.json").unwrap();
    let buffered = BufReader::new(input);
    let mut json = String::new();
    for line in buffered.lines() {
        json.push_str(line.unwrap().as_str());
    }
    c.bench_function("parse citm_catalog.json", |b| {
        b.iter(|| parse(json.as_str()).unwrap())
    });
}

fn bench_twitter(c: &mut Criterion) {
    let input = File::open("benches/data/twitter.json").unwrap();
    let buffered = BufReader::new(input);
    let mut json = String::new();
    for line in buffered.lines() {
        json.push_str(line.unwrap().as_str());
    }
    c.bench_function("parse twitter.json", |b| {
        b.iter(|| parse(json.as_str()).unwrap())
    });
}

criterion_group!(benches, bench_canada, bench_citm_catalog, bench_twitter);
// criterion_group!(benches, bench_citm_catalog);
// criterion_group!(benches, bench_twitter);

criterion_main!(benches);
