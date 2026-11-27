//! Benchmark of matching and rule application on the propositional workload (`tests/workload`).
//! The KeY counterpart is `tools/prop-bench/PropBench.java`; both print the same table.
//!
//! ```text
//! cargo run --release -p dale-logic --example prop_bench [-- <depth> <formulas> <problems>]
//! ```
//!
//! Single-threaded. Each phase is run `ROUNDS` times after a warm-up and the best and median round
//! are reported.

#[path = "../tests/workload/mod.rs"]
mod workload;

use std::time::{Duration, Instant};

use dale_logic::*;
use workload::*;

const ROUNDS: usize = 15;

fn median(v: &mut [Duration]) -> Duration {
    v.sort();
    v[v.len() / 2]
}

fn main() {
    let args: Vec<usize> = std::env::args()
        .skip(1)
        .filter_map(|a| a.parse().ok())
        .collect();
    let depth = args.first().copied().unwrap_or(6) as u32;
    let n_match = args.get(1).copied().unwrap_or(2000);
    let n_prove = args.get(2).copied().unwrap_or(200);

    // ---- phase 1: matching
    let mut d = Dale::new();
    let fs = corpus(1, n_match, depth);
    let ts: Vec<_> = fs.iter().map(|f| d.build(f)).collect();
    let size: usize = fs.iter().map(F::size).sum();
    let seqs: Vec<_> = ts
        .iter()
        .map(|t| {
            (
                d.one_sided(*t, Side::Antecedent),
                d.one_sided(*t, Side::Succedent),
            )
        })
        .collect();
    let attempts = (fs.len() * 8) as u64;
    let matches = match_all(&d, &seqs);
    let (mut best, mut all) = (Duration::MAX, Vec::new());
    for r in 0..ROUNDS + 3 {
        let t = Instant::now();
        let n = match_all(&d, &seqs);
        let e = t.elapsed();
        assert_eq!(n, matches);
        if r >= 3 {
            best = best.min(e);
            all.push(e);
        }
    }
    let med = median(&mut all);
    println!("DaLe propositional benchmark (depth {depth})");
    println!(
        "match:  {} formulas ({} nodes), {} attempts, {} matches",
        fs.len(),
        size,
        attempts,
        matches
    );
    println!(
        "        best {:>9.3} ms  median {:>9.3} ms   {:>7.1} ns/attempt (best)",
        best.as_secs_f64() * 1e3,
        med.as_secs_f64() * 1e3,
        best.as_nanos() as f64 / attempts as f64
    );

    // ---- phase 2: proof search with rule application, on one big conjunction
    let fs = corpus(2, n_prove, depth.min(5));
    let big = conjoin(&fs);
    let stats;
    let tautologies;
    {
        let mut d = Dale::new();
        let t = d.build(&big);
        let (st, closed) = saturate(&mut d, t, None);
        stats = st;
        tautologies = closed as u32;
    }
    let (mut best, mut best_apply, mut all) = (Duration::MAX, Duration::ZERO, Vec::new());
    for r in 0..ROUNDS + 3 {
        // a fresh pool per round: the term is rebuilt (untimed), the proof is timed
        let mut d = Dale::new();
        let t = d.build(&big);
        let mut apply = Duration::ZERO;
        let t0 = Instant::now();
        let (s, _) = saturate(&mut d, t, Some(&mut apply));
        let e = t0.elapsed();
        assert_eq!(s, stats);
        if r >= 3 {
            if e < best {
                best = e;
                best_apply = apply;
            }
            all.push(e);
        }
    }
    let med = median(&mut all);
    println!(
        "prove:  conjunction of {} formulas (closed: {}), {} rule applications, {} nodes, {} open goals",
        fs.len(),
        tautologies == 1,
        stats.apps,
        stats.nodes,
        stats.open
    );
    println!(
        "        best {:>9.3} ms  median {:>9.3} ms   {:>7.1} ns/application (best)",
        best.as_secs_f64() * 1e3,
        med.as_secs_f64() * 1e3,
        best.as_nanos() as f64 / stats.apps as f64
    );
    println!(
        "        of which applying (apply_rule + proof tree): {:>9.3} ms = {:>7.1} ns/application",
        best_apply.as_secs_f64() * 1e3,
        best_apply.as_nanos() as f64 / stats.apps as f64
    );
    let rest = best - best_apply;
    println!(
        "        rest (selecting the rule, matching, goal handling): {:>9.3} ms",
        rest.as_secs_f64() * 1e3
    );
}
