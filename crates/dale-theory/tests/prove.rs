//! Proofs with the rules of `RustyDL.theokey`, loaded through `dale-theory`, of problems given
//! as `\\problem`s.

mod common;

use common::*;

fn proves(seq: &str, max_steps: usize) -> Outcome {
    let (mut th, s) = prop_goal(seq);
    prove(&mut th, s, max_steps)
}

#[test]
fn simple_tautologies_close() {
    for f in [
        "imp(p0, p0)",
        "or(p0, not(p0))",
        "imp(and(p0, p1), or(p1, p2))",
        "equiv(and(p0, p1), and(p1, p0))",
        "imp(imp(imp(p0, p1), p0), p0)",
        "equiv(not(or(p0, p1)), and(not(p0), not(p1)))",
        "or(true, p3)",
    ] {
        let out = proves(&format!("==> {f}"), 10_000);
        assert!(out.closed, "{f}: {} open goals", out.open);
    }
}

#[test]
fn non_tautologies_stay_open() {
    for f in [
        "p0",
        "imp(p0, p1)",
        "or(p0, p1)",
        "equiv(p0, not(p0))",
        "false",
    ] {
        let out = proves(&format!("==> {f}"), 10_000);
        assert!(!out.closed, "{f} closed");
        assert!(out.open > 0);
    }
}

#[test]
fn sequent_problems() {
    assert!(proves("p0, imp(p0, p1) ==> p1", 1_000).closed);
    assert!(proves("false ==>", 1_000).closed);
    assert!(!proves("p0 ==> p1", 1_000).closed);
}

/// The verdict of the prover equals the truth table on random formulas.
#[test]
fn random_formulas_agree_with_truth_tables() {
    let mut r = Rng(0x9E37_79B9_7F4A_7C15);
    let (mut taut, mut non) = (0, 0);
    for _ in 0..200 {
        let f = random_formula(&mut r, 4);
        let out = proves(&format!("==> {}", f.text()), 100_000);
        assert!(out.steps < 100_000, "did not terminate: {}", f.text());
        assert_eq!(out.closed, f.is_tautology(), "{}", f.text());
        if out.closed { taut += 1 } else { non += 1 }
    }
    // the corpus has both kinds
    assert!(taut > 10 && non > 10, "{taut} tautologies, {non} others");
}

#[test]
fn example_problem_file() {
    let mut th = dale_theory::Theory::load(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../dale-syn/examples/PropProblem.theokey"
    ))
    .unwrap_or_else(|e| panic!("{e}"));
    let seq = th.problem.clone().unwrap();
    assert!(prove(&mut th, seq, 1_000).closed);
}
