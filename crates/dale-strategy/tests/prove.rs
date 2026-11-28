//! Proofs with the rules of `RustyDL.theokey`, loaded through `dale-theory`, of problems given
//! as `\problem`s, searched by the best-first strategy.

mod common;

use common::*;
use dale_strategy::*;

/// Stand-in ruleset costs: the values of `Cost::CLOSE`, `Cost::REWRITE` and `Cost::DECOMPOSE`
/// that `RustyDL.theokey` uses, and a finite cost for `beta` (the theory says `top`, so that
/// branching rules are interactive only; without them few tautologies close).
const TABLE: [(&str, Cost); 4] = [
    ("closure", Cost(-15000)),
    ("concrete", Cost(-11000)),
    ("alpha", Cost(-7000)),
    ("beta", Cost(0)),
];

fn search(seq: &str, cfg: &Config) -> Outcome {
    let (mut th, s) = prop_goal(seq);
    let costs = RulesetCosts::from_table(&th, &TABLE);
    prove_theory(&mut th, &costs, s, cfg)
}

fn proves(seq: &str, max_steps: usize) -> Outcome {
    search(
        seq,
        &Config {
            max_steps,
            ..Config::default()
        },
    )
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
        assert!(out.closed, "{f}: {} open goals", out.open());
        assert!(out.stuck.is_empty());
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
        assert!(!out.stuck.is_empty() && !out.out_of_steps);
        assert_eq!(out.open(), out.stuck.len());
    }
}

#[test]
fn sequent_problems() {
    assert!(proves("p0, imp(p0, p1) ==> p1", 1_000).closed);
    assert!(proves("false ==>", 1_000).closed);
    assert!(!proves("p0 ==> p1", 1_000).closed);
}

/// The cheapest candidate is applied: `close` before any decomposition.
#[test]
fn cheapest_first() {
    let out = proves("p0, and(p1, p2) ==> or(p2, p3), p0", 100);
    assert!(out.closed);
    assert_eq!(out.steps, 1);
}

/// Rules in no ruleset of the table are never applied.
#[test]
fn only_automatic_rules() {
    let (mut th, s) = prop_goal("==> and(p0, not(p0))");
    let costs = RulesetCosts::from_table(&th, &[("alpha", Cost(0))]);
    let out = prove_theory(&mut th, &costs, s, &Config::default());
    // `andRight` is `beta`: nothing applies
    assert_eq!(out.steps, 0);
    assert_eq!(out.stuck, [out.tree.root()]);
    // `top` costs are not automatic either
    let costs = RulesetCosts::from_table(&th, &[("beta", Cost::TOP)]);
    assert_eq!(costs.rule_cost(th.rule_named("andRight").unwrap()), None);
}

#[test]
fn stop_on_failure_and_step_limit() {
    // two branches; the first (`p0`) is stuck
    let cfg = Config {
        stop_on_failure: true,
        ..Config::default()
    };
    let out = search("==> and(p0, imp(p1, p1))", &cfg);
    assert!(!out.closed && out.stuck.len() == 1 && out.open() == 2);
    let out = search("==> and(p0, imp(p1, p1))", &Config::default());
    assert!(out.stuck.len() == 1 && out.open() == 1);
    let out = proves("==> imp(imp(imp(p0, p1), p0), p0)", 2);
    assert!(out.out_of_steps && out.steps == 2 && !out.closed);
}

/// The verdict of the prover equals the truth table on random formulas, with and without the
/// age term, and with branching preferred over decomposition.
#[test]
fn random_formulas_agree_with_truth_tables() {
    let beta_first = [
        ("closure", Cost(-15000)),
        ("concrete", Cost(-11000)),
        ("alpha", Cost(0)),
        ("beta", Cost(-7000)),
    ];
    for (age_weight, table) in [(1, &TABLE), (0, &TABLE), (1, &beta_first)] {
        let mut r = Rng(0x9E37_79B9_7F4A_7C15);
        let (mut taut, mut non) = (0, 0);
        for _ in 0..200 {
            let f = random_formula(&mut r, 4);
            let (mut th, s) = prop_goal(&format!("==> {}", f.text()));
            let costs = RulesetCosts::from_table(&th, table);
            let cfg = Config {
                age_weight,
                ..Config::default()
            };
            let out = prove_theory(&mut th, &costs, s, &cfg);
            assert!(!out.out_of_steps, "did not terminate: {}", f.text());
            assert_eq!(out.closed, f.is_tautology(), "{}", f.text());
            if out.closed { taut += 1 } else { non += 1 }
        }
        // the corpus has both kinds
        assert!(taut > 10 && non > 10, "{taut} tautologies, {non} others");
    }
}

#[test]
fn example_problem_file() {
    let mut th = dale_theory::Theory::load(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../dale-syn/examples/PropProblem.theokey"
    ))
    .unwrap_or_else(|e| panic!("{e}"));
    let seq = th.problem.clone().unwrap();
    let costs = RulesetCosts::from_table(&th, &TABLE);
    assert!(prove_theory(&mut th, &costs, seq, &Config::default()).closed);
}
