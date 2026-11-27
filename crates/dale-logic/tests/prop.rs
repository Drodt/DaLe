//! Tests of the propositional workload: matching counts against the formula structure, and the
//! naive proof search against truth tables.

mod workload;

use dale_logic::*;
use workload::*;

fn setup(seed: u64, n: usize, depth: u32) -> (Dale, Vec<F>, Vec<TermIdx>) {
    let mut d = Dale::new();
    let fs = corpus(seed, n, depth);
    let ts = fs.iter().map(|f| d.build(f)).collect();
    (d, fs, ts)
}

#[test]
fn generator_is_deterministic() {
    // pinned values
    let mut r = Rng(0x9E37_79B9_7F4A_7C15);
    let v: Vec<u64> = (0..3).map(|_| r.next()).collect();
    assert_eq!(
        v,
        [0xd83b3e29a21487a, 0x54c44c79f1fe9d67, 0xa845f342007a0e78]
    );
    let c = corpus(1, 3, 3);
    assert_eq!(
        c.iter().map(to_key).collect::<Vec<_>>(),
        [
            "(((p0 | p0) & (p0 -> p0)) | (p1 -> (p1 -> p0)))",
            "((p6 & (!p5)) | ((!p3) -> (p3 & p3)))",
            "(p5 | ((p0 -> p6) | (p0 -> p1)))",
        ]
    );
}

#[test]
fn building_twice_gives_the_same_term() {
    let (mut d, fs, ts) = setup(7, 50, 5);
    for (f, t) in fs.iter().zip(&ts) {
        assert_eq!(d.build(f), *t);
    }
}

/// The number of matches of the eight junctor rules at a formula is 1 for the rule whose head is
/// the formula's head on the rule's side, else 0.
#[test]
fn matching_counts() {
    let (d, fs, ts) = setup(11, 300, 5);
    let seqs: Vec<_> = ts
        .iter()
        .map(|t| {
            (
                d.one_sided(*t, Side::Antecedent),
                d.one_sided(*t, Side::Succedent),
            )
        })
        .collect();
    // every formula matches exactly the rules of its head operator: one on each side, except atoms
    let expect: u64 = fs
        .iter()
        .map(|f| if matches!(f, F::Atom(_)) { 0 } else { 2 })
        .sum();
    assert_eq!(match_all(&d, &seqs), expect);
    assert_eq!(match_all_indexed(&d, &seqs), expect);
}

/// The index finds exactly the rules of the head operator, and `close` on the succedent.
#[test]
fn index_candidates_follow_the_head() {
    let (d, fs, ts) = setup(13, 200, 4);
    let mut out = Vec::new();
    for (f, t) in fs.iter().zip(&ts) {
        for side in [Side::Antecedent, Side::Succedent] {
            d.index.sequent_candidates(&d.pool, side, *t, &mut out);
            out.sort();
            let mut expect: Vec<RuleId> = (0..RULES.len() as RuleId)
                .filter(|&r| rule_side(r as usize) == side)
                .filter(|&r| {
                    let name = RULES[r as usize];
                    name == "close"
                        || matches!(
                            (f, &name[..3]),
                            (F::Not(..), "not")
                                | (F::And(..), "and")
                                | (F::Or(..), "orL" | "orR")
                                | (F::Imp(..), "imp")
                        )
                })
                .collect();
            expect.sort();
            assert_eq!(out, expect, "{} on {side:?}", to_key(f));
        }
    }
}

/// The indexed proof search builds the same proof as the plain one.
#[test]
fn indexed_saturation_is_the_same() {
    let (mut d, _, ts) = setup(5, 200, 5);
    for t in &ts {
        let plain = saturate(&mut d, *t, None);
        assert_eq!(saturate_indexed(&mut d, *t, false, None), plain);
        assert_eq!(saturate_indexed(&mut d, *t, true, None), plain);
    }
}

#[test]
fn close_needs_both_sides() {
    let (mut d, _, _) = setup(1, 0, 1);
    let a = d.build(&F::Atom(3));
    let b = d.build(&F::Atom(4));
    let seq = d.one_sided(a, Side::Antecedent);
    let seq = seq.add(&d.pool, Side::Succedent, b).unwrap().sequent;
    let mut bs = Bindings::new(&d.rules[0]);
    let mut n = 0;
    let _ = match_sequent(&d.pool, &d.rules[0], &seq, &mut bs, &mut |_| {
        n += 1;
        std::ops::ControlFlow::Continue(())
    });
    assert_eq!(n, 0);
    let seq = seq.add(&d.pool, Side::Succedent, a).unwrap().sequent;
    let _ = match_sequent(&d.pool, &d.rules[0], &seq, &mut bs, &mut |_| {
        n += 1;
        std::ops::ControlFlow::Continue(())
    });
    assert_eq!(n, 1);
}

/// The proof search closes exactly the tautologies. This tests all nine rules and the
/// application machinery against an independent semantics.
#[test]
fn saturation_decides_tautologies() {
    let (mut d, fs, ts) = setup(5, 400, 5);
    let mut valid = 0;
    let mut closed_all = Stats::default();
    for (f, t) in fs.iter().zip(&ts) {
        let (st, closed) = saturate(&mut d, *t, None);
        assert_eq!(closed, f.is_tautology(), "{}", to_key(f));
        if closed {
            valid += 1;
            assert_eq!(st.open, 0);
        } else {
            assert!(st.open > 0);
        }
        closed_all += st;
    }
    assert!(valid > 0, "the corpus should contain tautologies");
    assert!(valid < fs.len());
    assert!(closed_all.apps > fs.len() as u64);
}

#[test]
fn known_tautologies_close() {
    let mut d = Dale::new();
    let p = F::Atom(0);
    let q = F::Atom(1);
    let b = |f: F| Box::new(f);
    let cases = [
        // p -> p
        F::Imp(b(p.clone()), b(p.clone())),
        // (p & q) -> (q & p)
        F::Imp(
            b(F::And(b(p.clone()), b(q.clone()))),
            b(F::And(b(q.clone()), b(p.clone()))),
        ),
        // !(p | q) -> (!p & !q)
        F::Imp(
            b(F::Not(b(F::Or(b(p.clone()), b(q.clone()))))),
            b(F::And(b(F::Not(b(p.clone()))), b(F::Not(b(q.clone()))))),
        ),
        // ((p -> q) & p) -> q
        F::Imp(
            b(F::And(b(F::Imp(b(p.clone()), b(q.clone()))), b(p.clone()))),
            b(q.clone()),
        ),
    ];
    for f in cases {
        let t = d.build(&f);
        assert!(saturate(&mut d, t, None).1, "{}", to_key(&f));
    }
    let t = d.build(&F::Imp(b(p.clone()), b(q.clone())));
    assert!(!saturate(&mut d, t, None).1);
}
