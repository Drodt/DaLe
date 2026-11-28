//! Shared by the tests: loading the propositional part of `RustyDL.theokey` with a few atoms,
//! and a naive prover that picks rules by a fixed ruleset priority (a stand-in for the cost
//! interpreter, which does not exist yet).
#![allow(dead_code)]

use std::ops::ControlFlow;
use std::path::PathBuf;

use dale_logic::*;
use dale_theory::Theory;

pub const ATOMS: usize = 4;

/// `RustyDL.theokey`, a theory `Atoms` with the predicates `p0`..`p3`, and the problem
/// `\\problem { .. <seq> }` that uses both.
pub fn prop_problem(seq: &str) -> Theory {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../dale-syn/examples");
    let atoms: Vec<String> = (0..ATOMS).map(|i| format!("p{i};")).collect();
    let text = format!(
        "\\module RustyDL;\n\\theory Atoms {{\n  \\use RustyDL::Prop::*;\n  \\predicates {{ {} }}\n}}\n\\problem {{\n  \\use RustyDL::Prop::*;\n  \\use Atoms::*;\n  {seq}\n}}\n",
        atoms.join(" ")
    );
    Theory::load_text(dir.join("PropTest.theokey"), text).unwrap_or_else(|e| panic!("{e}"))
}

/// [`prop_problem`] and its sequent.
pub fn prop_goal(seq: &str) -> (Theory, Sequent) {
    let th = prop_problem(seq);
    let s = th.problem.clone().expect("a problem");
    (th, s)
}

/// Ruleset priorities of the naive strategy; rules in no listed ruleset are never applied.
pub const PRIORITIES: [(&str, u32); 4] =
    [("closure", 0), ("concrete", 1), ("alpha", 2), ("beta", 3)];

pub struct Outcome {
    pub closed: bool,
    pub steps: usize,
    pub open: usize,
    pub tree: ProofTree,
}

fn priority(th: &Theory, rule: RuleId) -> Option<u32> {
    th.info[rule as usize]
        .rule_sets
        .iter()
        .filter_map(|s| {
            PRIORITIES
                .iter()
                .find(|(n, _)| *n == th.name(*s))
                .map(|p| p.1)
        })
        .min()
}

struct Hit {
    prio: u32,
    rule: RuleId,
    pos: Option<PosInOccurrence>,
    b: Bindings,
}

fn consider(best: &Option<Hit>, prio: u32) -> bool {
    best.as_ref().is_none_or(|h| prio < h.prio)
}

/// Depth-first: on every goal, applies the applicable rule of the best priority (first found
/// among equals), until no goal has an applicable rule or `max_steps` rules were applied.
pub fn prove(th: &mut Theory, root: Sequent, max_steps: usize) -> Outcome {
    let prios: Vec<Option<u32>> = (0..th.rules.len() as RuleId)
        .map(|r| priority(th, r))
        .collect();
    let mut bs: Vec<Bindings> = th.rules.iter().map(Bindings::new).collect();
    let mut tree = ProofTree::new(root);
    let mut stack = vec![tree.root()];
    let (mut steps, mut open) = (0, 0);
    let mut cands = Vec::new();
    while let Some(g) = stack.pop() {
        if steps >= max_steps {
            open += 1 + stack.len();
            break;
        }
        let seq = tree.sequent(g).clone();
        let mut best: Option<Hit> = None;
        for side in [Side::Antecedent, Side::Succedent] {
            for (i, &f) in seq.semisequent(side).as_slice().iter().enumerate() {
                // sequent finds at the formula
                th.index.sequent_candidates(&th.pool, side, f, &mut cands);
                for &r in &cands {
                    let Some(p) = prios[r as usize] else { continue };
                    if !consider(&best, p) {
                        continue;
                    }
                    let _ = match_sequent_at(
                        &th.pool,
                        &th.rules[r as usize],
                        &seq,
                        side,
                        i,
                        &mut bs[r as usize],
                        &mut |m| {
                            best = Some(Hit {
                                prio: p,
                                rule: r,
                                pos: None,
                                b: m.clone(),
                            });
                            ControlFlow::Break(())
                        },
                    );
                }
                // term finds at every subterm
                let mut todo = vec![(f, PosInTerm::top())];
                while let Some((t, path)) = todo.pop() {
                    th.index.candidates(&th.pool, t, &mut cands);
                    for &r in &cands {
                        let Some(p) = prios[r as usize] else { continue };
                        if !consider(&best, p) {
                            continue;
                        }
                        let pos = PosInOccurrence::new(side, i, path.clone());
                        let _ = match_term(
                            &th.pool,
                            &th.rules[r as usize],
                            &seq,
                            t,
                            &mut bs[r as usize],
                            &mut |m| {
                                best = Some(Hit {
                                    prio: p,
                                    rule: r,
                                    pos: Some(pos.clone()),
                                    b: m.clone(),
                                });
                                ControlFlow::Break(())
                            },
                        );
                    }
                    for (k, &s) in th.pool.subs_of(t).iter().enumerate() {
                        todo.push((s, path.down(k as u16)));
                    }
                }
            }
        }
        let Some(hit) = best else {
            open += 1;
            continue;
        };
        let rule = &th.rules[hit.rule as usize];
        let applied = apply_rule(
            &mut th.pool,
            rule,
            hit.rule,
            &seq,
            hit.pos.as_ref(),
            &hit.b,
            SortCheck::Enforce,
        )
        .unwrap_or_else(|e| panic!("{}: {e:?}", th.name(rule.name)));
        steps += 1;
        let step = tree
            .apply(g, applied.app, ViewId::ZERO, applied.branches)
            .unwrap();
        let kids: Vec<NodeId> = tree.step(step).children().into_iter().collect();
        stack.extend(kids.into_iter().rev());
    }
    Outcome {
        closed: tree.is_proof_closed(),
        steps,
        open,
        tree,
    }
}

// ---------------------------------------------------------------- random formulas

/// xorshift64*.
pub struct Rng(pub u64);

impl Rng {
    pub fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }
    pub fn below(&mut self, n: u64) -> u64 {
        (self.next() >> 32) % n
    }
}

#[derive(Clone, Debug)]
pub enum F {
    Atom(usize),
    True,
    False,
    Not(Box<F>),
    Bin(&'static str, Box<F>, Box<F>),
}

pub fn random_formula(r: &mut Rng, depth: u32) -> F {
    if depth == 0 {
        return match r.below(10) {
            0 => F::True,
            1 => F::False,
            _ => F::Atom(r.below(ATOMS as u64) as usize),
        };
    }
    match r.below(12) {
        0 => random_formula(r, 0),
        1 | 2 => F::Not(Box::new(random_formula(r, depth - 1))),
        k => {
            let op = ["and", "or", "imp", "equiv"][(k as usize - 3) % 4];
            F::Bin(
                op,
                Box::new(random_formula(r, depth - 1)),
                Box::new(random_formula(r, depth - 1)),
            )
        }
    }
}

impl F {
    /// In the syntax of problem terms.
    pub fn text(&self) -> String {
        match self {
            F::Atom(i) => format!("p{i}"),
            F::True => "true".into(),
            F::False => "false".into(),
            F::Not(a) => format!("not({})", a.text()),
            F::Bin(op, a, b) => format!("{op}({}, {})", a.text(), b.text()),
        }
    }

    pub fn eval(&self, v: u32) -> bool {
        match self {
            F::Atom(i) => v >> i & 1 == 1,
            F::True => true,
            F::False => false,
            F::Not(a) => !a.eval(v),
            F::Bin(op, a, b) => {
                let (a, b) = (a.eval(v), b.eval(v));
                match *op {
                    "and" => a && b,
                    "or" => a || b,
                    "imp" => !a || b,
                    _ => a == b,
                }
            }
        }
    }

    pub fn is_tautology(&self) -> bool {
        (0..1u32 << ATOMS).all(|v| self.eval(v))
    }
}
