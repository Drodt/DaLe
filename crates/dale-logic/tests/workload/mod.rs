//! A propositional workload shared by the tests (`tests/prop.rs`) and the benchmark
//! (`examples/prop_bench.rs`).
//!
//! Formulas come from a deterministic generator ([`Rng`], [`random_formula`]) as a small AST ([`F`]), which is
//! built into pool terms ([`Dale::build`]) or printed in KeY syntax ([`to_key`]).
//!
//! Rules: the propositional rules of KeY's `propRule.key` with the same names:
//! `close`, `notLeft`, `notRight`, `andLeft`, `orRight`, `impRight`, `andRight`, `orLeft`,
//! `impLeft`. The order is the order in which [`saturate`] tries them.
#![allow(dead_code)]

use std::ops::ControlFlow;
use std::sync::Arc;
use std::time::{Duration, Instant};

use dale_logic::*;
use dale_util::symbol::{Interner, Symbol};

/// xorshift64*, as in Marsaglia/Vigna.
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
    /// Uniform-ish value below `n`, from the high 32 bits.
    pub fn below(&mut self, n: u64) -> u64 {
        (self.next() >> 32) % n
    }
}

pub const ATOMS: u64 = 8;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum F {
    Atom(u8),
    Not(Box<F>),
    And(Box<F>, Box<F>),
    Or(Box<F>, Box<F>),
    Imp(Box<F>, Box<F>),
}

/// A random formula of depth at most `d`. At depth 0 it is an atom; above, 1/12 atoms, 2/12 `!`,
/// 3/12 `&`, 3/12 `|`, 3/12 `->`; `!` of a negation is the negation itself. The children are generated left to right.
pub fn random_formula(r: &mut Rng, d: u32) -> F {
    if d == 0 {
        return F::Atom(r.below(ATOMS) as u8);
    }
    match r.below(12) {
        0 => F::Atom(r.below(ATOMS) as u8),
        1 | 2 => {
            // no double negation: KeY's term builder (and parser) would simplify it
            let a = random_formula(r, d - 1);
            if matches!(a, F::Not(_)) {
                a
            } else {
                F::Not(Box::new(a))
            }
        }
        3..=5 => {
            let a = random_formula(r, d - 1);
            F::And(Box::new(a), Box::new(random_formula(r, d - 1)))
        }
        6..=8 => {
            let a = random_formula(r, d - 1);
            F::Or(Box::new(a), Box::new(random_formula(r, d - 1)))
        }
        _ => {
            let a = random_formula(r, d - 1);
            F::Imp(Box::new(a), Box::new(random_formula(r, d - 1)))
        }
    }
}

/// `n` formulas of depth `d` from `seed`.
pub fn corpus(seed: u64, n: usize, d: u32) -> Vec<F> {
    let mut r = Rng(seed);
    (0..n).map(|_| random_formula(&mut r, d)).collect()
}

impl F {
    pub fn size(&self) -> usize {
        match self {
            F::Atom(_) => 1,
            F::Not(a) => 1 + a.size(),
            F::And(a, b) | F::Or(a, b) | F::Imp(a, b) => 1 + a.size() + b.size(),
        }
    }

    /// Truth under the assignment given by the bits of `v`.
    pub fn eval(&self, v: u32) -> bool {
        match self {
            F::Atom(i) => v >> i & 1 == 1,
            F::Not(a) => !a.eval(v),
            F::And(a, b) => a.eval(v) && b.eval(v),
            F::Or(a, b) => a.eval(v) || b.eval(v),
            F::Imp(a, b) => !a.eval(v) || b.eval(v),
        }
    }

    pub fn is_tautology(&self) -> bool {
        (0..1u32 << ATOMS).all(|v| self.eval(v))
    }
}

/// The balanced conjunction of the formulas: the list is split in the middle, the left half first.
/// It is the root formula of the proof phase of the benchmark (one problem, so that KeY needs to
/// load only one file).
pub fn conjoin(fs: &[F]) -> F {
    match fs {
        [] => panic!("empty conjunction"),
        [f] => f.clone(),
        _ => {
            let (a, b) = fs.split_at(fs.len() / 2);
            F::And(Box::new(conjoin(a)), Box::new(conjoin(b)))
        }
    }
}

/// KeY syntax, fully parenthesized.
pub fn to_key(f: &F) -> String {
    match f {
        F::Atom(i) => format!("p{i}"),
        F::Not(a) => format!("(!{})", to_key(a)),
        F::And(a, b) => format!("({} & {})", to_key(a), to_key(b)),
        F::Or(a, b) => format!("({} | {})", to_key(a), to_key(b)),
        F::Imp(a, b) => format!("({} -> {})", to_key(a), to_key(b)),
    }
}

pub const RULES: [&str; 9] = [
    "close", "notLeft", "notRight", "andLeft", "orRight", "impRight", "andRight", "orLeft",
    "impLeft",
];

/// Which side of the sequent the find of each rule is on.
pub fn rule_side(rule: usize) -> Side {
    match RULES[rule] {
        "notLeft" | "andLeft" | "orLeft" | "impLeft" => Side::Antecedent,
        _ => Side::Succedent,
    }
}

pub struct Dale {
    pub pool: TermPool,
    pub rules: Vec<Rule>,
    /// The index of all rules (`RuleId` = position in `rules`).
    pub index: RuleIndex,
    /// The index of the eight junctor rules: its rule `i` is `rules[i + 1]`.
    pub junctor_index: RuleIndex,
    atoms: Vec<OpDeclId>,
    not: OpDeclId,
    and: OpDeclId,
    or: OpDeclId,
    imp: OpDeclId,
}

impl Dale {
    pub fn new() -> Self {
        let mut n = Interner::<Symbol>::default();
        let mut b = SignatureBuilder::new();
        let mut d = SortDecl::new(n.intern("Any"));
        d.kind = SortKind::Top;
        b.add_sort(d).unwrap();
        let mut d = SortDecl::new(n.intern("Formula"));
        d.kind = SortKind::Formula;
        let form = b.add_sort(d).unwrap();
        let f = SortPat::simple(form);
        let mut op = |name: &str, arity: usize| {
            b.add_op(OpDecl::new(
                n.intern(name),
                vec![f.clone(); arity],
                f.clone(),
            ))
            .unwrap()
        };
        let not = op("not", 1);
        let and = op("and", 2);
        let or = op("or", 2);
        let imp = op("imp", 2);
        let atoms: Vec<_> = (0..ATOMS).map(|i| op(&format!("p{i}"), 0)).collect();
        let sig = Arc::new(b.finish().unwrap());
        let pool = TermPool::new(sig.clone());
        let mut dale = Dale {
            pool,
            rules: Vec::new(),
            index: RuleIndex::default(),
            junctor_index: RuleIndex::default(),
            atoms,
            not,
            and,
            or,
            imp,
        };
        let mut rules = Vec::new();
        for name in RULES {
            rules.push(dale.make_rule(&sig, &mut n, name));
        }
        dale.index = RuleIndex::build(&dale.pool, &rules);
        dale.junctor_index = RuleIndex::build(&dale.pool, &rules[1..]);
        dale.rules = rules;
        dale
    }

    fn make_rule(&self, sig: &Arc<Signature>, n: &mut Interner<Symbol>, name: &str) -> Rule {
        let mut r = RuleBuilder::new(sig.clone(), n.intern(name));
        let mut sv =
            |r: &mut RuleBuilder, s: &str| r.declare_sv(n.intern(s), SvKind::Formula, SortReq::Any);
        if name == "close" {
            let x = sv(&mut r, "b");
            let px = r.sv(x).unwrap();
            r.find_sequent(None, Some(px));
            r.assume(Side::Antecedent, px);
            return r.finish().unwrap();
        }
        let (op, args) = match name {
            "notLeft" | "notRight" => (self.not, 1),
            "andLeft" | "andRight" => (self.and, 2),
            "orLeft" | "orRight" => (self.or, 2),
            _ => (self.imp, 2),
        };
        let vars: Vec<_> = ["b", "c"][..args].iter().map(|s| sv(&mut r, s)).collect();
        let pats: Vec<_> = vars.iter().map(|v| r.sv(*v).unwrap()).collect();
        let pat = r.app(op, &[], &[], &pats).unwrap();
        let side = rule_side(RULES.iter().position(|x| *x == name).unwrap());
        match side {
            Side::Antecedent => r.find_sequent(Some(pat), None),
            Side::Succedent => r.find_sequent(None, Some(pat)),
        }
        // (formulas replacing the find, formulas added to the other side), per goal
        let goals: &[(&[usize], &[usize])] = match name {
            "notLeft" | "notRight" => &[(&[], &[0])],
            "andLeft" | "orRight" => &[(&[0, 1], &[])],
            "andRight" | "orLeft" => &[(&[0], &[]), (&[1], &[])],
            "impLeft" => &[(&[], &[0]), (&[1], &[])],
            "impRight" => &[(&[1], &[0])],
            _ => unreachable!(),
        };
        for (repl, other) in goals {
            let t = |r: &mut RuleBuilder, i: &usize| r.tmpl_sv(vars[*i]).unwrap();
            let repl: Vec<_> = repl.iter().map(|i| t(&mut r, i)).collect();
            let other: Vec<_> = other.iter().map(|i| t(&mut r, i)).collect();
            let mut g = match side {
                Side::Antecedent => GoalTemplate::new(Replace::Sequent {
                    ante: repl,
                    succ: vec![],
                }),
                Side::Succedent => GoalTemplate::new(Replace::Sequent {
                    ante: vec![],
                    succ: repl,
                }),
            };
            match side {
                Side::Antecedent => g.add_succ = other,
                Side::Succedent => g.add_ante = other,
            }
            r.goal(g);
        }
        r.finish().unwrap()
    }

    pub fn build(&mut self, f: &F) -> TermIdx {
        match f {
            F::Atom(i) => self
                .pool
                .mk_inferred(self.atoms[*i as usize], &[], &[])
                .unwrap(),
            F::Not(a) => {
                let a = self.build(a);
                self.pool.mk_inferred(self.not, &[], &[a]).unwrap()
            }
            F::And(a, b) | F::Or(a, b) | F::Imp(a, b) => {
                let op = match f {
                    F::And(..) => self.and,
                    F::Or(..) => self.or,
                    _ => self.imp,
                };
                let a = self.build(a);
                let b = self.build(b);
                self.pool.mk_inferred(op, &[], &[a, b]).unwrap()
            }
        }
    }

    /// `f ==>` and `==> f`.
    pub fn one_sided(&self, f: TermIdx, side: Side) -> Sequent {
        Sequent::new().add(&self.pool, side, f).unwrap().sequent
    }
}

/// Phase 1, matching: tries each of the eight junctor rules on every formula, on the side the
/// rule's find is on. Returns the number of matches. `seqs[i]` is `(f_i ==>, ==> f_i)`.
pub fn match_all(d: &Dale, seqs: &[(Sequent, Sequent)]) -> u64 {
    let mut bs: Vec<Bindings> = d.rules.iter().map(Bindings::new).collect();
    let mut n = 0;
    for (l, r) in seqs {
        for (i, rule) in d.rules.iter().enumerate().skip(1) {
            let seq = if rule_side(i) == Side::Antecedent {
                l
            } else {
                r
            };
            let _ = match_sequent(&d.pool, rule, seq, &mut bs[i], &mut |_| {
                n += 1;
                ControlFlow::Continue(())
            });
        }
    }
    n
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Stats {
    /// Rule applications (including closing ones).
    pub apps: u64,
    /// Goals closed by `close`.
    pub closed: u64,
    /// Goals no rule applies to.
    pub open: u64,
    /// Proof tree nodes.
    pub nodes: u64,
}

impl std::ops::AddAssign for Stats {
    fn add_assign(&mut self, o: Stats) {
        self.apps += o.apps;
        self.closed += o.closed;
        self.open += o.open;
        self.nodes += o.nodes;
    }
}

/// Phase 2, rule application as part of a naive proof search: starting from `==> f`, repeatedly
/// takes a goal (depth first, left to right), applies the first applicable rule in the order of
/// [`RULES`] at the first formula it matches, until no rule applies. `apply_time` accumulates the
/// time spent in `apply_rule` and `ProofTree::apply`.
pub fn saturate(d: &mut Dale, f: TermIdx, apply_time: Option<&mut Duration>) -> (Stats, bool) {
    let root = d.one_sided(f, Side::Succedent);
    let mut tree = ProofTree::new(root);
    let mut bs: Vec<Bindings> = d.rules.iter().map(Bindings::new).collect();
    let mut stack = vec![tree.root()];
    let mut st = Stats::default();
    let mut spent = Duration::ZERO;
    let timed = apply_time.is_some();
    while let Some(g) = stack.pop() {
        let seq = tree.sequent(g).clone();
        let mut hit = None;
        for (i, rule) in d.rules.iter().enumerate() {
            let _ = match_sequent(&d.pool, rule, &seq, &mut bs[i], &mut |m| {
                hit = Some((i, m.clone()));
                ControlFlow::Break(())
            });
            if hit.is_some() {
                break;
            }
        }
        let Some((i, m)) = hit else {
            st.open += 1;
            continue;
        };
        let t0 = timed.then(Instant::now);
        let applied = apply_rule(
            &mut d.pool,
            &d.rules[i],
            i as RuleId,
            &seq,
            None,
            &m,
            SortCheck::Skip,
        )
        .unwrap();
        let closes = applied.branches.is_empty();
        let step = tree
            .apply(g, applied.app, ViewId::ZERO, applied.branches)
            .unwrap();
        if let Some(t0) = t0 {
            spent += t0.elapsed();
        }
        st.apps += 1;
        if closes {
            st.closed += 1;
        } else {
            let kids: Vec<NodeId> = tree.step(step).children().into_iter().collect();
            stack.extend(kids.into_iter().rev());
        }
    }
    st.nodes = tree.num_nodes() as u64;
    let closed = tree.is_proof_closed();
    if let Some(t) = apply_time {
        *t += spent;
    }
    (st, closed)
}

/// Phase 1 through the index: for every formula and both sides, looks up the candidate junctor
/// rules ([`Dale::junctor_index`]) and matches them at that formula. Returns the number of
/// matches, which is that of [`match_all`].
pub fn match_all_indexed(d: &Dale, seqs: &[(Sequent, Sequent)]) -> u64 {
    let mut bs: Vec<Bindings> = d.rules.iter().map(Bindings::new).collect();
    let mut out = Vec::new();
    let mut n = 0;
    for (l, r) in seqs {
        for (side, seq) in [(Side::Antecedent, l), (Side::Succedent, r)] {
            let f = seq.semisequent(side).as_slice()[0];
            d.junctor_index
                .sequent_candidates(&d.pool, side, f, &mut out);
            for &c in &out {
                let i = c as usize + 1;
                let _ =
                    match_sequent_at(&d.pool, &d.rules[i], seq, side, 0, &mut bs[i], &mut |_| {
                        n += 1;
                        ControlFlow::Continue(())
                    });
            }
        }
    }
    n
}

/// Phase 2 through the index ([`Dale::index`]): the candidates of every formula of the goal are
/// looked up, and the rule applied is the same as in [`saturate`], the smallest
/// `(rule, formula index)` that matches, so the proof is the same. `lazy`: the candidates are
/// sorted and matched in that order until one matches; otherwise every candidate is matched and
/// the smallest match is taken.
pub fn saturate_indexed(
    d: &mut Dale,
    f: TermIdx,
    lazy: bool,
    apply_time: Option<&mut Duration>,
) -> (Stats, bool) {
    let root = d.one_sided(f, Side::Succedent);
    let mut tree = ProofTree::new(root);
    let mut bs: Vec<Bindings> = d.rules.iter().map(Bindings::new).collect();
    let mut stack = vec![tree.root()];
    let mut st = Stats::default();
    let mut spent = Duration::ZERO;
    let timed = apply_time.is_some();
    let mut out = Vec::new();
    let mut cands: Vec<(RuleId, u32, Side)> = Vec::new();
    while let Some(g) = stack.pop() {
        let seq = tree.sequent(g).clone();
        cands.clear();
        for side in [Side::Antecedent, Side::Succedent] {
            for (i, &t) in seq.semisequent(side).as_slice().iter().enumerate() {
                d.index.sequent_candidates(&d.pool, side, t, &mut out);
                cands.extend(out.iter().map(|&r| (r, i as u32, side)));
            }
        }
        if lazy {
            cands.sort_unstable_by_key(|&(r, i, _)| (r, i));
        }
        let mut hit: Option<((RuleId, u32), Bindings)> = None;
        for &(r, i, side) in &cands {
            if hit.as_ref().is_some_and(|(k, _)| *k <= (r, i)) {
                if lazy {
                    break;
                }
                continue;
            }
            let ri = r as usize;
            let _ = match_sequent_at(
                &d.pool,
                &d.rules[ri],
                &seq,
                side,
                i as usize,
                &mut bs[ri],
                &mut |m| {
                    hit = Some(((r, i), m.clone()));
                    ControlFlow::Break(())
                },
            );
        }
        let Some(((r, _), m)) = hit else {
            st.open += 1;
            continue;
        };
        let t0 = timed.then(Instant::now);
        let applied = apply_rule(
            &mut d.pool,
            &d.rules[r as usize],
            r,
            &seq,
            None,
            &m,
            SortCheck::Skip,
        )
        .unwrap();
        let closes = applied.branches.is_empty();
        let step = tree
            .apply(g, applied.app, ViewId::ZERO, applied.branches)
            .unwrap();
        if let Some(t0) = t0 {
            spent += t0.elapsed();
        }
        st.apps += 1;
        if closes {
            st.closed += 1;
        } else {
            let kids: Vec<NodeId> = tree.step(step).children().into_iter().collect();
            stack.extend(kids.into_iter().rev());
        }
    }
    st.nodes = tree.num_nodes() as u64;
    let closed = tree.is_proof_closed();
    if let Some(t) = apply_time {
        *t += spent;
    }
    (st, closed)
}
