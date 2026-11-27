//! Matching rules against terms and sequents (the interpreter).
//!
//! Matching only reads the [`TermPool`]. A [`Bindings`] holds the instantiation of the schema
//! variables and generic slots; failed attempts are undone through a trail, so one `Bindings` can
//! be reused for all attempts.
//!
//! Semantics, following KeY where noted:
//! * a schema variable that occurs several times must match the same term; with hash-consed terms
//!   this is index equality, also modulo renaming of bound variables since de Bruijn indices are
//!   canonical;
//! * `\assumes` patterns are matched one after the other, each against *all* formulas of its
//!   side (depth-first with backtracking); one sequent formula may match several patterns (as in
//!   KeY);
//! * conditions are checked at the end.
//!
//! TODO: bucket the formulas of a semisequent by head operator; 97% of candidates fail in KeY's
//! profile. TODO: order the patterns statically (ground ones first, `\assumes` before a bare
//! schema variable `\find`).

use std::ops::ControlFlow;

use crate::ids::TermIdx;
use crate::pattern::{Cond, Find, GenPat, MetaPat, PatId, PatNode, Rule, SortReq, SvId, SvKind};
use crate::pool::{GenArg, TermPool};
use crate::sequent::{Sequent, Side};

#[derive(Clone, Copy, Debug)]
enum Undo {
    Sv(u32),
    Slot(u32),
}

/// The instantiation found by a match.
#[derive(Clone, Debug, Default)]
pub struct Bindings {
    /// For each schema variable: the matched term and the number of pattern binders above the
    /// place where it was first matched.
    svs: Vec<Option<(TermIdx, u32)>>,
    slots: Vec<Option<GenArg>>,
    trail: Vec<Undo>,
    /// The sequent formulas matched so far: the `\find` parts first, then the `\assumes`
    /// formulas, as (side, index in the semisequent).
    matched: Vec<(Side, u32)>,
}

impl Bindings {
    pub fn new(rule: &Rule) -> Self {
        let mut b = Self::default();
        b.reset(rule);
        b
    }

    /// Clears everything and sizes for `rule`.
    pub fn reset(&mut self, rule: &Rule) {
        self.svs.clear();
        self.svs.resize(rule.num_svs(), None);
        self.slots.clear();
        self.slots.resize(rule.num_slots(), None);
        self.trail.clear();
        self.matched.clear();
    }

    /// What the schema variable matched.
    pub fn get(&self, sv: SvId) -> Option<TermIdx> {
        self.svs[sv.0 as usize].map(|(t, _)| t)
    }

    /// What the schema variable matched and the number of pattern binders above the place where
    /// it matched first.
    pub fn get_at_depth(&self, sv: SvId) -> Option<(TermIdx, u32)> {
        self.svs[sv.0 as usize]
    }

    /// The sequent formulas the match used, as (side, index in the semisequent): first the formulas
    /// of a sequent `\find` (antecedent before succedent), then those of the `\assumes` patterns
    /// in order. Complete when the match callback is called.
    pub fn matched(&self) -> &[(Side, u32)] {
        &self.matched
    }

    /// The generic argument bound to slot `i`.
    pub fn slot(&self, i: u32) -> Option<GenArg> {
        self.slots[i as usize]
    }

    #[inline]
    fn mark(&self) -> usize {
        self.trail.len()
    }

    #[inline]
    fn undo(&mut self, mark: usize) {
        while self.trail.len() > mark {
            match self.trail.pop().unwrap() {
                Undo::Sv(i) => self.svs[i as usize] = None,
                Undo::Slot(i) => self.slots[i as usize] = None,
            }
        }
    }
}

/// A snapshot of the instantiation of a rule: what each schema variable and generic slot matched.
/// Kept in the proof tree for every rule application.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Instantiation {
    pub svs: Box<[Option<TermIdx>]>,
    pub slots: Box<[Option<GenArg>]>,
}

impl Bindings {
    /// Copies the current bindings out.
    pub fn instantiation(&self) -> Instantiation {
        Instantiation {
            svs: self.svs.iter().map(|x| x.map(|(t, _)| t)).collect(),
            slots: self.slots.clone().into_boxed_slice(),
        }
    }
}

/// `s == lift(t, k)` for the free variables of `t` (those at or above `cutoff` while descending
/// under binders), without building the lifted term.
fn is_lifted(pool: &TermPool, s: TermIdx, t: TermIdx, k: u32, cutoff: u32) -> bool {
    let tt = &pool[t];
    if tt.loose() <= cutoff {
        return s == t;
    }
    let st = &pool[s];
    let (to, so) = (&pool[tt.op()], &pool[st.op()]);
    if let Some(n) = to.var_index() {
        return so.var_index() == Some(n + k as u64) && st.sort() == tt.sort();
    }
    if to.decl() != so.decl() || tt.op() != st.op() {
        return false;
    }
    let bind = to.bind().map(|(b, _)| b);
    pool.subs_of(s)
        .iter()
        .zip(pool.subs_of(t))
        .enumerate()
        .all(|(i, (&sa, &ta))| {
            let c = if bind == Some(i as u32) {
                cutoff + 1
            } else {
                cutoff
            };
            is_lifted(pool, sa, ta, k, c)
        })
}

struct Ctx<'a> {
    pool: &'a TermPool,
    rule: &'a Rule,
    seq: &'a Sequent,
}

impl Ctx<'_> {
    /// Matches pattern `p` against term `t` that sits below `depth` pattern binders.
    fn m(&self, b: &mut Bindings, p: PatId, t: TermIdx, depth: u32) -> bool {
        let pool = self.pool;
        match self.rule.node(p) {
            PatNode::Ground(g) => *g == t,
            PatNode::Sv(sv) => self.m_sv(b, *sv, t, depth),
            PatNode::App {
                decl,
                gens,
                meta,
                args,
                bind_arg,
            } => {
                let od = &pool[pool.op_of(t)];
                if od.decl() != *decl {
                    return false;
                }
                for (gp, ga) in gens.iter().zip(od.generics()) {
                    match gp {
                        GenPat::Exact(x) => {
                            if x != ga {
                                return false;
                            }
                        }
                        GenPat::Slot(i) => {
                            if !bind_slot(b, *i, *ga) {
                                return false;
                            }
                        }
                    }
                }
                for (mp, mv) in meta.iter().zip(od.meta()) {
                    if let MetaPat::Exact(x) = mp
                        && x != mv
                    {
                        return false;
                    }
                }
                let subs = pool.subs_of(t);
                for (i, (&a, &s)) in args.iter().zip(subs).enumerate() {
                    let d = depth + (*bind_arg == Some(i as u32)) as u32;
                    if !self.m(b, a, s, d) {
                        return false;
                    }
                }
                true
            }
        }
    }

    fn m_sv(&self, b: &mut Bindings, sv: SvId, t: TermIdx, depth: u32) -> bool {
        let pool = self.pool;
        if let Some((bound, d0)) = b.svs[sv.0 as usize] {
            if d0 == depth {
                return bound == t;
            }
            // A different binder depth: only terms that do not mention the pattern's own binders
            // can be the same, and they differ by the shift of the outer variables.
            let (bt, tt) = (&pool[bound], &pool[t]);
            if bt.may_have_free_below(d0) || tt.may_have_free_below(depth) {
                return false;
            }
            return if depth > d0 {
                is_lifted(pool, t, bound, depth - d0, 0)
            } else {
                is_lifted(pool, bound, t, d0 - depth, 0)
            };
        }
        let decl = self.rule.sv(sv);
        let sort = pool.sort_of(t);
        let is_formula = sort == pool.formula();
        match decl.kind {
            SvKind::Formula if !is_formula => return false,
            SvKind::Term if is_formula => return false,
            _ => {}
        }
        match decl.sort {
            SortReq::Any => {}
            SortReq::Sub(s) => {
                if !pool.is_subsort(sort, s) {
                    return false;
                }
            }
            SortReq::Slot(i) => {
                if !bind_slot(b, i, GenArg::Sort(sort)) {
                    return false;
                }
            }
        }
        b.svs[sv.0 as usize] = Some((t, depth));
        b.trail.push(Undo::Sv(sv.0));
        true
    }

    fn conds_hold(&self, b: &Bindings) -> bool {
        self.rule.conds().iter().all(|c| match *c {
            Cond::NoFreeVar { sv, index } => match b.get(sv) {
                Some(t) => !self.pool[t].may_have_free(index),
                None => false,
            },
        })
    }

    /// Matches the remaining find steps against sequent formulas, then the `\assumes` patterns,
    /// then checks the conditions.
    fn steps(
        &self,
        b: &mut Bindings,
        find: &[FindStep],
        assumes: usize,
        k: &mut dyn FnMut(&Bindings) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        let (side, pat, at, rest_find, rest_assumes) = if let Some((f, r)) = find.split_first() {
            (f.side, f.pat, f.at, r, assumes)
        } else if let Some(a) = self.rule.assumes().get(assumes) {
            (a.side, a.pat, None, find, assumes + 1)
        } else {
            return if self.conds_hold(b) {
                k(b)
            } else {
                ControlFlow::Continue(())
            };
        };
        let formulas = self.seq.semisequent(side).as_slice();
        let range = match at {
            Some(i) => i..(i + 1).min(formulas.len()),
            None => 0..formulas.len(),
        };
        for i in range {
            let f = formulas[i];
            let mark = b.mark();
            b.matched.push((side, i as u32));
            let r = if self.m(b, pat, f, 0) {
                self.steps(b, rest_find, rest_assumes, k)
            } else {
                ControlFlow::Continue(())
            };
            b.matched.pop();
            b.undo(mark);
            r?;
        }
        ControlFlow::Continue(())
    }
}

/// A part of a sequent find: its pattern is matched against the formulas of `side`, or only
/// against the one at `at`.
#[derive(Clone, Copy)]
struct FindStep {
    side: Side,
    pat: PatId,
    at: Option<usize>,
}

fn bind_slot(b: &mut Bindings, i: u32, v: GenArg) -> bool {
    match b.slots[i as usize] {
        Some(x) => x == v,
        None => {
            b.slots[i as usize] = Some(v);
            b.trail.push(Undo::Slot(i));
            true
        }
    }
}

/// Finds all matches of `rule` with its `\find` at the term `t` (for a [`Find::Term`] rule),
/// extended by matches of the `\assumes` formulas in `seq`. `k` is called for each complete match
/// (patterns and conditions); returning `Break` stops the search. The bindings are restored on
/// return, so `b` can be reused.
///
/// For rules with a [`Find::Sequent`] find use [`match_sequent`].
pub fn match_term(
    pool: &TermPool,
    rule: &Rule,
    seq: &Sequent,
    t: TermIdx,
    b: &mut Bindings,
    k: &mut dyn FnMut(&Bindings) -> ControlFlow<()>,
) -> ControlFlow<()> {
    let Find::Term(p) = rule.find() else {
        return ControlFlow::Continue(());
    };
    let cx = Ctx { pool, rule, seq };
    let mark = b.mark();
    let r = if cx.m(b, p, t, 0) {
        cx.steps(b, &[], 0, k)
    } else {
        ControlFlow::Continue(())
    };
    b.undo(mark);
    r
}

/// Like [`match_term`] for rules with a sequent `\find`: the find formulas range over the
/// formulas of their side.
pub fn match_sequent(
    pool: &TermPool,
    rule: &Rule,
    seq: &Sequent,
    b: &mut Bindings,
    k: &mut dyn FnMut(&Bindings) -> ControlFlow<()>,
) -> ControlFlow<()> {
    sequent_steps(pool, rule, seq, None, b, k)
}

/// Like [`match_sequent`], with the find part on `side` matched only against the formula at
/// `index` of that side (a part on the other side still ranges over its side). This is how the
/// candidates of [`RuleIndex::sequent_candidates`](crate::RuleIndex::sequent_candidates) are
/// matched. Rules without a find part on `side` do not match.
pub fn match_sequent_at(
    pool: &TermPool,
    rule: &Rule,
    seq: &Sequent,
    side: Side,
    index: usize,
    b: &mut Bindings,
    k: &mut dyn FnMut(&Bindings) -> ControlFlow<()>,
) -> ControlFlow<()> {
    sequent_steps(pool, rule, seq, Some((side, index)), b, k)
}

fn sequent_steps(
    pool: &TermPool,
    rule: &Rule,
    seq: &Sequent,
    focus: Option<(Side, usize)>,
    b: &mut Bindings,
    k: &mut dyn FnMut(&Bindings) -> ControlFlow<()>,
) -> ControlFlow<()> {
    let Find::Sequent { ante, succ } = rule.find() else {
        return ControlFlow::Continue(());
    };
    // the antecedent part first: that is the order of `Bindings::matched`
    let mut find = [FindStep {
        side: Side::Antecedent,
        pat: PatId(0),
        at: None,
    }; 2];
    let mut n = 0;
    let mut focused = focus.is_none();
    for (side, pat) in [(Side::Antecedent, ante), (Side::Succedent, succ)] {
        let Some(pat) = pat else { continue };
        let at = match focus {
            Some((s, i)) if s == side => {
                focused = true;
                Some(i)
            }
            _ => None,
        };
        find[n] = FindStep { side, pat, at };
        n += 1;
    }
    if !focused {
        return ControlFlow::Continue(());
    }
    let cx = Ctx { pool, rule, seq };
    let mark = b.mark();
    let r = cx.steps(b, &find[..n], 0, k);
    b.undo(mark);
    r
}
