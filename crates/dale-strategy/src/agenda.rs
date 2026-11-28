//! The agenda of a goal: its candidates and the choice among them.

use std::cmp::Ordering;
use std::ops::ControlFlow;

use dale_logic::{
    Bindings, Find, PosInOccurrence, PosInTerm, Rule, RuleId, RuleIndex, Sequent, Side, TermIdx,
    TermPool, match_no_find, match_sequent_at, match_term,
};

use crate::cost::{Cost, CostCtx, CostModel};

/// A match of an automatic rule in a goal, with its cost.
#[derive(Clone, Debug)]
pub struct Candidate {
    pub rule: RuleId,
    /// Where a term `\find` matched; `None` for the other rules (see [`Bindings::matched`]).
    pub find: Option<PosInOccurrence>,
    pub bindings: Bindings,
    /// The cost from the cost model.
    pub cost: Cost,
    /// The depth of the node where the newest formula the candidate uses entered the sequent.
    pub birth: u32,
    /// `cost` plus the age term: what candidates are compared by first.
    pub key: Cost,
}

impl Candidate {
    /// The order of [`Agenda::best`]: `key`, then the tie-break of the crate documentation.
    pub fn cmp_key(&self, o: &Candidate) -> Ordering {
        self.key
            .cmp(&o.key)
            .then(self.rule.cmp(&o.rule))
            .then_with(|| cmp_pos(self.find.as_ref(), o.find.as_ref()))
            .then_with(|| {
                let side = |s: Side| s == Side::Succedent;
                let m = |c: &Candidate| {
                    c.bindings
                        .matched()
                        .iter()
                        .map(|&(s, i)| (side(s), i))
                        .collect::<Vec<_>>()
                };
                m(self).cmp(&m(o))
            })
    }
}

fn cmp_pos(a: Option<&PosInOccurrence>, b: Option<&PosInOccurrence>) -> Ordering {
    let k = |p: &PosInOccurrence| (p.side == Side::Succedent, p.index);
    match (a, b) {
        (Some(a), Some(b)) => k(a)
            .cmp(&k(b))
            .then_with(|| a.term_pos.as_slice().cmp(b.term_pos.as_slice())),
        (a, b) => a.is_some().cmp(&b.is_some()),
    }
}

/// What building an agenda needs besides the goal.
pub(crate) struct Env<'a, C> {
    pub pool: &'a TermPool,
    pub rules: &'a [Rule],
    pub index: &'a RuleIndex,
    pub costs: &'a C,
    /// Per rule: automatic for the cost model and without free schema variables.
    pub auto: &'a [bool],
    pub age_weight: i64,
}

/// The candidates of one goal: every match of an automatic rule in its sequent whose cost is not
/// [`Cost::TOP`], each once.
#[derive(Debug, Default)]
pub struct Agenda {
    cands: Vec<Candidate>,
}

impl Agenda {
    /// Matches every formula of `seq` against the rule index. `births` gives, per side
    /// (`[antecedent, succedent]`) and position, the depth at which the formula entered the
    /// sequent.
    pub(crate) fn for_goal<C: CostModel>(
        env: &Env<'_, C>,
        seq: &Sequent,
        births: &[Vec<u32>; 2],
    ) -> Agenda {
        let mut ag = Agenda::default();
        let mut bs: Vec<Option<Bindings>> = vec![None; env.rules.len()];
        let mut out = Vec::new();
        let add = |ag: &mut Agenda, rule: RuleId, find: Option<PosInOccurrence>, b: &Bindings| {
            let r = &env.rules[rule as usize];
            let cost = env.costs.cost(&CostCtx {
                pool: env.pool,
                rule_id: rule,
                rule: r,
                seq,
                find: find.as_ref(),
                bindings: b,
            });
            if cost.is_top() {
                return;
            }
            let birth_of = |side: Side, i: usize| births[side_ix(side)][i];
            let mut birth = find.as_ref().map_or(0, |p| birth_of(p.side, p.index));
            for &(side, i) in b.matched() {
                birth = birth.max(birth_of(side, i as usize));
            }
            let key = cost + Cost(env.age_weight.saturating_mul(birth as i64));
            ag.cands.push(Candidate {
                rule,
                find,
                bindings: b.clone(),
                cost,
                birth,
                key,
            });
        };
        // one `Bindings` per rule, created when first needed and reused
        let take = |bs: &mut Vec<Option<Bindings>>, r: RuleId| -> Bindings {
            bs[r as usize]
                .take()
                .unwrap_or_else(|| Bindings::new(&env.rules[r as usize]))
        };
        for side in [Side::Antecedent, Side::Succedent] {
            for (i, &f) in seq.semisequent(side).iter().enumerate() {
                // sequent finds with a part on this side, focused on `f`
                env.index.sequent_candidates(env.pool, side, f, &mut out);
                for &r in &out {
                    let rule = &env.rules[r as usize];
                    if !env.auto[r as usize] {
                        continue;
                    }
                    // a find with parts on both sides is found from its antecedent formula
                    if let Find::Sequent { ante: Some(_), .. } = rule.find()
                        && side == Side::Succedent
                    {
                        continue;
                    }
                    let mut b = take(&mut bs, r);
                    let _ = match_sequent_at(env.pool, rule, seq, side, i, &mut b, &mut |m| {
                        add(&mut ag, r, None, m);
                        ControlFlow::Continue(())
                    });
                    bs[r as usize] = Some(b);
                }
                // term finds at every subterm
                let mut todo: Vec<(TermIdx, PosInTerm)> = vec![(f, PosInTerm::top())];
                while let Some((t, path)) = todo.pop() {
                    env.index.candidates(env.pool, t, &mut out);
                    for &r in &out {
                        if !env.auto[r as usize] {
                            continue;
                        }
                        let rule = &env.rules[r as usize];
                        let pos = PosInOccurrence::new(side, i, path.clone());
                        let mut b = take(&mut bs, r);
                        let _ = match_term(env.pool, rule, seq, t, &mut b, &mut |m| {
                            add(&mut ag, r, Some(pos.clone()), m);
                            ControlFlow::Continue(())
                        });
                        bs[r as usize] = Some(b);
                    }
                    for (k, &s) in env.pool.subs_of(t).iter().enumerate().rev() {
                        todo.push((s, path.down(k as u16)));
                    }
                }
            }
        }
        // rules without find: those with `\assumes` (the plain ones, like `cut`, have free schema
        // variables and are not automatic)
        for &r in env.index.no_find() {
            if !env.auto[r as usize] {
                continue;
            }
            let rule = &env.rules[r as usize];
            let mut b = take(&mut bs, r);
            let _ = match_no_find(env.pool, rule, seq, &mut b, &mut |m| {
                add(&mut ag, r, None, m);
                ControlFlow::Continue(())
            });
            bs[r as usize] = Some(b);
        }
        ag
    }

    /// The candidate to apply: the least by [`Candidate::cmp_key`].
    pub fn best(&self) -> Option<&Candidate> {
        self.cands.iter().min_by(|a, b| a.cmp_key(b))
    }

    pub fn candidates(&self) -> &[Candidate] {
        &self.cands
    }

    pub fn len(&self) -> usize {
        self.cands.len()
    }

    pub fn is_empty(&self) -> bool {
        self.cands.is_empty()
    }
}

pub(crate) fn side_ix(side: Side) -> usize {
    match side {
        Side::Antecedent => 0,
        Side::Succedent => 1,
    }
}
