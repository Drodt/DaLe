//! A rule index: from a term to the rules whose `\find` could match it.
//!
//! KeY shows that 92% of match attempts fail at
//! the pattern's first comparison, and that for update and modality rules the head operator alone
//! is not selective (`{skip}t`, `{x := v}t` have the same head). The key of a rule is therefore
//! the head operator declaration of its find plus, if some argument pattern has a determinate
//! head too, that argument's position and head. Rules whose find is a schema variable can match
//! any term of their sort and are listed separately.
//!
//! Only rules with a [`Find::Term`] find are indexed; sequent finds start from the sequent.

use crate::ids::{OpDeclId, TermIdx};
use crate::pattern::{Find, PatId, PatNode, Rule, SortReq, SvKind};
use crate::pool::TermPool;

/// Position of a rule in the slice given to [`RuleIndex::build`].
pub type RuleId = u32;

type HeadIndex = (OpDeclId, Vec<u32>);

#[derive(Default, Debug)]
struct Bucket {
    /// Rules that only constrain the head.
    any: Vec<RuleId>,
    /// `(argument position, [(head of that argument, rules)])`, the inner list sorted by head.
    keyed: Vec<(u32, Vec<HeadIndex>)>,
}

#[derive(Debug)]
struct Wild {
    kind: SvKind,
    sort: SortReq,
    rule: RuleId,
}

#[derive(Debug, Default)]
pub struct RuleIndex {
    /// Indexed by operator declaration.
    buckets: Vec<Bucket>,
    wild: Vec<Wild>,
}

impl RuleIndex {
    pub fn build(pool: &TermPool, rules: &[Rule]) -> Self {
        let n_ops = pool.signature().op_decls().count();
        let mut ix = RuleIndex {
            buckets: (0..n_ops).map(|_| Bucket::default()).collect(),
            wild: Vec::new(),
        };
        for (i, r) in rules.iter().enumerate() {
            let Find::Term(p) = r.find() else { continue };
            let id = i as RuleId;
            match r.node(p) {
                PatNode::Sv(sv) => {
                    let d = r.sv(*sv);
                    // a slot-bound sort is checked when matching
                    let sort = match d.sort {
                        SortReq::Slot(_) => SortReq::Any,
                        s => s,
                    };
                    ix.wild.push(Wild {
                        kind: d.kind,
                        sort,
                        rule: id,
                    });
                }
                PatNode::Ground(g) => {
                    let head = pool[pool.op_of(*g)].decl();
                    ix.buckets[head.index()].any.push(id);
                }
                PatNode::App { decl, args, .. } => {
                    let key = args
                        .iter()
                        .enumerate()
                        .find_map(|(pos, a)| head_of(pool, r, *a).map(|h| (pos as u32, h)));
                    let b = &mut ix.buckets[decl.index()];
                    match key {
                        None => b.any.push(id),
                        Some((pos, h)) => {
                            let slot = match b.keyed.iter().position(|(p, _)| *p == pos) {
                                Some(s) => s,
                                None => {
                                    b.keyed.push((pos, Vec::new()));
                                    b.keyed.len() - 1
                                }
                            };
                            let list = &mut b.keyed[slot].1;
                            match list.binary_search_by_key(&h, |(d, _)| *d) {
                                Ok(j) => list[j].1.push(id),
                                Err(j) => list.insert(j, (h, vec![id])),
                            }
                        }
                    }
                }
            }
        }
        ix
    }

    /// Appends the rules that may match `t` to `out` (after clearing it). Every rule that matches
    /// is among them; most of the others are filtered out.
    pub fn candidates(&self, pool: &TermPool, t: TermIdx, out: &mut Vec<RuleId>) {
        out.clear();
        let op = &pool[pool.op_of(t)];
        let b = &self.buckets[op.decl().index()];
        out.extend_from_slice(&b.any);
        if !b.keyed.is_empty() {
            let subs = pool.subs_of(t);
            for (pos, list) in &b.keyed {
                let Some(&c) = subs.get(*pos as usize) else {
                    continue;
                };
                let h = pool[pool.op_of(c)].decl();
                if let Ok(j) = list.binary_search_by_key(&h, |(d, _)| *d) {
                    out.extend_from_slice(&list[j].1);
                }
            }
        }
        if !self.wild.is_empty() {
            let sort = pool.sort_of(t);
            let is_formula = sort == pool.formula();
            for w in &self.wild {
                let kind_ok = match w.kind {
                    SvKind::Formula => is_formula,
                    SvKind::Term => !is_formula,
                };
                let sort_ok = match w.sort {
                    SortReq::Any | SortReq::Slot(_) => true,
                    SortReq::Sub(s) => pool.is_subsort(sort, s),
                };
                if kind_ok && sort_ok {
                    out.push(w.rule);
                }
            }
        }
    }

    pub fn num_wild(&self) -> usize {
        self.wild.len()
    }
}

fn head_of(pool: &TermPool, r: &Rule, p: PatId) -> Option<OpDeclId> {
    match r.node(p) {
        PatNode::App { decl, .. } => Some(*decl),
        PatNode::Ground(g) => Some(pool[pool.op_of(*g)].decl()),
        PatNode::Sv(_) => None,
    }
}
