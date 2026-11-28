//! A rule index: from a term to the rules whose `\find` could match it.
//!
//! KeY shows that 92% of match attempts fail at
//! the pattern's first comparison, and that for update and modality rules the head operator alone
//! is not selective (`{skip}t`, `{x := v}t` have the same head). The key of a rule is therefore
//! the head operator declaration of its find plus, if some argument pattern has a determinate
//! head too, that argument's position and head. Rules whose find is a schema variable can match
//! any term of their sort and are listed separately.
//!
//! Term finds and the two parts of sequent finds (`\find(a ==>)`, `\find(==> b)`) are kept in
//! separate tables: a sequent find only matches a top-level formula of its side.
//!
//! Rules without `\find` are kept apart, so that they cost nothing when terms are looked up.
//! KeY uses them in two ways (21 of its ~2400 taclets):
//! * without `\assumes` (`cut`, `int_induction`, `sign_case_distinction`, the `jdiv`/`jmod`
//!   lemmas): they apply to every goal, and what they do depends only on the free schema
//!   variables that the user or the strategy supplies (KeY's strategy generates `cutFormula` and
//!   `signCasesLeft` from the formulas of the sequent). The index lists them once
//!   ([`RuleIndex::no_find_plain`]); a strategy offers them per goal, not per term.
//! * with `\assumes` (`subsortTrans`, `less_is_total_heu`, `seqPermForall`): lemmas that apply
//!   when the sequent has formulas of a shape. KeY keeps all no-find taclets in one list and
//!   tries each on every goal; here every `\assumes` pattern is put into a table of its side,
//!   keyed like a find, so a new formula only meets the rules that could use it
//!   ([`RuleIndex::assumes_candidates`], matched with
//!   [`match_no_find_at`](crate::match_no_find_at)). With incremental goals (a child shares
//!   its parent's formulas) only the added formulas need a look-up.

use crate::ids::{OpDeclId, TermIdx};
use crate::pattern::{Find, PatId, PatNode, Rule, SortReq, SvKind};
use crate::pool::TermPool;
use crate::sequent::Side;

/// Position of a rule in the slice given to [`RuleIndex::build`].
pub type RuleId = u32;

type HeadIndex<E> = (OpDeclId, Vec<E>);

#[derive(Debug)]
struct Bucket<E> {
    /// Rules that only constrain the head.
    any: Vec<E>,
    /// `(argument position, [(head of that argument, rules)])`, the inner list sorted by head.
    keyed: Vec<(u32, Vec<HeadIndex<E>>)>,
}

impl<E> Default for Bucket<E> {
    fn default() -> Self {
        Bucket {
            any: Vec::new(),
            keyed: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct Wild<E> {
    kind: SvKind,
    sort: SortReq,
    rule: E,
}

/// The rules of one kind of pattern (term finds, the antecedent or succedent parts of sequent
/// finds, or the `\assumes` of no-find rules on one side), keyed as described in the module
/// documentation. `E` is what is listed: a rule, or a rule and one of its `\assumes`.
#[derive(Debug)]
struct Table<E> {
    /// Indexed by operator declaration.
    buckets: Vec<Bucket<E>>,
    wild: Vec<Wild<E>>,
}

impl<E> Default for Table<E> {
    fn default() -> Self {
        Table {
            buckets: Vec::new(),
            wild: Vec::new(),
        }
    }
}

/// A no-find rule and the number of one of its `\assumes` patterns.
pub type AssumesRef = (RuleId, u32);

#[derive(Debug, Default)]
pub struct RuleIndex {
    term: Table<RuleId>,
    /// The parts of sequent finds: `[antecedent, succedent]`. A rule whose find has both parts is
    /// in both tables.
    seq: [Table<RuleId>; 2],
    /// The `\assumes` patterns of rules without find, by side.
    assumes: [Table<AssumesRef>; 2],
    /// All rules without find, and those of them without `\assumes`.
    no_find: Vec<RuleId>,
    no_find_plain: Vec<RuleId>,
}

impl<E: Copy> Table<E> {
    fn new(n_ops: usize) -> Self {
        Table {
            buckets: (0..n_ops).map(|_| Bucket::default()).collect(),
            wild: Vec::new(),
        }
    }

    fn insert(&mut self, pool: &TermPool, r: &Rule, p: PatId, id: E) {
        match r.node(p) {
            PatNode::Sv(sv) => {
                let d = r.sv(*sv);
                // a slot-bound sort is checked when matching
                let sort = match d.sort {
                    SortReq::Slot(_) => SortReq::Any,
                    s => s,
                };
                self.wild.push(Wild {
                    kind: d.kind,
                    sort,
                    rule: id,
                });
            }
            PatNode::Ground(g) => {
                let head = pool[pool.op_of(*g)].decl();
                self.buckets[head.index()].any.push(id);
            }
            PatNode::App { decl, args, .. } => {
                let key = args
                    .iter()
                    .enumerate()
                    .find_map(|(pos, a)| head_of(pool, r, *a).map(|h| (pos as u32, h)));
                let b = &mut self.buckets[decl.index()];
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

    fn candidates(&self, pool: &TermPool, t: TermIdx, out: &mut Vec<E>) {
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
}

impl RuleIndex {
    pub fn build(pool: &TermPool, rules: &[Rule]) -> Self {
        let n_ops = pool.signature().op_decls().count();
        let mut ix = RuleIndex {
            term: Table::new(n_ops),
            seq: [Table::new(n_ops), Table::new(n_ops)],
            assumes: [Table::new(n_ops), Table::new(n_ops)],
            no_find: Vec::new(),
            no_find_plain: Vec::new(),
        };
        for (i, r) in rules.iter().enumerate() {
            let id = i as RuleId;
            match r.find() {
                Find::Term(p) => ix.term.insert(pool, r, p, id),
                Find::Sequent { ante, succ } => {
                    if let Some(p) = ante {
                        ix.seq[0].insert(pool, r, p, id);
                    }
                    if let Some(p) = succ {
                        ix.seq[1].insert(pool, r, p, id);
                    }
                }
                Find::None => {
                    ix.no_find.push(id);
                    if r.assumes().is_empty() {
                        ix.no_find_plain.push(id);
                    }
                    for (j, a) in r.assumes().iter().enumerate() {
                        ix.assumes[side_index(a.side)].insert(pool, r, a.pat, (id, j as u32));
                    }
                }
            }
        }
        ix
    }

    /// Appends the rules with a term find that may match `t` to `out` (after clearing it). Every
    /// rule that matches is among them; most of the others are filtered out.
    pub fn candidates(&self, pool: &TermPool, t: TermIdx, out: &mut Vec<RuleId>) {
        self.term.candidates(pool, t, out)
    }

    /// Like [`candidates`](Self::candidates) for the top-level formula `t` on `side`: the rules
    /// with a sequent find whose part on `side` may match `t`. Match them with
    /// [`match_sequent_at`](crate::match_sequent_at).
    pub fn sequent_candidates(
        &self,
        pool: &TermPool,
        side: Side,
        t: TermIdx,
        out: &mut Vec<RuleId>,
    ) {
        self.seq[side_index(side)].candidates(pool, t, out)
    }

    /// The rules without `\find`, in rule order.
    pub fn no_find(&self) -> &[RuleId] {
        &self.no_find
    }

    /// The rules without `\find` and without `\assumes`: each applies to every goal, given its
    /// free schema variables. Match them with [`match_no_find`](crate::match_no_find).
    pub fn no_find_plain(&self) -> &[RuleId] {
        &self.no_find_plain
    }

    /// Appends to `out` (after clearing it) the `\assumes` patterns of rules without find that
    /// are on `side` and may match its top-level formula `t`, as (rule, number of the pattern).
    /// Every match of such a rule that uses `t` comes from one of them through
    /// [`match_no_find_at`](crate::match_no_find_at).
    pub fn assumes_candidates(
        &self,
        pool: &TermPool,
        side: Side,
        t: TermIdx,
        out: &mut Vec<AssumesRef>,
    ) {
        self.assumes[side_index(side)].candidates(pool, t, out)
    }

    /// The number of term-find rules whose find is a schema variable (candidates for every term).
    pub fn num_wild(&self) -> usize {
        self.term.wild.len()
    }

    /// Like [`num_wild`](Self::num_wild) for the sequent-find parts on `side`.
    pub fn num_wild_sequent(&self, side: Side) -> usize {
        self.seq[side_index(side)].wild.len()
    }
}

fn side_index(side: Side) -> usize {
    match side {
        Side::Antecedent => 0,
        Side::Succedent => 1,
    }
}

fn head_of(pool: &TermPool, r: &Rule, p: PatId) -> Option<OpDeclId> {
    match r.node(p) {
        PatNode::App { decl, .. } => Some(*decl),
        PatNode::Ground(g) => Some(pool[pool.op_of(*g)].decl()),
        PatNode::Sv(_) => None,
    }
}
