//! Applying a rule to a goal: from a match to the new goals.
//!
//! [`apply_rule`] takes a rule, the sequent it matched and the [`Bindings`] of the match, and
//! builds one new sequent per goal template: the found term or formula is replaced, formulas are
//! added. It returns them as [`Branch`]es together with the [`Application`] record, ready for
//! [`ProofTree::apply`](crate::ProofTree::apply).
//!
//! Binders: a schema variable matched `d0` pattern binders deep and used `d1` binders deep in a
//! template is shifted by `d1 - d0` (lifted if it moves into binders, lowered if it moves out).
//! That is only meaningful if the term does not mention the variables of those pattern binders;
//! otherwise the application fails with [`ApplyError::CapturesBinder`]. Rules state this with
//! [`Cond::NoFreeVar`](crate::Cond::NoFreeVar), which makes the match fail before.
//!
//! Rules without `\find` apply to the goal as a whole: no position, only added formulas. Their
//! free schema variables (and those of any rule, [`Rule::free_svs`]) must have been set in the
//! bindings ([`Bindings::set`]); an unset one is [`ApplyError::UnboundSv`].
//!
//! TODO: new symbols (`\new`, skolem constants), meta operations (`#typeof`, ...), `\addrules`,
//! update handling.

use crate::error::LogicError;
use crate::ids::TermIdx;
use crate::index::RuleId;
use crate::matcher::Bindings;
use crate::pattern::{Find, GenTmpl, Replace, Rule, SvId, TmplId, TmplNode};
use crate::pool::TermPool;
use crate::pos::{PosInOccurrence, PosInTerm, SortCheck};
use crate::proof::{Application, Branch};
use crate::sequent::{Sequent, SequentError, Side, SideEdit};
use crate::sig::binds_arg;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ApplyError {
    /// Building a term failed.
    Term(LogicError),
    /// A new sequent is not valid, e.g. an added formula is not closed.
    Sequent(SequentError),
    /// A template uses a schema variable the match did not bind.
    UnboundSv(SvId),
    UnboundSlot(u32),
    /// A term that mentions the variable of a pattern binder is used at a different binder depth.
    CapturesBinder(SvId),
    /// The rule has a term `\find`, but no position was given.
    MissingFindPosition,
    /// The bindings do not come from a match of this rule.
    BadMatch,
}

impl From<LogicError> for ApplyError {
    fn from(e: LogicError) -> Self {
        ApplyError::Term(e)
    }
}

impl From<SequentError> for ApplyError {
    fn from(e: SequentError) -> Self {
        ApplyError::Sequent(e)
    }
}

/// The result of a rule application.
#[derive(Debug)]
pub struct Applied {
    pub app: Application,
    /// One per goal template; empty if the rule closes the goal.
    pub branches: Vec<Branch>,
}

/// Applies `rule` (number `id` in the caller's rule list) to `seq`, which the match `b` was found
/// in. For a term `\find`, `find` is the position of the matched term in `seq`; for a sequent
/// `\find` or none it is not needed (the match knows the formulas). New terms are built in `pool`;
/// `checks` says whether their sorts are verified again (rules are checked when loaded).
pub fn apply_rule(
    pool: &mut TermPool,
    rule: &Rule,
    id: RuleId,
    seq: &Sequent,
    find: Option<&PosInOccurrence>,
    b: &Bindings,
    checks: SortCheck,
) -> Result<Applied, ApplyError> {
    let top =
        |&(side, index): &(Side, u32)| PosInOccurrence::new(side, index as usize, PosInTerm::top());
    let (find_pos, n_find): (Vec<PosInOccurrence>, usize) = match rule.find() {
        Find::Term(_) => (
            vec![find.ok_or(ApplyError::MissingFindPosition)?.clone()],
            0,
        ),
        Find::Sequent { ante, succ } => {
            let n = ante.is_some() as usize + succ.is_some() as usize;
            let m = b.matched().get(..n).ok_or(ApplyError::BadMatch)?;
            (m.iter().map(top).collect(), n)
        }
        Find::None => (Vec::new(), 0),
    };
    let assumes: Box<[PosInOccurrence]> = b
        .matched()
        .get(n_find..)
        .ok_or(ApplyError::BadMatch)?
        .iter()
        .map(top)
        .collect();
    let mut branches = Vec::with_capacity(rule.goals().len());
    for g in rule.goals() {
        // what replaces the find, per side (`[antecedent, succedent]`)
        let mut replaced: [Option<(usize, Insts)>; 2] = [None, None];
        match &g.replace {
            Replace::Nothing => {}
            Replace::Term(t) => {
                let new = inst(pool, rule, b, *t, 0, checks)?;
                let at = &find_pos[0];
                let f = seq
                    .get(at.side, at.index)
                    .ok_or(SequentError::IndexOutOfRange {
                        side: at.side,
                        index: at.index,
                    })?;
                let nf = pool
                    .replace_at(f, at.term_pos.as_slice(), new, checks)
                    .map_err(SequentError::from)?;
                replaced[side_index(at.side)] = Some((at.index, Insts::One(nf)));
            }
            Replace::Sequent { ante, succ } => {
                let Find::Sequent {
                    ante: fa, succ: fs, ..
                } = rule.find()
                else {
                    return Err(ApplyError::BadMatch);
                };
                let mut parts = find_pos.iter();
                for (has, side, list) in [
                    (fa.is_some(), Side::Antecedent, ante),
                    (fs.is_some(), Side::Succedent, succ),
                ] {
                    if !has {
                        continue;
                    }
                    let at = parts.next().ok_or(ApplyError::BadMatch)?;
                    let new = inst_all(pool, rule, b, list, checks)?;
                    replaced[side_index(side)] = Some((at.index, new));
                }
            }
        }
        let ante = inst_all(pool, rule, b, &g.add_ante, checks)?;
        let succ = inst_all(pool, rule, b, &g.add_succ, checks)?;
        let e = seq.edit(
            pool,
            [
                side_edit(&replaced[0], &ante),
                side_edit(&replaced[1], &succ),
            ],
        )?;
        branches.push(Branch {
            label: g.label,
            sequent: e.sequent,
            new_formulas: Some(e.new),
        });
    }
    Ok(Applied {
        app: Application {
            rule: id,
            find: find_pos.into(),
            assumes,
            inst: b.instantiation(),
        },
        branches,
    })
}

/// Instantiated templates. Most lists have at most one entry, which then needs no allocation.
enum Insts {
    One(TermIdx),
    Many(Vec<TermIdx>),
}

impl Insts {
    fn as_slice(&self) -> &[TermIdx] {
        match self {
            Insts::One(t) => std::slice::from_ref(t),
            Insts::Many(v) => v,
        }
    }
}

fn inst_all(
    pool: &mut TermPool,
    rule: &Rule,
    b: &Bindings,
    ts: &[TmplId],
    checks: SortCheck,
) -> Result<Insts, ApplyError> {
    if let [t] = ts {
        return Ok(Insts::One(inst(pool, rule, b, *t, 0, checks)?));
    }
    ts.iter()
        .map(|&t| inst(pool, rule, b, t, 0, checks))
        .collect::<Result<_, _>>()
        .map(Insts::Many)
}

/// Builds the term of a template node below `depth` binders of the template.
fn inst(
    pool: &mut TermPool,
    rule: &Rule,
    b: &Bindings,
    t: TmplId,
    depth: u32,
    checks: SortCheck,
) -> Result<TermIdx, ApplyError> {
    match rule.tmpl(t) {
        TmplNode::Ground(g) => Ok(*g),
        TmplNode::Sv(sv) => {
            let (term, d0) = b.get_at_depth(*sv).ok_or(ApplyError::UnboundSv(*sv))?;
            if d0 == depth {
                Ok(term)
            } else if pool[term].may_have_free_below(d0) {
                Err(ApplyError::CapturesBinder(*sv))
            } else if depth > d0 {
                Ok(pool.lift(term, depth - d0)?)
            } else {
                Ok(pool.lower(term, d0 - depth)?)
            }
        }
        TmplNode::App {
            decl,
            gens,
            meta,
            args,
            bind_mask,
        } => {
            // the arguments; up to `SMALL` of them on the stack
            const SMALL: usize = 4;
            let mut small = [TermIdx::from_u32(0); SMALL];
            let mut large = Vec::new();
            for (i, &x) in args.iter().enumerate() {
                let d = depth + binds_arg(*bind_mask, i) as u32;
                let t = inst(pool, rule, b, x, d, checks)?;
                if args.len() <= SMALL {
                    small[i] = t;
                } else {
                    large.push(t);
                }
            }
            let a: &[TermIdx] = if args.len() <= SMALL {
                &small[..args.len()]
            } else {
                &large
            };
            if gens.iter().any(|g| matches!(g, GenTmpl::Infer)) {
                return Ok(pool.mk_inferred(*decl, meta, a)?);
            }
            let mut ga = Vec::with_capacity(gens.len());
            for g in gens.iter() {
                ga.push(match g {
                    GenTmpl::Exact(x) => *x,
                    GenTmpl::Slot(i) => b.slot(*i).ok_or(ApplyError::UnboundSlot(*i))?,
                    GenTmpl::Infer => unreachable!(),
                });
            }
            let op = pool.mk_op(*decl, &ga, meta)?;
            Ok(match checks {
                SortCheck::Enforce => pool.mk(op, a)?,
                SortCheck::Skip => pool.mk_unchecked(op, a),
            })
        }
    }
}

fn side_index(side: Side) -> usize {
    match side {
        Side::Antecedent => 0,
        Side::Succedent => 1,
    }
}

fn side_edit<'a>(replaced: &'a Option<(usize, Insts)>, add: &'a Insts) -> SideEdit<'a> {
    SideEdit {
        replace: replaced.as_ref().map(|(i, v)| (*i, v.as_slice())),
        add: add.as_slice(),
    }
}
