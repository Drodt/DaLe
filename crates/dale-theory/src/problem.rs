//! Lowering the `\problem` of a file into a [`Sequent`].
//!
//! ```text
//! \problem {
//!   \use RustyDL::FOL::*;
//!   \use Atoms::*;
//!   p ==> all<int>(le(var(0), zero))
//! }
//! ```
//!
//! Terms are written in prefix notation with de Bruijn indices: `var(n)` is the variable of the
//! `n`-th enclosing binder (0 is the closest). Its sort is the variable sort of that binder, so a
//! binder with generic parameters needs explicit generic arguments (`all<int>(..)`); other
//! operators may leave them out and have them inferred from their arguments. Number literals
//! are meta arguments and must come first (`var(0)`).
//!
//! All sorts (arguments, binders, generic instantiations) are checked by the [`TermPool`]; every
//! formula of the sequent must be closed.

use std::collections::HashMap;

use dale_logic::{
    GenArg, MetaValue, OpDeclId, OpKind, Sequent, Signature, SortDeclId, SortIdx, TermIdx, TermPool,
};
use dale_syn::Parsed;
use dale_syn::ir::{DefId, GenericArg, Res, SortRef, Span, Term, TermKind};

use crate::Diag;

type R<T> = Result<T, Diag>;

fn fail<T>(span: Span, msg: impl Into<String>) -> R<T> {
    Err(Diag::new(Some(span), msg))
}

pub(crate) fn lower_problem(
    parsed: &Parsed,
    sig: &Signature,
    pool: &mut TermPool,
    sorts: &HashMap<DefId, SortDeclId>,
    ops: &HashMap<DefId, OpDeclId>,
) -> R<Option<Sequent>> {
    let Some(seq) = parsed.map.problem() else {
        return Ok(None);
    };
    let mut l = ProblemLower {
        sig,
        pool,
        sorts,
        ops,
        binders: Vec::new(),
    };
    let mut sides = [Vec::new(), Vec::new()];
    for (side, ts) in sides.iter_mut().zip([seq.ante, seq.succ]) {
        for t in ts {
            let f = l.term(t)?;
            if !l.pool.is_formula(f) {
                return fail(t.span, "not a formula");
            }
            side.push(f);
        }
    }
    let [ante, succ] = sides;
    Sequent::from_formulas(l.pool, &ante, &succ)
        .map(Some)
        .map_err(|e| Diag::new(None, format!("problem: {e:?}")))
}

struct ProblemLower<'a> {
    sig: &'a Signature,
    pool: &'a mut TermPool,
    sorts: &'a HashMap<DefId, SortDeclId>,
    ops: &'a HashMap<DefId, OpDeclId>,
    /// The variable sorts of the enclosing binders, innermost last.
    binders: Vec<SortIdx>,
}

impl ProblemLower<'_> {
    fn sort(&mut self, r: &SortRef<'static>) -> R<SortIdx> {
        let Res::Def(_, def) = r.path.res else {
            return fail(r.span, "unresolved sort");
        };
        let Some(&decl) = self.sorts.get(&def) else {
            return fail(r.span, "not a sort of the signature");
        };
        let args = r
            .args
            .iter()
            .map(|a| self.gen_arg(a))
            .collect::<R<Vec<_>>>()?;
        self.pool
            .mk_sort(decl, &args)
            .map_err(|e| Diag::new(Some(r.span), e.to_string()))
    }

    fn gen_arg(&mut self, a: &GenericArg<'static>) -> R<GenArg> {
        match a {
            GenericArg::Sort(s) => Ok(GenArg::Sort(self.sort(s)?)),
            GenericArg::Const(t, _) => Ok(GenArg::Const(self.term(t)?)),
        }
    }

    fn term(&mut self, t: &Term<'static>) -> R<TermIdx> {
        let err = |e: dale_logic::LogicError| Diag::new(Some(t.span), e.to_string());
        let (path, gens, subs) = match t.kind {
            TermKind::Path(p, g) => (p, g, &[][..]),
            TermKind::Call(p, g, s) => (p, g, s),
            TermKind::Num(_) => return fail(t.span, "a number can only be a meta argument"),
        };
        let Res::Def(_, def) = path.res else {
            return fail(t.span, "unresolved name");
        };
        let Some(&op) = self.ops.get(&def) else {
            return fail(t.span, "not an operator, function or predicate");
        };
        let d = self.sig.op_decl(op);
        let n_meta = d.meta.len();
        if subs.len() < n_meta {
            return fail(t.span, format!("expected {n_meta} meta arguments first"));
        }
        let mut meta = Vec::with_capacity(n_meta);
        for s in &subs[..n_meta] {
            let TermKind::Num(n) = s.kind else {
                return fail(s.span, "expected a number (a meta argument)");
            };
            meta.push(MetaValue::Nat(n));
        }
        let subs = &subs[n_meta..];
        let (kind, n_gens, binds) = (d.kind, d.generics.len(), d.bind.is_some());

        if kind == OpKind::BoundVar {
            let [MetaValue::Nat(n)] = meta[..] else {
                return fail(t.span, "the bound variable has one meta argument");
            };
            let sort = if gens.is_empty() {
                match (self.binders.len() as u64).checked_sub(n + 1) {
                    Some(i) => self.binders[i as usize],
                    None => return fail(t.span, format!("`var({n})` is not bound here")),
                }
            } else {
                let [GenericArg::Sort(s)] = gens else {
                    return fail(t.span, "the bound variable has one sort argument");
                };
                self.sort(s)?
            };
            return self.pool.mk_var(sort, n).map_err(err);
        }

        if gens.is_empty() && n_gens > 0 {
            if binds {
                return fail(
                    t.span,
                    "a binder needs explicit generic arguments, e.g. `all<int>(..)`",
                );
            }
            let args = subs.iter().map(|s| self.term(s)).collect::<R<Vec<_>>>()?;
            return self.pool.mk_inferred(op, &meta, &args).map_err(err);
        }
        let gens = gens
            .iter()
            .map(|g| self.gen_arg(g))
            .collect::<R<Vec<_>>>()?;
        let o = self.pool.mk_op(op, &gens, &meta).map_err(err)?;
        let bind = self.pool[o].bind();
        let mut args = Vec::with_capacity(subs.len());
        for (i, s) in subs.iter().enumerate() {
            let bound = self.pool[o].binds(i);
            if let (true, Some((_, var_sort))) = (bound, bind) {
                self.binders.push(var_sort);
            }
            let a = self.term(s);
            if bound {
                self.binders.pop();
            }
            args.push(a?);
        }
        self.pool.mk(o, &args).map_err(err)
    }
}
