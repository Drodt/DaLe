//! Lowering the `\rules` of the IR into [`Rule`]s.
//!
//! * `\schemaVar b: S`: a formula schema variable if `S` is the `\formula` sort, a term schema
//!   variable of any sort if `S` is the `\top` sort, else a term schema variable of `S` or a
//!   subsort.
//! * `\find(t)` is a term find, `\find(a ==> b)` a sequent find (at most one formula per side).
//!   `\assumes(a1, .. ==> b1, ..)` gives one assumes pattern per formula.
//! * Subterms without schema variables become ground terms of the pool (matched by index
//!   comparison). An operator written without generic arguments matches any instantiation in a
//!   pattern (each generic parameter gets its own slot) and is inferred from its arguments in a
//!   template.
//! * `\closegoal` gives no goal templates. Each goal spec gives one template: `\replacewith(t)`
//!   for a term find, `\replacewith(a ==> b)` for a sequent find, `\add(a ==> b)`, and the label
//!   `"name":`.
//!
//! Not supported yet (the rule is skipped with a message): rules without `\find`, meta arguments
//! in rules, sort parameters in schema variable sorts, a term in `\assumes` or `\add`.

use std::collections::HashMap;
use std::sync::Arc;

use dale_logic::Side;
use dale_logic::{
    GenArg, GenPat, GenTmpl, GoalTemplate, OpDeclId, PatId, Replace, Rule, RuleBuilder, Signature,
    SortDeclId, SortIdx, SortReq, SvId, SvKind, TermIdx, TermPool, TmplId,
};
use dale_syn::Parsed;
use dale_syn::ir::{
    self, DefId, DefKind, GenericArg, GoalSpecs, ItemKind, Res, SortRef, Span, Term, TermKind,
    TermOrSeq,
};
use dale_util::symbol::Symbol;

use crate::{Diag, sig::items};

/// What the strategy needs to know about a rule besides its patterns.
#[derive(Clone, Debug)]
pub struct RuleInfo {
    pub name: Symbol,
    pub display_name: Option<Symbol>,
    /// The rulesets the rule belongs to (the last segment of each `\rulesets(..)` path). Ruleset
    /// arguments are not lowered yet.
    pub rule_sets: Vec<Symbol>,
    /// The IR declaration, e.g. to find its ruleset references again.
    pub def: DefId,
    pub span: Span,
}

pub(crate) fn lower_rules(
    parsed: &Parsed,
    sig: &Arc<Signature>,
    pool: &mut TermPool,
    sorts: &HashMap<DefId, SortDeclId>,
    ops: &HashMap<DefId, OpDeclId>,
) -> (Vec<Rule>, Vec<RuleInfo>, Vec<Diag>) {
    let mut rules = Vec::new();
    let mut info = Vec::new();
    let mut skipped = Vec::new();
    for item in items(parsed) {
        let ItemKind::Rules(rs) = item.kind else {
            continue;
        };
        for r in rs {
            let mut l = RuleLower {
                sig,
                pool: &mut *pool,
                sorts,
                ops,
                b: RuleBuilder::new(sig.clone(), r.name.name),
                svs: HashMap::new(),
                slots: 0,
            };
            match l.rule(r) {
                Ok(rule) => {
                    rules.push(rule);
                    info.push(RuleInfo {
                        name: r.name.name,
                        display_name: r.display_name.map(|d| d.name),
                        rule_sets: r
                            .rule_sets
                            .map(|(_, refs)| {
                                refs.iter()
                                    .filter_map(|x| x.path.segments.last().map(|s| s.ident.name))
                                    .collect()
                            })
                            .unwrap_or_default(),
                        def: r.id,
                        span: r.span,
                    });
                }
                Err((span, msg)) => skipped.push(Diag::new(
                    Some(span.unwrap_or(r.span)),
                    format!("rule `{}` skipped: {msg}", parsed.interner.get(r.name.name)),
                )),
            }
        }
    }
    (rules, info, skipped)
}

type Res2<T> = Result<T, (Option<Span>, String)>;

fn fail<T>(span: Span, msg: impl Into<String>) -> Res2<T> {
    Err((Some(span), msg.into()))
}

struct RuleLower<'a> {
    sig: &'a Arc<Signature>,
    pool: &'a mut TermPool,
    sorts: &'a HashMap<DefId, SortDeclId>,
    ops: &'a HashMap<DefId, OpDeclId>,
    b: RuleBuilder,
    svs: HashMap<DefId, SvId>,
    slots: u32,
}

/// What a term's head refers to.
enum Head<'ir> {
    Sv(SvId),
    Op(OpDeclId, &'ir [GenericArg<'ir>], &'ir [Term<'ir>]),
}

impl RuleLower<'_> {
    fn rule(&mut self, r: &ir::Rule<'static>) -> Res2<Rule> {
        let Some((find_span, find)) = r.find else {
            // TODO: lower rules without `\find` once dale-logic supports them (see
            // `PatternError::NoFind`).
            return fail(r.span, "rules without \\find are not supported yet");
        };
        for sv in r.schema_vars {
            let (kind, sort) = self.sv_sort(sv.sort)?;
            let id = self.b.declare_sv(sv.name.name, kind, sort);
            self.svs.insert(sv.id, id);
        }
        let mut find_sides = (false, false);
        let term_find = match find {
            TermOrSeq::Term(t) => {
                let p = self.pat(t)?;
                self.b.find_term(p);
                true
            }
            TermOrSeq::Seq(s) => {
                if s.ante.len() > 1 || s.succ.len() > 1 {
                    return fail(
                        find_span,
                        "a sequent \\find has at most one formula per side",
                    );
                }
                let ante = s.ante.first().map(|t| self.pat(t)).transpose()?;
                let succ = s.succ.first().map(|t| self.pat(t)).transpose()?;
                find_sides = (ante.is_some(), succ.is_some());
                self.b.find_sequent(ante, succ);
                false
            }
        };
        if let Some((span, a)) = r.assumes {
            let TermOrSeq::Seq(s) = a else {
                return fail(span, "\\assumes needs a sequent");
            };
            for (side, ts) in [(Side::Antecedent, s.ante), (Side::Succedent, s.succ)] {
                for t in ts {
                    let p = self.pat(t)?;
                    self.b.assume(side, p);
                }
            }
        }
        if let GoalSpecs::Specs(specs, _) = r.goal_specs {
            for g in specs {
                // As in KeY, the formulas of a sequent `\replacewith` on a side without a find
                // part are added to that side.
                let (mut extra_ante, mut extra_succ) = (Vec::new(), Vec::new());
                let replace = match g.replace_with {
                    None => Replace::Nothing,
                    Some(TermOrSeq::Term(t)) if term_find => Replace::Term(self.tmpl(t)?),
                    Some(TermOrSeq::Seq(s)) if !term_find => {
                        let mut ante = self.tmpls(s.ante)?;
                        let mut succ = self.tmpls(s.succ)?;
                        if !find_sides.0 {
                            extra_ante = std::mem::take(&mut ante);
                        }
                        if !find_sides.1 {
                            extra_succ = std::mem::take(&mut succ);
                        }
                        Replace::Sequent { ante, succ }
                    }
                    Some(_) => {
                        return fail(
                            g.span,
                            "\\replacewith must have the shape of \\find (a term or a sequent)",
                        );
                    }
                };
                let mut goal = GoalTemplate::new(replace);
                goal.label = g.name.map(|n| n.name);
                match g.add {
                    None => {}
                    Some(TermOrSeq::Seq(s)) => {
                        extra_ante.extend(self.tmpls(s.ante)?);
                        extra_succ.extend(self.tmpls(s.succ)?);
                    }
                    Some(TermOrSeq::Term(t)) => return fail(t.span, "\\add needs a sequent"),
                }
                goal.add_ante = extra_ante;
                goal.add_succ = extra_succ;
                self.b.goal(goal);
            }
        }
        let b = std::mem::replace(&mut self.b, RuleBuilder::new(self.sig.clone(), r.name.name));
        b.finish().map_err(|e| (Some(r.span), format!("{e:?}")))
    }

    fn sv_sort(&mut self, r: &SortRef<'static>) -> Res2<(SvKind, SortReq)> {
        let s = self.sort(r)?;
        Ok(if s == self.pool.formula() {
            (SvKind::Formula, SortReq::Any)
        } else if s == self.pool.top() {
            (SvKind::Term, SortReq::Any)
        } else {
            (SvKind::Term, SortReq::Sub(s))
        })
    }

    /// A ground sort.
    fn sort(&mut self, r: &SortRef<'static>) -> Res2<SortIdx> {
        let Res::Def(_, def) = r.path.res else {
            return fail(r.span, "unresolved sort");
        };
        let Some(&decl) = self.sorts.get(&def) else {
            return fail(
                r.span,
                "not a sort of the signature (a generic parameter or meta sort)",
            );
        };
        let mut args = Vec::with_capacity(r.args.len());
        for a in r.args {
            args.push(self.gen_arg(a)?);
        }
        self.pool
            .mk_sort(decl, &args)
            .map_err(|e| (Some(r.span), e.to_string()))
    }

    fn gen_arg(&mut self, a: &GenericArg<'static>) -> Res2<GenArg> {
        match a {
            GenericArg::Sort(s) => Ok(GenArg::Sort(self.sort(s)?)),
            GenericArg::Const(t, span) => match self.ground(t)? {
                Some(t) => Ok(GenArg::Const(t)),
                None => fail(*span, "a const generic argument must be ground"),
            },
        }
    }

    fn head(&self, t: &Term<'static>) -> Res2<Head<'static>> {
        let (path, gens, subs) = match t.kind {
            TermKind::Path(p, g) => (p, g, &[][..]),
            TermKind::Call(p, g, s) => (p, g, s),
            TermKind::Num(_) => return fail(t.span, "number literals are not supported in rules"),
        };
        let Res::Def(kind, def) = path.res else {
            return fail(t.span, "unresolved name");
        };
        if kind == DefKind::SchemaVar {
            if !subs.is_empty() || !gens.is_empty() {
                return fail(t.span, "a schema variable takes no arguments");
            }
            return match self.svs.get(&def) {
                Some(&sv) => Ok(Head::Sv(sv)),
                None => fail(t.span, "schema variable of another rule"),
            };
        }
        let Some(&op) = self.ops.get(&def) else {
            return fail(t.span, "not an operator of the signature");
        };
        let d = self.sig.op_decl(op);
        if !d.meta.is_empty() {
            return fail(
                t.span,
                "operators with meta arguments are not supported in rules yet",
            );
        }
        if !gens.is_empty() && gens.len() != d.generics.len() {
            return fail(t.span, "wrong number of generic arguments");
        }
        Ok(Head::Op(op, gens, subs))
    }

    /// The term, if it has no schema variables.
    fn ground(&mut self, t: &Term<'static>) -> Res2<Option<TermIdx>> {
        let Head::Op(op, gens, subs) = self.head(t)? else {
            return Ok(None);
        };
        let mut args = Vec::with_capacity(subs.len());
        for s in subs {
            match self.ground(s)? {
                Some(x) => args.push(x),
                None => return Ok(None),
            }
        }
        let n_gens = self.sig.op_decl(op).generics.len();
        let r = if gens.is_empty() && n_gens > 0 {
            self.pool.mk_inferred(op, &[], &args)
        } else {
            let mut ga = Vec::with_capacity(gens.len());
            for g in gens {
                ga.push(self.gen_arg(g)?);
            }
            self.pool
                .mk_op(op, &ga, &[])
                .and_then(|o| self.pool.mk(o, &args))
        };
        r.map(Some).map_err(|e| (Some(t.span), e.to_string()))
    }

    fn pat(&mut self, t: &Term<'static>) -> Res2<PatId> {
        if let Some(g) = self.ground(t)? {
            return Ok(self.b.ground(g));
        }
        let err = |e| (Some(t.span), format!("{e:?}"));
        match self.head(t)? {
            Head::Sv(sv) => self.b.sv(sv).map_err(err),
            Head::Op(op, gens, subs) => {
                let n_gens = self.sig.op_decl(op).generics.len();
                let gp: Vec<GenPat> = if gens.is_empty() {
                    (0..n_gens)
                        .map(|_| {
                            self.slots += 1;
                            GenPat::Slot(self.slots - 1)
                        })
                        .collect()
                } else {
                    gens.iter()
                        .map(|g| self.gen_arg(g).map(GenPat::Exact))
                        .collect::<Res2<_>>()?
                };
                let args = subs.iter().map(|s| self.pat(s)).collect::<Res2<Vec<_>>>()?;
                self.b.app(op, &gp, &[], &args).map_err(err)
            }
        }
    }

    fn tmpl(&mut self, t: &Term<'static>) -> Res2<TmplId> {
        if let Some(g) = self.ground(t)? {
            return Ok(self.b.tmpl_ground(g));
        }
        let err = |e| (Some(t.span), format!("{e:?}"));
        match self.head(t)? {
            Head::Sv(sv) => self.b.tmpl_sv(sv).map_err(err),
            Head::Op(op, gens, subs) => {
                let n_gens = self.sig.op_decl(op).generics.len();
                let gt: Vec<GenTmpl> = if gens.is_empty() {
                    vec![GenTmpl::Infer; n_gens]
                } else {
                    gens.iter()
                        .map(|g| self.gen_arg(g).map(GenTmpl::Exact))
                        .collect::<Res2<_>>()?
                };
                let args = self.tmpls(subs)?;
                self.b.tmpl_app(op, &gt, &[], &args).map_err(err)
            }
        }
    }

    fn tmpls(&mut self, ts: &[Term<'static>]) -> Res2<Vec<TmplId>> {
        ts.iter().map(|t| self.tmpl(t)).collect()
    }
}
