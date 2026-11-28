//! Lowering the declarations of the IR into a [`Signature`].
//!
//! * `\sorts`: one [`SortDecl`] per declaration; `\top` and `\formula` give the special kinds.
//!   `\meta` sorts are not sorts of `dale-logic`: an argument of a meta sort becomes a *meta
//!   argument* of the operator ([`MetaKind::Nat`]; the only meta kind so far). Sorts are added
//!   in dependency order (a sort after the sorts it extends or mentions), since
//!   [`SignatureBuilder`] only accepts references to earlier sorts.
//! * `\operators`, `\functions`, `\predicates`: one [`OpDecl`] each; predicates have the formula
//!   sort as result. `{S: 0, 1}` binds a variable of sort `S` in the arguments 0 and 1
//!   (`all<S>{S: 0}(Formula)`).
//! * The function marked `\boundVar` is the bound variable operator (`\boundVar var<S>(NUMBER)
//!   -> S`); the signature checks its shape. At most one function may be marked.
//!
//! All theories of the file end up in one signature, in the order of the file.

use std::collections::{HashMap, HashSet};

use dale_logic::{
    Binding, GenArgPat, GenKind, GenParam, MetaKind, OpDecl, OpDeclId, OpFlags, OpKind, Signature,
    SignatureBuilder, SortDecl, SortDeclId, SortKind, SortPat,
};
use dale_syn::Parsed;
use dale_syn::ir::{
    self, DefId, GenericArg, GenericParam, GenericParamKind, ItemKind, Res, SortRef, Span, TermKind,
};
use dale_util::symbol::Symbol;

use crate::Diag;

pub(crate) struct Lowered {
    pub sig: Signature,
    pub sorts: HashMap<DefId, SortDeclId>,
    pub ops: HashMap<DefId, OpDeclId>,
}

/// The declarations of all theories, in file order.
pub(crate) fn items(parsed: &Parsed) -> Vec<&'static ir::Item<'static>> {
    let map = parsed.map;
    map.file
        .theories()
        .iter()
        .flat_map(|t| t.items.iter().map(|id| map.items[id]))
        .collect()
}

pub(crate) fn lower_signature(parsed: &Parsed) -> Result<Lowered, Vec<Diag>> {
    let mut l = SigLower {
        b: SignatureBuilder::new(),
        decls: HashMap::new(),
        order: Vec::new(),
        sorts: HashMap::new(),
        meta: HashSet::new(),
        visiting: HashSet::new(),
        diags: Vec::new(),
        name: |s| parsed.interner.get(s).to_owned(),
    };
    let items = items(parsed);
    for item in &items {
        if let ItemKind::Sorts(ds) = item.kind {
            for d in ds {
                l.decls.insert(d.id, *d);
                l.order.push(d.id);
                if d.modifiers.meta {
                    l.meta.insert(d.id);
                }
            }
        }
    }
    for id in l.order.clone() {
        l.sort(id);
    }
    let mut ops = HashMap::new();
    let mut bound_var: Option<ir::Ident> = None;
    for item in &items {
        let decls: Vec<OpLike> = match item.kind {
            ItemKind::Operators(ds) => ds
                .iter()
                .map(|d| OpLike {
                    id: d.id,
                    name: d.name,
                    flags: OpFlags {
                        rigid: d.rigid,
                        ..Default::default()
                    },
                    bound_var: false,
                    params: d.params,
                    bind: d.bind,
                    args: d.arg_sort_refs,
                    result: Some(d.sort_ref),
                })
                .collect(),
            ItemKind::Functions(ds) => ds
                .iter()
                .map(|d| OpLike {
                    id: d.id,
                    name: d.name,
                    flags: OpFlags {
                        rigid: d.modifiers.rigid,
                        unique: d.modifiers.unique,
                        skolem: d.modifiers.skolem,
                    },
                    bound_var: d.modifiers.bound_var,
                    params: d.params,
                    bind: d.bind,
                    args: d.arg_sort_refs,
                    result: Some(d.sort_ref),
                })
                .collect(),
            ItemKind::Predicates(ds) => ds
                .iter()
                .map(|d| OpLike {
                    id: d.id,
                    name: d.name,
                    flags: OpFlags {
                        rigid: d.rigid,
                        ..Default::default()
                    },
                    bound_var: false,
                    params: d.params,
                    bind: d.bind,
                    args: d.arg_sort_refs,
                    result: None,
                })
                .collect(),
            _ => continue,
        };
        for d in decls {
            if d.bound_var {
                if let Some(first) = bound_var {
                    l.diags.push(Diag::new(
                        Some(d.name.span),
                        format!(
                            "`{}` is marked \\boundVar, but `{}` already is",
                            (l.name)(d.name.name),
                            (l.name)(first.name)
                        ),
                    ));
                }
                bound_var.get_or_insert(d.name);
            }
            if let Some(op) = l.op(&d) {
                match l.b.add_op(op) {
                    Ok(id) => {
                        ops.insert(d.id, id);
                    }
                    Err(e) => l.diags.push(Diag::new(
                        Some(d.name.span),
                        format!("operator `{}`: {e:?}", (l.name)(d.name.name)),
                    )),
                }
            }
        }
    }
    if !l.diags.is_empty() {
        return Err(l.diags);
    }
    match l.b.finish() {
        Ok(sig) => Ok(Lowered {
            sig,
            sorts: l.sorts,
            ops,
        }),
        Err(e) => Err(vec![Diag::new(None, format!("signature: {e:?}"))]),
    }
}

struct OpLike<'ir> {
    id: DefId,
    name: ir::Ident,
    flags: OpFlags,
    bound_var: bool,
    params: &'ir [GenericParam<'ir>],
    bind: Option<ir::BindSpec<'ir>>,
    args: &'ir [SortRef<'ir>],
    result: Option<&'ir SortRef<'ir>>,
}

struct SigLower<'ir, N: Fn(Symbol) -> String> {
    b: SignatureBuilder,
    decls: HashMap<DefId, ir::SortDecl<'ir>>,
    order: Vec<DefId>,
    sorts: HashMap<DefId, SortDeclId>,
    meta: HashSet<DefId>,
    visiting: HashSet<DefId>,
    diags: Vec<Diag>,
    name: N,
}

impl<'ir, N: Fn(Symbol) -> String> SigLower<'ir, N> {
    /// Adds the sort declaration `id` (after its dependencies) unless it was added before.
    /// `None` for meta sorts and failed declarations (already reported).
    fn sort(&mut self, id: DefId) -> Option<SortDeclId> {
        if let Some(s) = self.sorts.get(&id) {
            return Some(*s);
        }
        if self.meta.contains(&id) {
            return None;
        }
        let d = *self.decls.get(&id)?;
        if !self.visiting.insert(id) {
            self.diags.push(Diag::new(
                Some(d.span),
                format!("sort `{}` depends on itself", (self.name)(d.name.name)),
            ));
            return None;
        }
        let params = self.params(d.params);
        let extends = match d.extends {
            // an error is reported by `sort_pat`; go on without the supersort
            Some((_, r)) => self.sort_pat(r, d.params),
            None => None,
        };
        self.visiting.remove(&id);
        let mut decl = SortDecl::new(d.name.name);
        decl.kind = if d.modifiers.top {
            SortKind::Top
        } else if d.modifiers.formula {
            SortKind::Formula
        } else {
            SortKind::Normal
        };
        decl.params = params?;
        decl.extends = extends;
        match self.b.add_sort(decl) {
            Ok(s) => {
                self.sorts.insert(id, s);
                Some(s)
            }
            Err(e) => {
                self.diags.push(Diag::new(
                    Some(d.span),
                    format!("sort `{}`: {e:?}", (self.name)(d.name.name)),
                ));
                None
            }
        }
    }

    fn params(&mut self, params: &'ir [GenericParam<'ir>]) -> Option<Vec<GenParam>> {
        let mut out = Vec::with_capacity(params.len());
        for p in params {
            let kind = match p.kind {
                GenericParamKind::Sort => GenKind::Sort,
                GenericParamKind::Const { sort, .. } => {
                    GenKind::Const(self.sort_pat(sort, params)?)
                }
            };
            out.push(GenParam {
                name: p.name.name,
                kind,
            });
        }
        Some(out)
    }

    fn param_index(params: &[GenericParam<'_>], def: DefId) -> Option<(u32, bool)> {
        params
            .iter()
            .position(|p| p.id == def)
            .map(|i| (i as u32, matches!(params[i].kind, GenericParamKind::Sort)))
    }

    /// A sort reference in a declaration with the generic parameters `params`.
    fn sort_pat(&mut self, r: &SortRef<'ir>, params: &[GenericParam<'ir>]) -> Option<SortPat> {
        let Res::Def(_, def) = r.path.res else {
            return self.err(r.span, "unresolved sort");
        };
        if let Some((i, is_sort)) = Self::param_index(params, def) {
            if !is_sort || !r.args.is_empty() {
                return self.err(r.span, "a const parameter is not a sort");
            }
            return Some(SortPat::Param(i));
        }
        if self.meta.contains(&def) {
            return self.err(
                r.span,
                "a meta sort can only be the sort of an operator argument",
            );
        }
        let decl = self.sort(def)?;
        let mut args = Vec::with_capacity(r.args.len());
        for a in r.args {
            match a {
                GenericArg::Sort(s) => args.push(GenArgPat::Sort(self.sort_pat(s, params)?)),
                GenericArg::Const(t, span) => {
                    let TermKind::Path(p, _) = t.kind else {
                        return self.err(*span, "only const parameters are supported here");
                    };
                    let Res::Def(_, d) = p.res else {
                        return self.err(*span, "unresolved const argument");
                    };
                    match Self::param_index(params, d) {
                        Some((i, false)) => args.push(GenArgPat::Const(i)),
                        _ => return self.err(*span, "only const parameters are supported here"),
                    }
                }
            }
        }
        Some(SortPat::Inst { decl, args })
    }

    fn is_meta(&self, r: &SortRef<'_>) -> bool {
        matches!(r.path.res, Res::Def(_, d) if self.meta.contains(&d))
    }

    fn op(&mut self, d: &OpLike<'ir>) -> Option<OpDecl> {
        let generics = self.params(d.params)?;
        let mut meta = Vec::new();
        let mut args = Vec::new();
        for a in d.args {
            if self.is_meta(a) {
                if !args.is_empty() {
                    // meta arguments are not positional in dale-logic; keep them in front so
                    // that argument numbers (and `{i}`) mean the same on both sides
                    return self.err(
                        a.span,
                        "meta arguments must come before all other arguments",
                    );
                }
                meta.push(MetaKind::Nat);
            } else {
                args.push(self.sort_pat(a, d.params)?);
            }
        }
        let result = match d.result {
            Some(r) => self.sort_pat(r, d.params)?,
            None => match self.b.formula_pat() {
                Ok(p) => p,
                Err(_) => return self.err(d.name.span, "predicate without a \\formula sort"),
            },
        };
        let bind = match d.bind {
            None => None,
            Some(b) => {
                let var_sort = self.sort_pat(b.sort, d.params)?;
                let mut args = Vec::with_capacity(b.args.len());
                for &i in b.args {
                    // positions count all declared arguments; meta arguments come first
                    match (i as u32).checked_sub(meta.len() as u32) {
                        Some(a) => args.push(a),
                        None => return self.err(b.span, "a meta argument cannot be bound"),
                    }
                }
                Some(Binding { args, var_sort })
            }
        };
        let mut op = OpDecl::new(d.name.name, args, result);
        op.kind = if d.bound_var {
            OpKind::BoundVar
        } else {
            OpKind::Normal
        };
        op.flags = d.flags;
        op.generics = generics;
        op.meta = meta;
        op.bind = bind;
        Some(op)
    }

    fn err<T>(&mut self, span: Span, msg: &str) -> Option<T> {
        self.diags.push(Diag::new(Some(span), msg));
        None
    }
}
