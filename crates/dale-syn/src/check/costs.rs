//! Type and stage checking of the cost language: `\costs` items, the bodies of `\rulesets`
//! declarations, and the arguments of `\rulesets(...)` references in rules.
//!
//! Every expression gets a type ([`CTy`]) and a [`Stage`] (the information it needs). The stage of
//! a ruleset body is compared against its `\stage(...)` assertion.
//!
//! About branch pruning: an `if` whose condition is a *compile-time constant* (built from
//! literals, `Cost::*` bands and `const`s) only counts the selected branch. A condition that is
//! merely taclet-level (`branch_count > 1`, say) is only known per rule, not per declaration; for
//! those the declaration-level stage is the join of both branches, i.e. an upper bound.

use std::collections::HashSet;

use crate::{
    check::{CheckErr, Checker, Sort},
    cost::{self, Stage, Ty},
    ir::{
        BinOp, CallArg, CostItem, CostItemKind, CostParam, CostTy, Def, DefId, DefKind, Expr,
        ExprKind, Path, Res, RuleSetDecl, RuleSetRef, UnOp,
    },
};

/// Types of the cost language, with the sort of a term where it is known.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CTy<'ir> {
    Cost,
    Int,
    Float,
    Bool,
    Op,
    Sort,
    /// A generator; only allowed as the generator of a binder.
    Gen,
    /// A term; `None` if the sort is not statically known (e.g. the `find` term).
    Term(Option<Sort<'ir>>),
    /// An already reported error; compatible with everything to avoid follow-up errors.
    Err,
}

impl<'ir> CTy<'ir> {
    fn kind(self) -> Ty {
        match self {
            CTy::Cost => Ty::Cost,
            CTy::Int => Ty::Int,
            CTy::Float => Ty::Float,
            CTy::Bool => Ty::Bool,
            CTy::Op => Ty::Op,
            CTy::Sort => Ty::Sort,
            CTy::Gen => Ty::Gen,
            CTy::Term(_) => Ty::Term,
            CTy::Err => Ty::Any,
        }
    }

    fn numeric(self) -> bool {
        matches!(self, CTy::Cost | CTy::Int | CTy::Float)
    }

    fn from_builtin(t: Ty) -> CTy<'ir> {
        match t {
            Ty::Cost => CTy::Cost,
            Ty::Int => CTy::Int,
            Ty::Float => CTy::Float,
            Ty::Bool => CTy::Bool,
            Ty::Term => CTy::Term(None),
            Ty::Op => CTy::Op,
            Ty::Sort => CTy::Sort,
            Ty::Gen => CTy::Gen,
            Ty::Any => CTy::Err,
        }
    }
}

/// Whether a value of type `from` can be used where `to` is expected. Integers are usable as costs
/// and floats; terms are usable where a term of a super sort is expected (a term of unknown sort
/// is accepted anywhere).
fn assignable(from: CTy<'_>, to: CTy<'_>) -> bool {
    match (from, to) {
        (CTy::Err, _) | (_, CTy::Err) => true,
        (CTy::Int, CTy::Cost | CTy::Float) => true,
        (CTy::Term(a), CTy::Term(b)) => match (a, b) {
            (Some(a), Some(b)) => a.sub_sort(&b),
            _ => true,
        },
        (a, b) => a == b,
    }
}

fn numeric_join<'ir>(a: CTy<'ir>, b: CTy<'ir>) -> CTy<'ir> {
    if a == CTy::Cost || b == CTy::Cost {
        CTy::Cost
    } else if a == CTy::Float || b == CTy::Float {
        CTy::Float
    } else {
        CTy::Int
    }
}

/// A value computed at compile time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Const {
    Int(i64),
    Bool(bool),
}

/// State of the cost checker, part of [`Checker`].
#[derive(Default)]
pub(super) struct CostState<'ir> {
    locals: std::collections::HashMap<DefId, CTy<'ir>>,
    ty_cache: std::collections::HashMap<crate::ir::IrId, CTy<'ir>>,
    fn_stages: std::collections::HashMap<DefId, Stage>,
    fn_active: HashSet<DefId>,
}

impl<'ir> Checker<'ir> {
    fn name(&self, sym: dale_util::symbol::Symbol) -> &'ir str {
        self.interner.get(sym)
    }

    // ------------------------------------------------------------------------------------------
    // Types
    // ------------------------------------------------------------------------------------------

    fn cost_ty(&mut self, t: &CostTy<'ir>) -> CTy<'ir> {
        if let Some(c) = self.cost.ty_cache.get(&t.id) {
            return *c;
        }
        let base = match self.name(t.name.name) {
            "Cost" => Some(CTy::Cost),
            "Int" => Some(CTy::Int),
            "Float" => Some(CTy::Float),
            "Bool" => Some(CTy::Bool),
            "Op" => Some(CTy::Op),
            "Sort" => Some(CTy::Sort),
            "Term" => Some(CTy::Term(None)),
            _ => None,
        };
        let result = match (base, t.arg) {
            (None, _) => {
                self.errors.push(CheckErr::UnknownCostType(t.id));
                CTy::Err
            }
            (Some(CTy::Term(_)), Some(s)) => CTy::Term(Some(self.ref_to_sort(s))),
            (Some(b), Some(_)) => {
                self.errors.push(CheckErr::CostTypeArgNotAllowed(t.id));
                b
            }
            (Some(b), None) => b,
        };
        self.cost.ty_cache.insert(t.id, result);
        result
    }

    fn expect(&mut self, id: crate::ir::IrId, found: CTy<'ir>, expected: CTy<'ir>) {
        if assignable(found, expected) {
            return;
        }
        if let (CTy::Term(_), CTy::Term(_)) = (found, expected) {
            self.errors.push(CheckErr::CostTermSortMismatch(id));
        } else {
            self.errors.push(CheckErr::CostTypeMismatch(
                id,
                expected.kind(),
                found.kind(),
            ));
        }
    }

    // ------------------------------------------------------------------------------------------
    // Items
    // ------------------------------------------------------------------------------------------

    pub(super) fn check_cost_item(&mut self, x: &'ir CostItem<'ir>) {
        match x.kind {
            CostItemKind::Const { ty, value } => {
                let want = self.cost_ty(&ty);
                let (t, s) = self.cost_expr(value);
                self.expect(value.id, t, want);
                if !s.le(Stage::TACLET) {
                    self.errors
                        .push(CheckErr::CostConstNotConstant(value.id, s));
                }
            }
            CostItemKind::Fn { body, .. } => {
                self.check_cost_fn(x.id, body.id);
            }
        }
    }

    /// Checks the body of a cost function (once) and returns the stage it needs.
    fn check_cost_fn(&mut self, id: DefId, at: crate::ir::IrId) -> Stage {
        if let Some(s) = self.cost.fn_stages.get(&id) {
            return *s;
        }
        if !self.cost.fn_active.insert(id) {
            self.errors.push(CheckErr::CostRecursion(at));
            return Stage::TACLET;
        }
        let Def::CostItem(item) = self.map.get_def(id) else {
            self.cost.fn_active.remove(&id);
            return Stage::TACLET;
        };
        let stage = match item.kind {
            CostItemKind::Fn { params, ret, body } => {
                for p in params {
                    self.cost_ty(&p.ty);
                }
                let want = self.cost_ty(&ret);
                let (t, s) = self.cost_expr(body);
                self.expect(body.id, t, want);
                s
            }
            CostItemKind::Const { .. } => Stage::TACLET,
        };
        self.cost.fn_active.remove(&id);
        self.cost.fn_stages.insert(id, stage);
        stage
    }

    pub(super) fn check_rule_set_decl(&mut self, x: &'ir RuleSetDecl<'ir>) {
        let param_tys: Vec<CTy<'ir>> = x.params.iter().map(|p| self.cost_ty(&p.ty)).collect();
        let (t, s) = self.cost_expr(x.body);
        self.expect(x.body.id, t, CTy::Cost);

        if let Some((_, asserted)) = x.stage {
            if !s.le(asserted) {
                self.errors
                    .push(CheckErr::StageExceedsAssertion(x.id, asserted, s));
            } else if s != asserted {
                self.errors
                    .push(CheckErr::StageAssertionStronger(x.id, asserted, s));
            }
        }

        // `extend` / `override` must keep the parameter list of the ruleset they change.
        if let Some(Res::Def(DefKind::RuleSet, base_id)) = x.base {
            if let Some(base) = self.rule_sets.get(&base_id).copied() {
                let base_tys: Vec<CTy<'ir>> =
                    base.params.iter().map(|p| self.cost_ty(&p.ty)).collect();
                if base_tys != param_tys {
                    self.errors
                        .push(CheckErr::ExtendParamMismatch(x.id, base_id));
                }
            }
        }
    }

    /// Checks the arguments of a `\rulesets(name(args))` reference against the parameters of the
    /// ruleset: the number of arguments, and for each the type (for terms: the sort).
    pub(super) fn check_rule_set_ref(&mut self, x: &'ir RuleSetRef<'ir>) {
        let arg_tys: Vec<CTy<'ir>> = x.args.iter().map(|a| self.cost_expr(a).0).collect();
        let Res::Def(DefKind::RuleSet, id) = x.path.res else {
            return;
        };
        let Some(decl) = self.rule_sets.get(&id).copied() else {
            return;
        };
        if arg_tys.len() != decl.params.len() {
            self.errors.push(CheckErr::RuleSetArity(
                x.id,
                id,
                decl.params.len(),
                arg_tys.len(),
            ));
            return;
        }
        for (idx, (found, p)) in arg_tys.iter().zip(decl.params.iter()).enumerate() {
            let want = self.cost_ty(&p.ty);
            if !assignable(*found, want) {
                self.errors
                    .push(CheckErr::RuleSetArgMismatch(x.id, id, idx));
            }
        }
    }

    // ------------------------------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------------------------------

    fn cost_expr(&mut self, e: &'ir Expr<'ir>) -> (CTy<'ir>, Stage) {
        match e.kind {
            ExprKind::Int(_) => (CTy::Int, Stage::TACLET),
            ExprKind::Float(_) => (CTy::Float, Stage::TACLET),
            ExprKind::Top => (CTy::Cost, Stage::TACLET),
            ExprKind::Path(p) => self.cost_path(e, p),
            ExprKind::Call(p, args) => self.cost_call(e, p, args),
            ExprKind::Unary(op, x) => {
                let (t, s) = self.cost_expr(x);
                match op {
                    UnOp::Neg => {
                        if !t.numeric() && t != CTy::Err {
                            self.expect(x.id, t, CTy::Cost);
                        }
                        (t, s)
                    }
                    UnOp::Not => {
                        self.expect(x.id, t, CTy::Bool);
                        (CTy::Bool, s)
                    }
                }
            }
            ExprKind::Binary(op, a, b) => {
                let (ta, sa) = self.cost_expr(a);
                let (tb, sb) = self.cost_expr(b);
                let s = sa.join(sb);
                let ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul => {
                        self.expect_numeric(a.id, ta);
                        self.expect_numeric(b.id, tb);
                        if ta == CTy::Err || tb == CTy::Err {
                            CTy::Err
                        } else {
                            numeric_join(ta, tb)
                        }
                    }
                    BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt => {
                        self.expect_numeric(a.id, ta);
                        self.expect_numeric(b.id, tb);
                        CTy::Bool
                    }
                    BinOp::Eq | BinOp::Ne => {
                        let ok = match (ta, tb) {
                            (CTy::Err, _) | (_, CTy::Err) => true,
                            (a, b) if a.numeric() && b.numeric() => true,
                            (CTy::Bool, CTy::Bool) | (CTy::Op, CTy::Op) => true,
                            _ => false,
                        };
                        if !ok {
                            self.errors.push(CheckErr::CostTypeMismatch(
                                b.id,
                                ta.kind(),
                                tb.kind(),
                            ));
                        }
                        CTy::Bool
                    }
                    BinOp::And | BinOp::Or => {
                        self.expect(a.id, ta, CTy::Bool);
                        self.expect(b.id, tb, CTy::Bool);
                        CTy::Bool
                    }
                };
                (ty, s)
            }
            ExprKind::If(c, a, b) => {
                let (tc, sc) = self.cost_expr(c);
                self.expect(c.id, tc, CTy::Bool);
                let (ta, sa) = self.cost_expr(a);
                let (tb, sb) = self.cost_expr(b);
                let ty = if assignable(ta, tb) {
                    tb
                } else if assignable(tb, ta) {
                    ta
                } else {
                    self.errors
                        .push(CheckErr::CostTypeMismatch(b.id, ta.kind(), tb.kind()));
                    CTy::Err
                };
                // A compile-time constant condition selects a branch; only that one counts.
                let branches = match self.const_eval(c, 0) {
                    Some(Const::Bool(true)) => sa,
                    Some(Const::Bool(false)) => sb,
                    _ => sa.join(sb),
                };
                (ty, sc.join(branches))
            }
            ExprKind::Let {
                def, value, body, ..
            } => {
                let (tv, sv) = self.cost_expr(value);
                let tv = if tv == CTy::Gen {
                    self.errors.push(CheckErr::CostGeneratorMisuse(value.id));
                    CTy::Err
                } else {
                    tv
                };
                self.cost.locals.insert(def, tv);
                let (tb, sb) = self.cost_expr(body);
                (tb, sv.join(sb))
            }
        }
    }

    fn expect_numeric(&mut self, id: crate::ir::IrId, t: CTy<'ir>) {
        if !t.numeric() && t != CTy::Err {
            self.errors
                .push(CheckErr::CostTypeMismatch(id, Ty::Cost, t.kind()));
        }
    }

    fn cost_path(&mut self, e: &'ir Expr<'ir>, p: &'ir Path<'ir>) -> (CTy<'ir>, Stage) {
        match p.res {
            Res::Builtin(sym) => {
                let name = self.name(sym);
                if let Some(b) = cost::builtin(name) {
                    if b.params.is_empty() && !is_binder(name) {
                        (CTy::from_builtin(b.ret), b.stage)
                    } else {
                        self.errors
                            .push(CheckErr::CostArity(e.id, b.params.len(), 0));
                        (CTy::Err, Stage::TACLET)
                    }
                } else if cost::cost_const(name).is_some() {
                    (CTy::Cost, Stage::TACLET)
                } else {
                    (CTy::Err, Stage::TACLET)
                }
            }
            Res::Def(DefKind::CostConst, id) => {
                let ty = match self.map.get_def(id) {
                    Def::CostItem(CostItem {
                        kind: CostItemKind::Const { ty, .. },
                        ..
                    }) => self.cost_ty(&ty),
                    _ => CTy::Err,
                };
                (ty, Stage::TACLET)
            }
            Res::Def(DefKind::CostParam, id) => {
                let ty = match self.map.get_def(id) {
                    Def::CostParam(CostParam { ty, .. }) => self.cost_ty(&ty),
                    _ => CTy::Err,
                };
                (ty, Stage::TACLET)
            }
            Res::Def(DefKind::CostLocal, id) => (
                self.cost.locals.get(&id).copied().unwrap_or(CTy::Err),
                Stage::TACLET,
            ),
            Res::Def(DefKind::CostFn, _) => {
                self.errors.push(CheckErr::CostNotAValue(e.id));
                (CTy::Err, Stage::TACLET)
            }
            Res::Def(DefKind::SchemaVar, id) => {
                let ty = match self.map.get_def(id) {
                    Def::SchemaVar(sv) => CTy::Term(Some(self.ref_to_sort(sv.sort))),
                    _ => CTy::Err,
                };
                (ty, Stage::TACLET)
            }
            Res::Def(DefKind::Operator | DefKind::Fn | DefKind::Pred, _) => {
                (CTy::Op, Stage::TACLET)
            }
            _ => (CTy::Err, Stage::TACLET),
        }
    }

    fn cost_call(
        &mut self,
        e: &'ir Expr<'ir>,
        p: &'ir Path<'ir>,
        args: &'ir [CallArg<'ir>],
    ) -> (CTy<'ir>, Stage) {
        match p.res {
            Res::Builtin(sym) => {
                let name = self.name(sym).to_string();
                let Some(b) = cost::builtin(&name) else {
                    self.errors.push(CheckErr::CostNotCallable(e.id));
                    let s = self.cost_args_unchecked(e, args);
                    return (CTy::Err, s);
                };
                let ret = CTy::from_builtin(b.ret);
                if is_binder(&name) {
                    return self.cost_binder_call(e, &name, b.stage, ret, args);
                }
                if args.len() != b.params.len() {
                    self.errors
                        .push(CheckErr::CostArity(e.id, b.params.len(), args.len()));
                }
                let mut stage = b.stage;
                for (idx, a) in args.iter().enumerate() {
                    let CallArg::Expr(ex) = a else {
                        self.errors.push(CheckErr::CostBinderMisuse(e.id));
                        continue;
                    };
                    let (t, s) = self.cost_expr(ex);
                    stage = stage.join(s);
                    match b.params.get(idx) {
                        Some(Ty::Any) => {
                            if t == CTy::Gen {
                                self.errors.push(CheckErr::CostGeneratorMisuse(ex.id));
                            }
                        }
                        Some(want) => self.expect(ex.id, t, CTy::from_builtin(*want)),
                        None => {}
                    }
                }
                (ret, stage)
            }
            Res::Def(DefKind::CostFn, id) => {
                let Def::CostItem(item) = self.map.get_def(id) else {
                    return (CTy::Err, Stage::TACLET);
                };
                let CostItemKind::Fn { params, ret, .. } = item.kind else {
                    return (CTy::Err, Stage::TACLET);
                };
                if args.len() != params.len() {
                    self.errors
                        .push(CheckErr::CostArity(e.id, params.len(), args.len()));
                }
                let mut stage = Stage::TACLET;
                for (idx, a) in args.iter().enumerate() {
                    let CallArg::Expr(ex) = a else {
                        self.errors.push(CheckErr::CostBinderMisuse(e.id));
                        continue;
                    };
                    let (t, s) = self.cost_expr(ex);
                    stage = stage.join(s);
                    if let Some(param) = params.get(idx) {
                        let want = self.cost_ty(&param.ty);
                        self.expect(ex.id, t, want);
                    }
                }
                let fn_stage = self.check_cost_fn(id, e.id);
                let ret = self.cost_ty(&ret);
                (ret, stage.join(fn_stage))
            }
            Res::Err => {
                let s = self.cost_args_unchecked(e, args);
                (CTy::Err, s)
            }
            _ => {
                self.errors.push(CheckErr::CostNotCallable(e.id));
                let s = self.cost_args_unchecked(e, args);
                (CTy::Err, s)
            }
        }
    }

    /// Checks the arguments of a call whose callee is invalid, only to find errors inside them.
    fn cost_args_unchecked(&mut self, e: &'ir Expr<'ir>, args: &'ir [CallArg<'ir>]) -> Stage {
        let mut stage = Stage::TACLET;
        for a in args {
            match a {
                CallArg::Expr(ex) => stage = stage.join(self.cost_expr(ex).1),
                CallArg::Binder(_) => self.errors.push(CheckErr::CostBinderMisuse(e.id)),
            }
        }
        stage
    }

    /// `sum(x in gen: cost)`, `any(x in gen: bool)`, `all(x in gen: bool)`.
    fn cost_binder_call(
        &mut self,
        e: &'ir Expr<'ir>,
        name: &str,
        builtin_stage: Stage,
        ret: CTy<'ir>,
        args: &'ir [CallArg<'ir>],
    ) -> (CTy<'ir>, Stage) {
        let [CallArg::Binder(bd)] = args else {
            self.errors.push(CheckErr::CostBinderMisuse(e.id));
            let s = self.cost_args_unchecked(e, args);
            return (ret, builtin_stage.join(s));
        };
        let (tg, sg) = self.cost_expr(bd.generator);
        if tg != CTy::Gen && tg != CTy::Err {
            self.errors.push(CheckErr::CostTypeMismatch(
                bd.generator.id,
                Ty::Gen,
                tg.kind(),
            ));
        }
        self.cost.locals.insert(bd.def, CTy::Term(None));
        let (tb, sb) = self.cost_expr(bd.body);
        let want = if name == "sum" { CTy::Cost } else { CTy::Bool };
        self.expect(bd.body.id, tb, want);
        (ret, builtin_stage.join(sg).join(sb))
    }

    // ------------------------------------------------------------------------------------------
    // Compile-time evaluation (only used to prune `if` branches)
    // ------------------------------------------------------------------------------------------

    fn const_eval(&self, e: &Expr<'ir>, depth: u8) -> Option<Const> {
        if depth > 16 {
            return None;
        }
        match e.kind {
            ExprKind::Int(i) => Some(Const::Int(i)),
            ExprKind::Path(p) => match p.res {
                Res::Builtin(sym) => {
                    let name = self.interner.get(sym);
                    if cost::builtin(name).is_none() {
                        cost::cost_const(name).map(Const::Int)
                    } else {
                        None
                    }
                }
                Res::Def(DefKind::CostConst, id) => match self.map.get_def(id) {
                    Def::CostItem(CostItem {
                        kind: CostItemKind::Const { value, .. },
                        ..
                    }) => self.const_eval(value, depth + 1),
                    _ => None,
                },
                _ => None,
            },
            ExprKind::Unary(UnOp::Neg, x) => match self.const_eval(x, depth + 1)? {
                Const::Int(i) => i.checked_neg().map(Const::Int),
                Const::Bool(_) => None,
            },
            ExprKind::Unary(UnOp::Not, x) => match self.const_eval(x, depth + 1)? {
                Const::Bool(b) => Some(Const::Bool(!b)),
                Const::Int(_) => None,
            },
            ExprKind::Binary(op, a, b) => {
                let a = self.const_eval(a, depth + 1)?;
                let b = self.const_eval(b, depth + 1)?;
                match (op, a, b) {
                    (BinOp::Add, Const::Int(a), Const::Int(b)) => a.checked_add(b).map(Const::Int),
                    (BinOp::Sub, Const::Int(a), Const::Int(b)) => a.checked_sub(b).map(Const::Int),
                    (BinOp::Mul, Const::Int(a), Const::Int(b)) => a.checked_mul(b).map(Const::Int),
                    (BinOp::Lt, Const::Int(a), Const::Int(b)) => Some(Const::Bool(a < b)),
                    (BinOp::Le, Const::Int(a), Const::Int(b)) => Some(Const::Bool(a <= b)),
                    (BinOp::Ge, Const::Int(a), Const::Int(b)) => Some(Const::Bool(a >= b)),
                    (BinOp::Gt, Const::Int(a), Const::Int(b)) => Some(Const::Bool(a > b)),
                    (BinOp::Eq, a, b)
                        if std::mem::discriminant(&a) == std::mem::discriminant(&b) =>
                    {
                        Some(Const::Bool(a == b))
                    }
                    (BinOp::Ne, a, b)
                        if std::mem::discriminant(&a) == std::mem::discriminant(&b) =>
                    {
                        Some(Const::Bool(a != b))
                    }
                    (BinOp::And, Const::Bool(a), Const::Bool(b)) => Some(Const::Bool(a && b)),
                    (BinOp::Or, Const::Bool(a), Const::Bool(b)) => Some(Const::Bool(a || b)),
                    _ => None,
                }
            }
            ExprKind::If(c, a, b) => match self.const_eval(c, depth + 1)? {
                Const::Bool(true) => self.const_eval(a, depth + 1),
                Const::Bool(false) => self.const_eval(b, depth + 1),
                Const::Int(_) => None,
            },
            _ => None,
        }
    }
}

/// The builtins whose argument is a binder (`x in generator: body`).
fn is_binder(name: &str) -> bool {
    matches!(name, "sum" | "any" | "all")
}
