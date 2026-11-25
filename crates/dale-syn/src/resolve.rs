use std::{collections::HashMap, fmt::Display, ops};

use dale_index::Idx;
use dale_util::symbol::Symbol;
use indexmap::IndexMap;

use crate::{
    cost::{self, Stage},
    ctx::Ctx,
    ir::{DefId, DefKind, LocalDefId, Res},
    raw_theory::{
        self as raw, File, Ident, NodeId, PathSegment, Span, UseTreeKind, Visit, visit_binder,
        visit_cost_item, visit_cost_param, visit_expr, visit_file, visit_function_decl,
        visit_generic_param, visit_item, visit_item_kind, visit_predicate_decl, visit_rule,
        visit_rule_set_decl, visit_schema_var_decl, visit_sort, visit_sort_decl, visit_term,
        visit_theory, visit_use_tree,
    },
};

pub fn resolve<'cx>(cx: Ctx<'cx>, file: &File) -> Resolver {
    let mut r = Resolver::new();
    collect_defs(&mut r, file);
    let mut sb = ScopeBuilder::new(cx, &mut r);
    sb.visit_file(file);
    r
}

#[derive(Debug, Clone)]
pub enum ResolveErr {
    DuplicateName(Symbol, Span),
    NotFound(Symbol, Namespace, Span),
    NoRes(Span),
    InvalidStage(Symbol, Span),
}

impl ResolveErr {
    pub fn format<'cx>(&self, cx: Ctx<'cx>) -> String {
        match self {
            ResolveErr::DuplicateName(symbol, _) => {
                format!(
                    "Already an element named {} in namespace",
                    cx.interner.get(*symbol)
                )
            }
            ResolveErr::NotFound(sym, ns, ..) => {
                format!(
                    "Failed to resolve symbol {} in {} namespace",
                    cx.interner.get(*sym),
                    ns
                )
            }
            ResolveErr::NoRes(_) => "No resolution found".to_string(),
            ResolveErr::InvalidStage(sym, _) => {
                format!(
                    "Unknown stage {} (expected one of taclet, app, formula, sequent, history)",
                    cx.interner.get(*sym)
                )
            }
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            ResolveErr::DuplicateName(_, span) => span,
            ResolveErr::NotFound(_, _, span) => span,
            ResolveErr::NoRes(span) => span,
            ResolveErr::InvalidStage(_, span) => span,
        }
    }
}

pub struct Scope<R = Res> {
    pub bindings: IndexMap<Symbol, R>,
    pub kind: ScopeKind,
}

impl<R> Scope<R> {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            bindings: IndexMap::default(),
            kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Use,
    Normal,
    File,
    Theory,
    PossiblyGenericDecl,
    Rule,
}

#[derive(Debug, Clone, Copy)]
pub enum Namespace {
    OpNS,
    TheoryNS,
    RuleNS,
    RuleSetNS,
    SortNS,
    /// Constants, functions, parameters and locals of the cost language.
    CostNS,
}

impl Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Namespace::OpNS => "operator",
            Namespace::TheoryNS => "theory",
            Namespace::RuleNS => "rule",
            Namespace::RuleSetNS => "rule sets",
            Namespace::SortNS => "sort",
            Namespace::CostNS => "cost",
        };
        Display::fmt(s, f)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PerNS<T> {
    pub op_ns: T,
    pub theory_ns: T,
    pub rule_ns: T,
    pub rule_set_ns: T,
    pub sort_ns: T,
    pub cost_ns: T,
}

impl<T> ops::Index<Namespace> for PerNS<T> {
    type Output = T;

    fn index(&self, index: Namespace) -> &Self::Output {
        match index {
            Namespace::OpNS => &self.op_ns,
            Namespace::TheoryNS => &self.theory_ns,
            Namespace::RuleNS => &self.rule_ns,
            Namespace::RuleSetNS => &self.rule_set_ns,
            Namespace::SortNS => &self.sort_ns,
            Namespace::CostNS => &self.cost_ns,
        }
    }
}

impl<T> ops::IndexMut<Namespace> for PerNS<T> {
    fn index_mut(&mut self, index: Namespace) -> &mut Self::Output {
        match index {
            Namespace::OpNS => &mut self.op_ns,
            Namespace::TheoryNS => &mut self.theory_ns,
            Namespace::RuleNS => &mut self.rule_ns,
            Namespace::RuleSetNS => &mut self.rule_set_ns,
            Namespace::SortNS => &mut self.sort_ns,
            Namespace::CostNS => &mut self.cost_ns,
        }
    }
}

impl<T> PerNS<T> {
    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> PerNS<U> {
        PerNS {
            op_ns: f(self.op_ns),
            theory_ns: f(self.theory_ns),
            rule_ns: f(self.rule_ns),
            rule_set_ns: f(self.rule_set_ns),
            sort_ns: f(self.sort_ns),
            cost_ns: f(self.cost_ns),
        }
    }
}

impl<T> Default for PerNS<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            op_ns: Default::default(),
            theory_ns: Default::default(),
            rule_ns: Default::default(),
            rule_set_ns: Default::default(),
            sort_ns: Default::default(),
            cost_ns: Default::default(),
        }
    }
}

impl<T> PerNS<Option<T>> {
    pub fn is_empty(&self) -> bool {
        self.op_ns.is_none()
            && self.theory_ns.is_none()
            && self.rule_ns.is_none()
            && self.rule_set_ns.is_none()
            && self.sort_ns.is_none()
            && self.cost_ns.is_none()
    }
}

pub struct Resolver {
    node_id_to_def_id: HashMap<NodeId, LocalDefId>,
    next_def_id: LocalDefId,
    resolved: HashMap<NodeId, Res>,
    import_res_map: HashMap<NodeId, PerNS<Option<Res>>>,
    pub errors: Vec<ResolveErr>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            node_id_to_def_id: HashMap::new(),
            next_def_id: LocalDefId::new(0),
            resolved: Default::default(),
            import_res_map: Default::default(),
            errors: Vec::new(),
        }
    }

    pub fn get_res(&self, id: NodeId) -> Option<Res> {
        self.resolved.get(&id).copied()
    }

    pub fn get_import_res(&self, id: NodeId) -> PerNS<Option<Res>> {
        self.import_res_map[&id]
    }

    pub fn def_id(&self, id: NodeId) -> DefId {
        self.node_id_to_def_id[&id].to_def_id()
    }
}

fn collect_defs(resolver: &mut Resolver, file: &File) {
    let mut coll = DefCollector::new(resolver);
    coll.visit_file(file);
}

struct TheoryData {
    scopes: PerNS<Scope>,
}

pub struct DefCollector<'a> {
    resolver: &'a mut Resolver,
    in_operator_mode: bool,
}

impl<'a> DefCollector<'a> {
    pub fn new(resolver: &'a mut Resolver) -> Self {
        Self {
            resolver,
            in_operator_mode: false,
        }
    }

    pub(crate) fn create_def(
        &mut self,
        node_id: NodeId,
        name: Option<Symbol>,
        def_kind: DefKind,
    ) -> LocalDefId {
        assert!(
            !self.resolver.node_id_to_def_id.contains_key(&node_id),
            "adding a def for node id {:?}, name {:?}, data {:?} but a previous def exists: {:?}",
            node_id,
            name,
            def_kind,
            self.resolver.node_id_to_def_id.get(&node_id)
        );

        let id = self.resolver.next_def_id;
        self.resolver.next_def_id.increment_by(1);
        self.resolver.node_id_to_def_id.insert(node_id, id);
        id
    }
}

impl<'a, 'ast> Visit<'ast> for DefCollector<'a> {
    fn visit_file(&mut self, x: &'ast crate::raw_theory::File) {
        self.create_def(x.id, None, DefKind::File);
        visit_file(self, x);
    }

    fn visit_theory(&mut self, x: &'ast crate::raw_theory::Theory) {
        self.create_def(x.id, Some(x.name.node), DefKind::Theory);
        visit_theory(self, x);
    }

    fn visit_item(&mut self, x: &'ast crate::raw_theory::Item) {
        if let crate::raw_theory::ItemKind::Use(_) = &x.kind {
            self.create_def(x.id, None, DefKind::Use);
        }
        if let raw::ItemKind::Operators(..) = &x.kind {
            self.in_operator_mode = true;
        }
        visit_item(self, x);
        self.in_operator_mode = false;
    }

    fn visit_use_tree(&mut self, x: &'ast crate::raw_theory::UseTree) {
        if let raw::UseTreeKind::Nested { items, .. } = &x.kind {
            for (_, id) in items {
                self.create_def(*id, None, DefKind::Use);
            }
        }
        visit_use_tree(self, x);
    }

    fn visit_function_decl(&mut self, x: &'ast raw::FunctionDecl) {
        self.create_def(
            x.id,
            Some(x.name.node),
            if self.in_operator_mode {
                DefKind::Operator
            } else {
                DefKind::Fn
            },
        );
        visit_function_decl(self, x);
    }

    fn visit_sort_decl(&mut self, x: &'ast raw::SortDecl) {
        self.create_def(x.id, Some(x.name.node), DefKind::Sort);
        visit_sort_decl(self, x);
    }

    fn visit_predicate_decl(&mut self, x: &'ast raw::PredicateDecl) {
        self.create_def(x.id, Some(x.name.node), DefKind::Pred);
        visit_predicate_decl(self, x);
    }

    fn visit_generic_param(&mut self, x: &'ast raw::GenericParam) {
        self.create_def(
            x.id,
            Some(x.name.node),
            match x.kind {
                raw::GenericParamKind::Sort => DefKind::SortParam,
                raw::GenericParamKind::Const { .. } => DefKind::ConstParam,
            },
        );
    }

    fn visit_rule(&mut self, x: &'ast raw::Rule) {
        self.create_def(x.id, Some(x.name.node), DefKind::Rule);
        visit_rule(self, x);
    }

    fn visit_schema_var_decl(&mut self, x: &'ast raw::SchemaVarDecl) {
        self.create_def(x.id, Some(x.name.node), DefKind::SchemaVar);
    }

    fn visit_rule_set_decl(&mut self, x: &'ast raw::RuleSetDecl) {
        self.create_def(x.id, Some(x.name.node), DefKind::RuleSet);
        visit_rule_set_decl(self, x);
    }

    fn visit_cost_item(&mut self, x: &'ast raw::CostItem) {
        let (name, kind) = match &x.kind {
            raw::CostItemKind::Const { name, .. } => (name.node, DefKind::CostConst),
            raw::CostItemKind::Fn { name, .. } => (name.node, DefKind::CostFn),
        };
        self.create_def(x.id, Some(name), kind);
        visit_cost_item(self, x);
    }

    fn visit_cost_param(&mut self, x: &'ast raw::CostParam) {
        self.create_def(x.id, Some(x.name.node), DefKind::CostParam);
        visit_cost_param(self, x);
    }

    fn visit_expr(&mut self, x: &'ast raw::Expr) {
        if let raw::ExprKind::Let(id, name, ..) = &x.kind {
            self.create_def(*id, Some(name.node), DefKind::CostLocal);
        }
        visit_expr(self, x);
    }

    fn visit_binder(&mut self, x: &'ast raw::Binder) {
        self.create_def(x.id, Some(x.var.node), DefKind::CostLocal);
        visit_binder(self, x);
    }
}

pub struct ScopeBuilder<'a, 'cx> {
    cx: Ctx<'cx>,
    scopes: PerNS<Vec<Scope>>,
    resolver: &'a mut Resolver,
    in_operator_context: bool,
    expected_ns: Namespace,
    theory_data: HashMap<DefId, TheoryData>,
    active_id: Option<NodeId>,
}

impl<'a, 'cx> ScopeBuilder<'a, 'cx> {
    pub fn new(cx: Ctx<'cx>, resolver: &'a mut Resolver) -> Self {
        Self {
            cx,
            scopes: PerNS::default(),
            resolver,
            in_operator_context: false,
            expected_ns: Namespace::TheoryNS,
            theory_data: Default::default(),
            active_id: None,
        }
    }

    fn with_scope<T>(
        &mut self,
        ns: Namespace,
        kind: ScopeKind,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.scopes[ns].push(Scope::new(kind));
        let r = f(self);
        self.scopes[ns].pop();
        r
    }

    fn with_generics<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes[Namespace::SortNS].push(Scope::new(ScopeKind::PossiblyGenericDecl));
        self.scopes[Namespace::OpNS].push(Scope::new(ScopeKind::PossiblyGenericDecl));
        let r = f(self);
        self.scopes[Namespace::OpNS].pop();
        self.scopes[Namespace::SortNS].pop();
        r
    }

    fn add_res(&mut self, ns: Namespace, ident: Ident, res: Res) {
        let Some(scope) = self.scopes[ns].last_mut() else {
            panic!("Encountered non-existent scope while adding {res:?}")
        };
        match scope.bindings.entry(ident.node) {
            indexmap::map::Entry::Occupied(_) => {
                self.resolver
                    .errors
                    .push(ResolveErr::DuplicateName(ident.node, ident.span));
            }
            indexmap::map::Entry::Vacant(v) => {
                v.insert(res);
            }
        };
    }

    /// Resolves a path in a cost expression.
    ///
    /// Lookup order, first hit wins:
    /// 1. `Cost::NAME` is a predefined cost band;
    /// 2. locals, parameters, constants and functions (`CostNS`);
    /// 3. for a callee: builtins, then operators; otherwise operators (and schema variables of
    ///    the enclosing rule), then builtins. So `has_op(t, all)` finds the operator `all`,
    ///    while `all(x in ...: ...)` is the builtin.
    fn resolve_expr_path(&mut self, id: NodeId, path: &raw::Path, is_callee: bool) {
        let last = path.segments.last().expect("non-empty path");
        let res = self.lookup_expr_path(path, is_callee);
        self.resolver.resolved.insert(last.id, res);
        self.resolver.resolved.insert(id, res);
    }

    fn lookup_expr_path(&mut self, path: &raw::Path, is_callee: bool) -> Res {
        let interner = self.cx.interner;
        if path.segments.len() == 2 {
            let first = path.segments[0].ident.node;
            let second = path.segments[1].ident.node;
            if interner.get(first) == "Cost" && cost::cost_const(interner.get(second)).is_some() {
                return Res::Builtin(second);
            }
        }
        if path.segments.len() != 1 {
            self.resolver.errors.push(ResolveErr::NotFound(
                path.segments.last().unwrap().ident.node,
                Namespace::CostNS,
                path.span,
            ));
            return Res::Err;
        }
        let name = path.segments[0].ident.node;
        let lookup = |scopes: &Vec<Scope>| {
            scopes
                .iter()
                .rev()
                .find_map(|sc| sc.bindings.get(&name).copied())
        };
        if let Some(res) = lookup(&self.scopes[Namespace::CostNS]) {
            return res;
        }
        let is_builtin = cost::builtin(interner.get(name)).is_some();
        if is_callee && is_builtin {
            return Res::Builtin(name);
        }
        if let Some(res) = lookup(&self.scopes[Namespace::OpNS]) {
            return res;
        }
        if is_builtin {
            return Res::Builtin(name);
        }
        self.resolver
            .errors
            .push(ResolveErr::NotFound(name, Namespace::CostNS, path.span));
        Res::Err
    }

    fn add_imports(
        path: &mut Vec<PathSegment>,
        uk: &UseTreeKind,
        td: &TheoryData,
        id: NodeId,
        scopes: &mut PerNS<Vec<Scope>>,
        import_res_map: &mut HashMap<NodeId, PerNS<Option<Res>>>,
    ) {
        let nss = [
            Namespace::OpNS,
            Namespace::RuleNS,
            Namespace::RuleSetNS,
            Namespace::SortNS,
            Namespace::CostNS,
        ];
        match uk {
            UseTreeKind::Simple(ident) => {
                let mut import_res = PerNS::default();
                for ns in nss {
                    if let Some(r) = td.scopes[ns].bindings.get(&path.last().unwrap().ident.node) {
                        let s = scopes[ns].last_mut().unwrap();
                        let ident = if let Some(i) = ident {
                            i.node
                        } else {
                            path.last().unwrap().ident.node
                        };
                        s.bindings.insert(ident, *r);
                        import_res[ns] = Some(*r);
                    }
                }
                import_res_map.insert(id, import_res);
            }
            UseTreeKind::Nested { items, .. } => {
                let orig_len = path.len();
                for (t, id) in items {
                    path.extend(t.prefix.segments.iter().cloned());
                    Self::add_imports(path, &t.kind, td, *id, scopes, import_res_map);
                    for _ in orig_len..path.len() {
                        path.pop();
                    }
                }
            }
            UseTreeKind::Glob => {
                for ns in nss {
                    let s = scopes[ns].last_mut().unwrap();
                    s.bindings.extend(td.scopes[ns].bindings.iter());
                }
            }
        }
    }
}

impl<'a, 'ast, 'cx> Visit<'ast> for ScopeBuilder<'a, 'cx> {
    fn visit_file(&mut self, x: &'ast crate::raw_theory::File) {
        self.with_scope(Namespace::TheoryNS, ScopeKind::File, |s| visit_file(s, x));
    }

    fn visit_theory(&mut self, x: &'ast crate::raw_theory::Theory) {
        self.scopes[Namespace::OpNS].push(Scope::new(ScopeKind::Theory));
        self.scopes[Namespace::RuleSetNS].push(Scope::new(ScopeKind::Theory));
        self.scopes[Namespace::SortNS].push(Scope::new(ScopeKind::Theory));
        self.scopes[Namespace::RuleNS].push(Scope::new(ScopeKind::Theory));
        self.scopes[Namespace::CostNS].push(Scope::new(ScopeKind::Theory));
        visit_theory(self, x);
        let mut theory = TheoryData {
            scopes: PerNS {
                op_ns: Scope::new(ScopeKind::Theory),
                theory_ns: Scope::new(ScopeKind::Theory),
                rule_ns: Scope::new(ScopeKind::Theory),
                rule_set_ns: Scope::new(ScopeKind::Theory),
                sort_ns: Scope::new(ScopeKind::Theory),
                cost_ns: Scope::new(ScopeKind::Theory),
            },
        };
        for ns in [
            Namespace::OpNS,
            Namespace::RuleSetNS,
            Namespace::RuleNS,
            Namespace::SortNS,
            Namespace::CostNS,
        ] {
            while let Some(scope) = self.scopes[ns].pop() {
                if scope.kind == ScopeKind::File {
                    self.scopes[ns].push(scope);
                    break;
                }
                if scope.kind == ScopeKind::Use {
                    // We ignore important items
                    continue;
                }
                theory.scopes[ns].bindings.extend(scope.bindings);
            }
        }
        let def_id = self.resolver.node_id_to_def_id[&x.id].to_def_id();
        self.add_res(
            Namespace::TheoryNS,
            x.name,
            Res::Def(DefKind::Theory, def_id),
        );
        self.theory_data.insert(def_id, theory);
    }

    fn visit_item(&mut self, x: &'ast raw::Item) {
        self.active_id = Some(x.id);
        visit_item(self, x);
    }

    fn visit_item_kind(&mut self, x: &'ast raw::ItemKind) {
        if let raw::ItemKind::Operators(..) = &x {
            self.in_operator_context = true;
        } else if let raw::ItemKind::Use(..) = &x {
            let nss = [
                Namespace::OpNS,
                Namespace::RuleNS,
                Namespace::RuleSetNS,
                Namespace::SortNS,
                Namespace::CostNS,
            ];

            for ns in nss {
                self.scopes[ns].push(Scope::new(ScopeKind::Use));
            }

            visit_item_kind(self, x);

            for ns in nss {
                self.scopes[ns].push(Scope::new(ScopeKind::Normal));
            }
            return;
        }
        visit_item_kind(self, x);
        self.in_operator_context = false;
    }

    fn visit_use_tree(&mut self, x: &'ast raw::UseTree) {
        // Resolve theory
        self.expected_ns = Namespace::TheoryNS;
        self.visit_path(&x.prefix);
        let Res::Def(DefKind::Theory, def_id) =
            self.resolver.resolved[&x.prefix.segments.last().unwrap().id]
        else {
            panic!("Expected theory")
        };
        let td = &self.theory_data[&def_id];
        let uk = &x.kind;
        let id = self.active_id.take().unwrap();
        Self::add_imports(
            &mut x.prefix.segments.to_vec(),
            uk,
            td,
            id,
            &mut self.scopes,
            &mut self.resolver.import_res_map,
        );
    }

    fn visit_function_decl(&mut self, x: &'ast raw::FunctionDecl) {
        self.with_generics(|this| visit_function_decl(this, x));
        self.add_res(
            Namespace::OpNS,
            x.name,
            Res::Def(
                if self.in_operator_context {
                    DefKind::Operator
                } else {
                    DefKind::Fn
                },
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_sort_decl(&mut self, x: &'ast raw::SortDecl) {
        self.with_generics(|this| visit_sort_decl(this, x));
        self.add_res(
            Namespace::SortNS,
            x.name,
            Res::Def(
                DefKind::Sort,
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_rule_set_decl(&mut self, x: &'ast raw::RuleSetDecl) {
        if let Some(st) = &x.stage
            && Stage::parse(self.cx.interner.get(st.node)).is_none()
        {
            self.resolver
                .errors
                .push(ResolveErr::InvalidStage(st.node, st.span));
        }

        // Parameters, guards and body live in their own scope.
        self.with_scope(Namespace::CostNS, ScopeKind::Normal, |this| {
            for p in &x.params {
                this.visit_cost_param(p);
            }
            for g in &x.guards {
                this.visit_guard(g);
            }
            this.visit_expr(&x.body);
        });

        let def = self.resolver.node_id_to_def_id[&x.id].to_def_id();
        match x.ext {
            None => self.add_res(
                Namespace::RuleSetNS,
                x.name,
                Res::Def(DefKind::RuleSet, def),
            ),
            Some(_) => {
                // `extend` / `override` refer to a ruleset that is already in scope.
                let found = self.scopes[Namespace::RuleSetNS]
                    .iter()
                    .rev()
                    .find_map(|sc| sc.bindings.get(&x.name.node).copied());
                let res = match found {
                    Some(res) => res,
                    None => {
                        self.resolver.errors.push(ResolveErr::NotFound(
                            x.name.node,
                            Namespace::RuleSetNS,
                            x.name.span,
                        ));
                        Res::Err
                    }
                };
                self.resolver.resolved.insert(x.id, res);
            }
        }
    }

    fn visit_guard(&mut self, x: &'ast raw::Guard) {
        let seg = x.path.segments.last().expect("non-empty path");
        let res =
            if x.path.segments.len() == 1 && cost::is_guard(self.cx.interner.get(seg.ident.node)) {
                Res::Builtin(seg.ident.node)
            } else {
                self.resolver.errors.push(ResolveErr::NotFound(
                    seg.ident.node,
                    Namespace::CostNS,
                    x.path.span,
                ));
                Res::Err
            };
        self.resolver.resolved.insert(seg.id, res);
        self.resolver.resolved.insert(x.id, res);
    }

    fn visit_cost_item(&mut self, x: &'ast raw::CostItem) {
        let def = self.resolver.node_id_to_def_id[&x.id].to_def_id();
        match &x.kind {
            raw::CostItemKind::Const { name, ty, value } => {
                self.visit_cost_ty(ty);
                self.visit_expr(value);
                self.add_res(Namespace::CostNS, *name, Res::Def(DefKind::CostConst, def));
            }
            raw::CostItemKind::Fn {
                name,
                params,
                ret,
                body,
            } => {
                // The function is added after its body: no recursion.
                self.with_scope(Namespace::CostNS, ScopeKind::Normal, |this| {
                    for p in params {
                        this.visit_cost_param(p);
                    }
                    this.visit_cost_ty(ret);
                    this.visit_expr(body);
                });
                self.add_res(Namespace::CostNS, *name, Res::Def(DefKind::CostFn, def));
            }
        }
    }

    fn visit_cost_param(&mut self, x: &'ast raw::CostParam) {
        self.visit_cost_ty(&x.ty);
        let def = self.resolver.node_id_to_def_id[&x.id].to_def_id();
        self.add_res(Namespace::CostNS, x.name, Res::Def(DefKind::CostParam, def));
    }

    fn visit_expr(&mut self, x: &'ast raw::Expr) {
        match &x.kind {
            raw::ExprKind::Int(_) | raw::ExprKind::Float(_) | raw::ExprKind::Top => {}
            raw::ExprKind::Path(p) => self.resolve_expr_path(x.id, p, false),
            raw::ExprKind::Call(p, args) => {
                self.resolve_expr_path(x.id, p, true);
                for a in args {
                    self.visit_call_arg(a);
                }
            }
            raw::ExprKind::Unary(_, e) => self.visit_expr(e),
            raw::ExprKind::Binary(_, a, b) => {
                self.visit_expr(a);
                self.visit_expr(b);
            }
            raw::ExprKind::If(c, a, b) => {
                self.visit_expr(c);
                self.visit_expr(a);
                self.visit_expr(b);
            }
            raw::ExprKind::Let(id, name, value, body) => {
                self.visit_expr(value);
                let def = self.resolver.node_id_to_def_id[id].to_def_id();
                self.with_scope(Namespace::CostNS, ScopeKind::Normal, |this| {
                    this.add_res(Namespace::CostNS, *name, Res::Def(DefKind::CostLocal, def));
                    this.visit_expr(body);
                });
            }
        }
    }

    fn visit_binder(&mut self, x: &'ast raw::Binder) {
        // The generator does not see the bound variable.
        self.visit_expr(&x.generator);
        let def = self.resolver.node_id_to_def_id[&x.id].to_def_id();
        self.with_scope(Namespace::CostNS, ScopeKind::Normal, |this| {
            this.add_res(Namespace::CostNS, x.var, Res::Def(DefKind::CostLocal, def));
            this.visit_expr(&x.body);
        });
    }

    fn visit_predicate_decl(&mut self, x: &'ast raw::PredicateDecl) {
        self.with_generics(|this| visit_predicate_decl(this, x));
        self.add_res(
            Namespace::OpNS,
            x.name,
            Res::Def(
                DefKind::Pred,
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_rule(&mut self, x: &'ast raw::Rule) {
        self.with_scope(Namespace::OpNS, ScopeKind::Rule, |this| {
            visit_rule(this, x);
        });
        self.add_res(
            Namespace::RuleNS,
            x.name,
            Res::Def(
                DefKind::Rule,
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_schema_var_decl(&mut self, x: &'ast raw::SchemaVarDecl) {
        visit_schema_var_decl(self, x);
        self.add_res(
            Namespace::OpNS,
            x.name,
            Res::Def(
                DefKind::SchemaVar,
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_term(&mut self, x: &'ast raw::Term) {
        let old_ns = self.expected_ns;
        self.expected_ns = Namespace::OpNS;
        visit_term(self, x);
        let path = match &x.kind {
            raw::TermKind::Path(p) => p,
            raw::TermKind::Call(p, ..) => p,
            raw::TermKind::ParametricPath(path, ..) => path,
            raw::TermKind::ParametricCall(path, ..) => path,
        };
        let last_seg_id = path.segments.last().unwrap().id;
        let res = self.resolver.resolved[&last_seg_id];
        self.resolver.resolved.insert(x.id, res);
        self.expected_ns = old_ns;
    }

    fn visit_sort(&mut self, x: &'ast raw::Sort) {
        let old_ns = self.expected_ns;
        self.expected_ns = Namespace::SortNS;
        visit_sort(self, x);
        self.expected_ns = old_ns;

        let path = match &x.kind {
            raw::SortKind::Simple(path) => path,
            raw::SortKind::Parametric(path, ..) => path,
        };
        self.resolver.resolved.insert(
            x.id,
            self.resolver.resolved[&path.segments.last().unwrap().id],
        );
    }

    fn visit_generic_param(&mut self, x: &'ast raw::GenericParam) {
        visit_generic_param(self, x);
        let (ns, kind) = match &x.kind {
            raw::GenericParamKind::Sort => (Namespace::SortNS, DefKind::SortParam),
            raw::GenericParamKind::Const { .. } => (Namespace::OpNS, DefKind::ConstParam),
        };
        self.add_res(
            ns,
            x.name,
            Res::Def(kind, self.resolver.node_id_to_def_id[&x.id].to_def_id()),
        );
    }

    fn visit_rule_set_ref(&mut self, x: &'ast raw::RuleSetRef) {
        let old_ns = self.expected_ns;
        self.expected_ns = Namespace::RuleSetNS;
        self.visit_path(&x.path);
        self.expected_ns = old_ns;
        let res = self.resolver.resolved[&x.path.segments.last().unwrap().id];
        self.resolver.resolved.insert(x.id, res);
        // The arguments are evaluated in the scope of the rule (schema variables, `find`, ...).
        for a in &x.args {
            self.visit_expr(a);
        }
    }

    fn visit_path(&mut self, x: &'ast raw::Path) {
        // TODO: lift this restriction
        assert!(x.segments.len() == 1);
        let seg = &x.segments[0];
        for sc in self.scopes[self.expected_ns].iter().rev() {
            if let Some(res) = sc.bindings.get(&seg.ident.node) {
                self.resolver.resolved.insert(seg.id, *res);
                return;
            }
        }

        self.resolver.errors.push(ResolveErr::NotFound(
            x.segments[0].ident.node,
            self.expected_ns,
            x.span,
        ));
        self.resolver.resolved.insert(seg.id, Res::Err);
    }
}
