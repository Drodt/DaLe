use std::{collections::HashMap, fmt::Display, ops};

use dale_index::Idx;
use dale_util::symbol::Symbol;
use indexmap::IndexMap;

use crate::{
    ctx::Ctx,
    ir::{DefId, DefKind, LocalDefId, Res, TermKind},
    raw_theory::{
        self as raw, File, Ident, NodeId, PathSegment, Span, UseTreeKind, Visit, visit_file,
        visit_function_decl, visit_generic_param, visit_item, visit_item_kind,
        visit_predicate_decl, visit_rule, visit_rule_sets, visit_schema_var_decl, visit_sort,
        visit_sort_decl, visit_term, visit_theory, visit_use_tree,
    },
};

pub fn resolve<'cx>(cx: Ctx<'cx>, file: &File) -> Resolver<'cx> {
    let mut r = Resolver::new(cx);
    collect_defs(cx, &mut r, file);
    let mut sb = ScopeBuilder::new(cx, &mut r);
    sb.visit_file(file);
    r
}

#[derive(Debug, Clone)]
pub enum ResolveErr {
    DuplicateName(Symbol, Span),
    NotFound(Symbol, Namespace, Span),
    NoRes(Span),
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
            ResolveErr::NoRes(_) => format!("No resolution found"),
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            ResolveErr::DuplicateName(_, span) => span,
            ResolveErr::NotFound(_, _, span) => span,
            ResolveErr::NoRes(span) => span,
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
}

impl Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Namespace::OpNS => "operator",
            Namespace::TheoryNS => "theory",
            Namespace::RuleNS => "rule",
            Namespace::RuleSetNS => "rule sets",
            Namespace::SortNS => "sort",
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
    }
}

pub struct Resolver<'cx> {
    cx: Ctx<'cx>,
    node_id_to_def_id: HashMap<NodeId, LocalDefId>,
    next_def_id: LocalDefId,
    resolved: HashMap<NodeId, Res>,
    import_res_map: HashMap<NodeId, PerNS<Option<Res>>>,
    pub errors: Vec<ResolveErr>,
}

impl<'cx> Resolver<'cx> {
    pub fn new(cx: Ctx<'cx>) -> Self {
        Self {
            cx,
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

fn collect_defs<'cx>(cx: Ctx<'cx>, resolver: &mut Resolver<'cx>, file: &File) {
    let mut coll = DefCollector::new(cx, resolver);
    coll.visit_file(file);
}

struct TheoryData {
    scopes: PerNS<Scope>,
}

pub struct DefCollector<'a, 'cx> {
    cx: Ctx<'cx>,
    resolver: &'a mut Resolver<'cx>,
    in_operator_mode: bool,
}

impl<'a, 'cx> DefCollector<'a, 'cx> {
    pub fn new(cx: Ctx<'cx>, resolver: &'a mut Resolver<'cx>) -> Self {
        Self {
            cx,
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

impl<'a, 'ast, 'cx> Visit<'ast> for DefCollector<'a, 'cx> {
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
    }
}

pub struct ScopeBuilder<'a, 'cx> {
    cx: Ctx<'cx>,
    scopes: PerNS<Vec<Scope>>,
    resolver: &'a mut Resolver<'cx>,
    in_operator_context: bool,
    expected_ns: Namespace,
    theory_data: HashMap<DefId, TheoryData>,
    active_id: Option<NodeId>,
}

impl<'a, 'cx> ScopeBuilder<'a, 'cx> {
    pub fn new(cx: Ctx<'cx>, resolver: &'a mut Resolver<'cx>) -> Self {
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

    fn op_ns_opt_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.op_ns.last_mut()
    }

    fn op_ns_mut(&mut self) -> &mut Scope {
        self.op_ns_opt_mut()
            .expect("operator/function/predicate scopes are not empty")
    }

    fn theory_ns_opt_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.theory_ns.last_mut()
    }

    fn theory_ns_mut(&mut self) -> &mut Scope {
        self.theory_ns_opt_mut()
            .expect("theory scopes are not empty")
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
        visit_theory(self, x);
        let mut theory = TheoryData {
            scopes: PerNS {
                op_ns: Scope::new(ScopeKind::Theory),
                theory_ns: Scope::new(ScopeKind::Theory),
                rule_ns: Scope::new(ScopeKind::Theory),
                rule_set_ns: Scope::new(ScopeKind::Theory),
                sort_ns: Scope::new(ScopeKind::Theory),
            },
        };
        for ns in [
            Namespace::OpNS,
            Namespace::RuleSetNS,
            Namespace::RuleNS,
            Namespace::SortNS,
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
                theory.scopes[ns]
                    .bindings
                    .extend(scope.bindings.into_iter());
            }
        }
        let def_id = self.resolver.node_id_to_def_id[&x.id].to_def_id();
        self.add_res(
            Namespace::TheoryNS,
            x.name.clone(),
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
            x.name.clone(),
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
            x.name.clone(),
            Res::Def(
                DefKind::Sort,
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_rule_set_decl(&mut self, x: &'ast raw::RuleSetDecl) {
        self.add_res(
            Namespace::RuleSetNS,
            x.name.clone(),
            Res::Def(
                DefKind::RuleSet,
                self.resolver.node_id_to_def_id[&x.id].to_def_id(),
            ),
        );
    }

    fn visit_predicate_decl(&mut self, x: &'ast raw::PredicateDecl) {
        self.with_generics(|this| visit_predicate_decl(this, x));
        self.add_res(
            Namespace::OpNS,
            x.name.clone(),
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
            x.name.clone(),
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
            x.name.clone(),
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
            x.name.clone(),
            Res::Def(kind, self.resolver.node_id_to_def_id[&x.id].to_def_id()),
        );
    }

    fn visit_rule_sets(&mut self, x: &'ast raw::Spanned<Vec<(NodeId, raw::Path)>>) {
        let old_ns = self.expected_ns;
        self.expected_ns = Namespace::RuleSetNS;
        visit_rule_sets(self, x);
        self.expected_ns = old_ns;
        for (id, path) in x.node.iter() {
            self.resolver.resolved.insert(
                *id,
                self.resolver.resolved[&path.segments.last().unwrap().id],
            );
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
            x.span.clone(),
        ));
        self.resolver.resolved.insert(seg.id, Res::Err);
    }
}
