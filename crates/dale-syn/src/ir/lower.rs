use std::collections::HashMap;

use dale_util::arena::DroplessArena;
use smallvec::{SmallVec, smallvec};

use super::*;
use crate::{
    ctx::Ctx,
    raw_theory::{self as raw, NodeId},
    resolve::Resolver,
};

pub fn lower<'ir>(file: raw::File, cx: Ctx<'ir>, resolver: Resolver<'ir>) -> Map<'ir> {
    let mut lcx = LowerCtx::new(cx, resolver);
    let file = lcx.lower_file(file);
    Map {
        file,
        items: lcx.items,
    }
}

struct LowerCtx<'ir> {
    resolver: Resolver<'ir>,
    arena: &'ir DroplessArena,
    current_file_id: FileId,
    current_ir_id: IrId,
    node_id_to_ir_id: HashMap<NodeId, IrId>,
    items: HashMap<ItemId, &'ir Item<'ir>>,
}

impl<'ir> LowerCtx<'ir> {
    fn new(cx: Ctx<'ir>, resolver: Resolver<'ir>) -> Self {
        Self {
            resolver,
            arena: cx.ir_arena,
            current_file_id: FileId::ZERO,
            current_ir_id: IrId::ZERO,
            node_id_to_ir_id: HashMap::default(),
            items: HashMap::default(),
        }
    }
}

impl<'ir> LowerCtx<'ir> {
    fn ir_id(&self, id: NodeId) -> IrId {
        *self.node_id_to_ir_id.get(&id).expect("Ir id")
    }

    fn lower_span(&self, span: &raw::Span) -> Span {
        Span {
            file: self.current_file_id,
            start: span.0,
            end: span.1,
        }
    }

    fn lower_ident(&self, ident: &raw::Ident) -> Ident {
        Ident {
            span: self.lower_span(&ident.span),
            name: ident.node,
        }
    }

    fn lower_node_id(&mut self, node_id: raw::NodeId) -> IrId {
        let id = self.current_ir_id;
        self.current_ir_id.increment_by(1);
        let old = self.node_id_to_ir_id.insert(node_id, id);
        assert!(old.is_none(), "Already created IR id for {:?}", node_id);
        id
    }

    fn lower_file(&mut self, file: raw::File) -> File<'ir> {
        let theories = self
            .arena
            .alloc_from_iter(file.theories.iter().map(|t| self.lower_theory(t)));
        File {
            id: FileId::ZERO,
            theories,
        }
    }

    fn lower_theory(&mut self, theory: &raw::Theory) -> Theory<'ir> {
        let id = self.resolver.def_id(theory.id);
        let name = self.lower_ident(&theory.name);
        let items = self
            .arena
            .alloc_from_iter(theory.items.iter().flat_map(|x| self.lower_item_refs(x)));
        for i in &theory.items {
            self.lower_item(i);
        }
        Theory { id, name, items }
    }

    fn lower_item_refs(&mut self, item: &raw::Item) -> SmallVec<[ItemId; 1]> {
        let mut ids = smallvec![ItemId {
            id: self.lower_node_id(item.id)
        }];
        if let raw::ItemKind::Use(use_tree) = &item.kind {
            self.lower_item_id_use_tree(use_tree, &mut ids);
        }
        ids
    }

    fn lower_item_id_use_tree(&mut self, tree: &raw::UseTree, vec: &mut SmallVec<[ItemId; 1]>) {
        match &tree.kind {
            raw::UseTreeKind::Simple(_) | raw::UseTreeKind::Glob => {}
            raw::UseTreeKind::Nested { items, .. } => {
                for &(ref nested, id) in items {
                    vec.push(ItemId {
                        id: self.lower_node_id(id),
                    });
                    self.lower_item_id_use_tree(nested, vec);
                }
            }
        }
    }

    fn lower_item(&mut self, item: &raw::Item) -> &'ir Item<'ir> {
        let span = self.lower_span(&item.span);
        let id = ItemId {
            id: self.ir_id(item.id),
        };
        let kind = self.lower_item_kind(&item.kind, item.id);
        let i = self.arena.alloc(Item { id, span, kind });
        let old = self.items.insert(id, i);
        assert!(old.is_none(), "Already have item for {id:?}");
        i
    }

    fn lower_item_kind(&mut self, kind: &raw::ItemKind, id: NodeId) -> ItemKind<'ir> {
        match kind {
            raw::ItemKind::Use(use_tree) => {
                let prefix = raw::Path {
                    segments: Vec::new(),
                    span: use_tree.span,
                };
                self.lower_use_tree(use_tree, &prefix, id)
            }
            raw::ItemKind::Operators(ops) => ItemKind::Operators(
                self.arena
                    .alloc_from_iter(ops.iter().map(|f| self.lower_operator_decl(f))),
            ),
            raw::ItemKind::Sorts(sort_decls) => ItemKind::Sorts(
                self.arena
                    .alloc_from_iter(sort_decls.iter().map(|s| self.lower_sort_decl(s))),
            ),
            raw::ItemKind::DataTypes(_data_type_decls) => todo!(),
            raw::ItemKind::Functions(fns) => ItemKind::Functions(
                self.arena
                    .alloc_from_iter(fns.iter().map(|f| self.lower_function_decl(f))),
            ),
            raw::ItemKind::Predicates(preds) => ItemKind::Predicates(
                self.arena
                    .alloc_from_iter(preds.iter().map(|p| self.lower_pred_decl(p))),
            ),
            raw::ItemKind::Axioms() => todo!(),
            raw::ItemKind::Rules(rules) => ItemKind::Rules(
                self.arena
                    .alloc_from_iter(rules.iter().map(|r| self.lower_rule(r))),
            ),
            raw::ItemKind::RuleSets(rule_sets) => ItemKind::RuleSets(
                self.arena
                    .alloc_from_iter(rule_sets.iter().map(|r| self.lower_rule_set_decl(r))),
            ),
        }
    }

    fn lower_use_tree(
        &mut self,
        tree: &raw::UseTree,
        prefix: &raw::Path,
        id: NodeId,
    ) -> ItemKind<'ir> {
        let path = &tree.prefix;
        let segments = prefix
            .segments
            .iter()
            .chain(path.segments.iter())
            .cloned()
            .collect();

        match tree.kind {
            raw::UseTreeKind::Simple(_) => {
                let ident = tree.ident();

                // First, apply the prefix to the path.
                let path = raw::Path {
                    segments,
                    span: path.span,
                };

                let res = self.lower_import_res(id);
                let path = self.lower_use_path(res, &path);
                let ident = self.lower_ident(&ident);
                ItemKind::Use(path, UseKind::Single(ident))
            }
            raw::UseTreeKind::Nested {
                items: ref trees, ..
            } => {
                todo!()
            }
            raw::UseTreeKind::Glob => {
                let res = self
                    .resolver
                    .get_res(segments.last().unwrap().id)
                    .expect("Res");
                let res = match res {
                    Res::Def(DefKind::Theory | DefKind::File, _) => PerNS {
                        op_ns: None,
                        theory_ns: Some(res),
                        rule_ns: None,
                        rule_set_ns: None,
                        sort_ns: None,
                    },
                    Res::Def(DefKind::Operator | DefKind::Pred | DefKind::Fn, _) => PerNS {
                        op_ns: Some(res),
                        theory_ns: None,
                        rule_ns: None,
                        rule_set_ns: None,
                        sort_ns: None,
                    },
                    Res::Def(DefKind::Sort | DefKind::DataType, _) => PerNS {
                        op_ns: None,
                        theory_ns: None,
                        rule_ns: None,
                        rule_set_ns: None,
                        sort_ns: Some(res),
                    },
                    Res::Def(DefKind::Rule, _) => PerNS {
                        op_ns: None,
                        theory_ns: None,
                        rule_ns: Some(res),
                        rule_set_ns: None,
                        sort_ns: None,
                    },
                    Res::Def(DefKind::RuleSet, _) => PerNS {
                        op_ns: None,
                        theory_ns: None,
                        rule_ns: None,
                        rule_set_ns: Some(res),
                        sort_ns: None,
                    },
                    Res::Err => {
                        let err = Some(Res::Err);
                        PerNS {
                            op_ns: err,
                            theory_ns: err,
                            rule_ns: err,
                            rule_set_ns: err,
                            sort_ns: err,
                        }
                    }
                    _ => panic!("Invalid use"),
                };
                let path = raw::Path {
                    segments,
                    span: path.span,
                };
                let path = self.lower_use_path(res, &path);
                ItemKind::Use(path, UseKind::Glob)
            }
        }
    }

    fn lower_import_res(&self, id: NodeId) -> PerNS<Option<Res>> {
        let per_ns = self.resolver.get_import_res(id);
        if per_ns.is_empty() {
            // Propagate the error to all namespaces, just to be sure.
            let err = Some(Res::Err);
            return PerNS {
                op_ns: err,
                theory_ns: err,
                rule_ns: err,
                rule_set_ns: err,
                sort_ns: err,
            };
        }
        per_ns
    }

    fn lower_use_path(&mut self, res: PerNS<Option<Res>>, path: &raw::Path) -> &'ir UsePath<'ir> {
        assert!(!res.is_empty());
        self.arena.alloc(UsePath {
            res,
            segments: self
                .arena
                .alloc_from_iter(path.segments.iter().map(|seg| self.lower_path_segment(seg))),
            span: self.lower_span(&path.span),
        })
    }

    fn lower_operator_decl(&mut self, o: &raw::FunctionDecl) -> OperatorDecl<'ir> {
        let id = self.resolver.def_id(o.id);
        let name = self.lower_ident(&o.name);
        let params = self
            .arena
            .alloc_from_iter(o.params.iter().map(|p| self.lower_generic_param(p)));
        let where_to_bind = self.arena.alloc_from_iter(o.where_to_bind.iter().copied());
        let arg_sort_refs = self
            .arena
            .alloc_from_iter(o.arg_sorts.iter().map(|s| self.lower_sort_ref(s)));
        let sort_ref = self.arena.alloc(self.lower_sort_ref(&o.sort));
        OperatorDecl {
            id,
            rigid: o.modifiers.rigid,
            name,
            params,
            where_to_bind,
            arg_sort_refs,
            sort_ref,
        }
    }

    fn lower_function_decl(&mut self, o: &raw::FunctionDecl) -> FunctionDecl<'ir> {
        let id = self.lower_node_id(o.id);
        let name = self.lower_ident(&o.name);
        let params = self
            .arena
            .alloc_from_iter(o.params.iter().map(|p| self.lower_generic_param(p)));
        let where_to_bind = self.arena.alloc_from_iter(o.where_to_bind.iter().copied());
        let arg_sort_refs = self
            .arena
            .alloc_from_iter(o.arg_sorts.iter().map(|s| self.lower_sort_ref(s)));
        let sort_ref = self.arena.alloc(self.lower_sort_ref(&o.sort));
        FunctionDecl {
            id,
            modifiers: self.lower_fn_modifiers(o.modifiers),
            name,
            params,
            where_to_bind,
            arg_sort_refs,
            sort_ref,
        }
    }

    fn lower_pred_decl(&mut self, o: &raw::PredicateDecl) -> PredicateDecl<'ir> {
        let id = self.resolver.def_id(o.id);
        let name = self.lower_ident(&o.name);
        let params = self
            .arena
            .alloc_from_iter(o.params.iter().map(|p| self.lower_generic_param(p)));
        let where_to_bind = self.arena.alloc_from_iter(o.where_to_bind.iter().copied());
        let arg_sort_refs = self
            .arena
            .alloc_from_iter(o.arg_sorts.iter().map(|s| self.lower_sort_ref(s)));
        PredicateDecl {
            id,
            rigid: o.rigid,
            name,
            params,
            where_to_bind,
            arg_sort_refs,
        }
    }

    fn lower_sort_decl(&mut self, s: &raw::SortDecl) -> SortDecl<'ir> {
        let id = self.resolver.def_id(s.id);
        let modifiers = self.lower_sort_modifiers(s.modifiers);
        let span = self.lower_span(&s.span);
        let name = self.lower_ident(&s.name);
        let params = self
            .arena
            .alloc_from_iter(s.params.iter().map(|p| self.lower_generic_param(p)));
        let extends: Option<(Span, &[SortRef<'ir>])> = s.extends.as_ref().map(|(sp, e)| {
            (
                self.lower_span(sp),
                self.arena
                    .alloc_from_iter(e.iter().map(|s| self.lower_sort_ref(s)))
                    as &[SortRef],
            )
        });
        SortDecl {
            id,
            modifiers,
            span,
            name,
            params,
            extends,
        }
    }

    fn lower_rule(&mut self, r: &raw::Rule) -> Rule<'ir> {
        let id = self.resolver.def_id(r.id);
        let name = self.lower_ident(&r.name);
        let schema_vars = self
            .arena
            .alloc_from_iter(r.schema_vars.iter().map(|sv| self.lower_schema_var(sv)));
        let assumes = r
            .assumes
            .as_ref()
            .map(|s| (self.lower_span(&s.span), self.lower_term_or_seq(&s.node)));
        let find = r
            .find
            .as_ref()
            .map(|s| (self.lower_span(&s.span), self.lower_term_or_seq(&s.node)));
        let goal_specs = self.lower_goal_specs(&r.goal_specs);
        let rule_sets: Option<(Span, &[Path<'_>])> = r.rule_sets.as_ref().map(|rs| {
            (
                self.lower_span(&rs.span),
                self.arena
                    .alloc_from_iter(rs.node.iter().map(|r| self.lower_path(&r.1, r.0)))
                    as &[Path],
            )
        });
        let display_name = r.display_name.as_ref().map(|i| self.lower_ident(i));
        let span = self.lower_span(&r.span);
        Rule {
            id,
            name,
            schema_vars,
            assumes,
            find,
            goal_specs,
            rule_sets,
            display_name,
            span,
        }
    }

    fn lower_fn_modifiers(&mut self, m: raw::FunctionModifiers) -> FunctionModifiers {
        FunctionModifiers {
            rigid: m.rigid,
            unique: m.unique,
            skolem: m.skolem,
        }
    }

    fn lower_sort_modifiers(&mut self, m: raw::SortModifiers) -> SortModifiers {
        SortModifiers {
            meta: m.meta,
            top: m.top,
        }
    }

    fn lower_generic_param(&mut self, p: &raw::GenericParam) -> GenericParam<'ir> {
        let id = self.resolver.def_id(p.id);
        let name = self.lower_ident(&p.name);
        let kind = self.lower_generic_param_kind(&p.kind);
        let colon_span = p.colon_span.map(|s| self.lower_span(&s));
        GenericParam {
            id,
            name,
            kind,
            colon_span,
        }
    }

    fn lower_generic_param_kind(&mut self, k: &raw::GenericParamKind) -> GenericParamKind<'ir> {
        match k {
            raw::GenericParamKind::Sort => GenericParamKind::Sort,
            raw::GenericParamKind::Const { sort, span } => GenericParamKind::Const {
                sort: self.arena.alloc(self.lower_sort_ref(sort)),
                span: self.lower_span(span),
            },
        }
    }

    fn lower_sort_ref(&mut self, s: &raw::Sort) -> SortRef<'ir> {
        let id = self.lower_node_id(s.id);
        let span = self.lower_span(&s.span);
        let (name, args): (&Path<'_>, &[GenericArg<'_>]) = match &s.kind {
            raw::SortKind::Simple(path) => (
                self.arena.alloc(self.lower_path(path, s.id)),
                self.arena.alloc_from_iter(
                    [].iter()
                        .map(|a: &raw::GenericArg| self.lower_generic_arg(a)),
                ),
            ),
            raw::SortKind::Parametric(path, generic_args) => (
                self.arena.alloc(self.lower_path(path, s.id)),
                self.arena
                    .alloc_from_iter(generic_args.iter().map(|a| self.lower_generic_arg(a))),
            ),
        };
        SortRef {
            id,
            span,
            path: name,
            args,
        }
    }

    fn lower_generic_arg(&mut self, a: &raw::GenericArg) -> GenericArg<'ir> {
        match a {
            raw::GenericArg::Sort(sort) => {
                GenericArg::Sort(self.arena.alloc(self.lower_sort_ref(sort)))
            }
            raw::GenericArg::Const(term, span) => GenericArg::Const(
                self.arena.alloc(self.lower_term(term)),
                self.lower_span(span),
            ),
        }
    }

    fn lower_path(&mut self, path: &raw::Path, id: NodeId) -> Path<'ir> {
        let span = self.lower_span(&path.span);
        let res = self.resolver.get_res(id).unwrap_or(Res::Err);
        let segments = self
            .arena
            .alloc_from_iter(path.segments.iter().map(|s| self.lower_path_segment(s)));
        Path {
            span,
            segments,
            res,
        }
    }

    fn lower_path_segment(&mut self, s: &raw::PathSegment) -> PathSegment {
        let id = self.lower_node_id(s.id);
        let ident = self.lower_ident(&s.ident);
        let span = self.lower_span(&s.span);
        PathSegment { id, ident, span }
    }

    fn lower_goal_specs(&mut self, s: &raw::GoalSpecs) -> GoalSpecs<'ir> {
        match s {
            raw::GoalSpecs::CloseGoal(node_id, span) => {
                GoalSpecs::CloseGoal(self.lower_node_id(*node_id), self.lower_span(span))
            }
            raw::GoalSpecs::Specs(goal_specs, span) => GoalSpecs::Specs(
                self.arena
                    .alloc_from_iter(goal_specs.iter().map(|s| self.lower_goal_spec(s))),
                self.lower_span(span),
            ),
        }
    }

    fn lower_goal_spec(&mut self, s: &raw::GoalSpec) -> GoalSpec<'ir> {
        let span = self.lower_span(&s.span);
        let id = self.lower_node_id(s.id);
        let name = s.name.as_ref().map(|i| self.lower_ident(i));
        let replace_with = s.replace_with.as_ref().map(|r| self.lower_term_or_seq(r));
        let add = s.add.as_ref().map(|a| self.lower_term_or_seq(a));
        GoalSpec {
            id,
            name,
            replace_with,
            add,
            span,
        }
    }

    fn lower_term_or_seq(&mut self, ts: &raw::TermOrSeq) -> TermOrSeq<'ir> {
        match ts {
            raw::TermOrSeq::Term(term) => TermOrSeq::Term(self.arena.alloc(self.lower_term(term))),
            raw::TermOrSeq::Seq(seq) => TermOrSeq::Seq(self.arena.alloc(self.lower_seq(seq))),
        }
    }

    fn lower_seq(&mut self, s: &raw::Seq) -> Seq<'ir> {
        let id = self.lower_node_id(s.id);
        let ante = self
            .arena
            .alloc_from_iter(s.ante.iter().map(|t| self.lower_term(t)));
        let succ = self
            .arena
            .alloc_from_iter(s.succ.iter().map(|t| self.lower_term(t)));
        Seq { id, ante, succ }
    }

    fn lower_term(&mut self, t: &raw::Term) -> Term<'ir> {
        let id = self.lower_node_id(t.id);
        let span = self.lower_span(&t.span);
        let kind = match &t.kind {
            raw::TermKind::Path(path) => TermKind::Path(
                self.arena.alloc(self.lower_path(path, t.id)),
                self.arena.alloc_from_iter(
                    [].iter()
                        .map(|a: &raw::GenericArg| self.lower_generic_arg(a)),
                ),
            ),
            raw::TermKind::ParametricPath(path, generic_args) => TermKind::Path(
                self.arena.alloc(self.lower_path(path, t.id)),
                self.arena
                    .alloc_from_iter(generic_args.iter().map(|a| self.lower_generic_arg(a))),
            ),
            raw::TermKind::Call(path, terms) => TermKind::Call(
                self.arena.alloc(self.lower_path(path, t.id)),
                self.arena.alloc_from_iter(
                    [].iter()
                        .map(|a: &raw::GenericArg| self.lower_generic_arg(a)),
                ),
                self.arena
                    .alloc_from_iter(terms.iter().map(|t| self.lower_term(t))),
            ),
            raw::TermKind::ParametricCall(path, generic_args, terms) => TermKind::Call(
                self.arena.alloc(self.lower_path(path, t.id)),
                self.arena
                    .alloc_from_iter(generic_args.iter().map(|a| self.lower_generic_arg(a))),
                self.arena
                    .alloc_from_iter(terms.iter().map(|t| self.lower_term(t))),
            ),
        };
        Term { id, span, kind }
    }

    fn lower_schema_var(&mut self, sv: &raw::SchemaVarDecl) -> SchemaVarDecl<'ir> {
        let id = self.resolver.def_id(sv.id);
        let span = self.lower_span(&sv.span);
        let name = self.lower_ident(&sv.name);
        let sort = self.arena.alloc(self.lower_sort_ref(&sv.sort));
        SchemaVarDecl {
            id,
            span,
            name,
            sort,
        }
    }

    fn lower_rule_set_decl(&mut self, r: &raw::RuleSetDecl) -> RuleSetDecl {
        let id = self.resolver.def_id(r.id);
        let name = self.lower_ident(&r.name);
        RuleSetDecl { id, name }
    }
}
