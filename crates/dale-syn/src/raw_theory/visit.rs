use paste::paste;

use super::*;

macro_rules! create_visitor_traits {
    ($($name:ident : $ty:ty), *) => {
        /// Syntax tree traversal to walk a shared borrow of a [Term] syntax tree.
        pub trait Visit<'ast> {
            $(
                paste!{
                    fn [< visit_ $name >] (&mut self, x: &'ast $ty) {
                        [< visit_ $name >](self, x)
                    }
                }
            )*
        }

        $(
            impl Visitable for $ty {
                fn visit<'ast, V: Visit<'ast>>(&'ast self, v: &mut V) {
                    paste!{
                        v.[< visit_ $name >](self)
                    }
                }
            }
        )*
    };
}

/// Conveniance trait for traversal of a shared borrow of a syntax tree.
pub trait Visitable {
    fn visit<'ast, V: Visit<'ast>>(&'ast self, v: &mut V);
}

create_visitor_traits! {
  file: File,
  module_item: ModuleItem,
  module: Module,
  theory: Theory,
  path: Path,
  path_segment: PathSegment,
  id: NodeId,
  ident: Ident,
  item: Item,
  item_kind: ItemKind,
  use_tree: UseTree,
  function_decl: FunctionDecl,
  sort_decl: SortDecl,
  predicate_decl: PredicateDecl,
  sort: Sort,
  sort_kind: SortKind,
  generic_arg: GenericArg,
  generic_param: GenericParam,
  generic_param_kind: GenericParamKind,
  term: Term,
  term_kind: TermKind,
  rule: Rule,
  schema_var_decl: SchemaVarDecl,
  term_or_seq: TermOrSeq,
  seq: Seq,
  goal_specs: GoalSpecs,
  goal_spec: GoalSpec,
  rule_sets: Spanned<Vec<RuleSetRef>>,
  rule_set_ref: RuleSetRef,
  rule_set_decl: RuleSetDecl,
  guard: Guard,
  cost_item: CostItem,
  cost_param: CostParam,
  cost_ty: CostTy,
  expr: Expr,
  call_arg: CallArg,
  binder: Binder
}

pub fn visit_file<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a File) {
    v.visit_id(&x.id);
    for i in &x.items {
        v.visit_module_item(i);
    }
}

pub fn visit_module_item<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ModuleItem) {
    match x {
        ModuleItem::Theory(t) => v.visit_theory(t),
        ModuleItem::Module(m) => v.visit_module(m),
    }
}

pub fn visit_module<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Module) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for i in &x.items {
        v.visit_module_item(i);
    }
}

pub fn visit_theory<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Theory) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for i in &x.items {
        v.visit_item(i);
    }
}

pub fn visit_path<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Path) {
    for s in &x.segments {
        v.visit_path_segment(s);
    }
}

pub fn visit_path_segment<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a PathSegment) {
    v.visit_id(&x.id);
    v.visit_ident(&x.ident);
}

pub fn visit_id<'a, V: Visit<'a> + ?Sized>(_: &mut V, _: &'a NodeId) {}

pub fn visit_ident<'a, V: Visit<'a> + ?Sized>(_: &mut V, _: &'a Ident) {}

pub fn visit_item<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Item) {
    v.visit_id(&x.id);
    v.visit_item_kind(&x.kind);
}

pub fn visit_item_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ItemKind) {
    match x {
        ItemKind::Use(use_tree) => v.visit_use_tree(use_tree),
        ItemKind::Operators(function_decls) => {
            for d in function_decls {
                v.visit_function_decl(d);
            }
        }
        ItemKind::Sorts(sort_decls) => {
            for d in sort_decls {
                v.visit_sort_decl(d);
            }
        }
        ItemKind::DataTypes(_) => todo!(),
        ItemKind::Functions(function_decls) => {
            for d in function_decls {
                v.visit_function_decl(d);
            }
        }
        ItemKind::Predicates(predicate_decls) => {
            for d in predicate_decls {
                v.visit_predicate_decl(d);
            }
        }
        ItemKind::Axioms() => todo!(),
        ItemKind::Rules(rules) => {
            for r in rules {
                v.visit_rule(r);
            }
        }
        ItemKind::RuleSets(rs) => {
            for r in rs {
                v.visit_rule_set_decl(r);
            }
        }
        ItemKind::Costs(items) => {
            for c in items {
                v.visit_cost_item(c);
            }
        }
        ItemKind::Problem(seq) => {
            for t in seq.ante.iter().chain(&seq.succ) {
                v.visit_term(t);
            }
        }
    }
}

pub fn visit_use_tree<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a UseTree) {
    v.visit_path(&x.prefix);
    match &x.kind {
        UseTreeKind::Simple(Some(ident)) => v.visit_ident(ident),
        UseTreeKind::Simple(_) => {}
        UseTreeKind::Nested { items, .. } => {
            for (tree, id) in items {
                v.visit_id(id);
                v.visit_use_tree(tree);
            }
        }
        UseTreeKind::Glob => {}
    }
}

pub fn visit_function_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a FunctionDecl) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for p in &x.params {
        v.visit_generic_param(p);
    }
    if let Some(b) = &x.bind {
        v.visit_sort(&b.sort);
    }
    for s in &x.arg_sorts {
        v.visit_sort(s);
    }
    v.visit_sort(&x.sort);
}

pub fn visit_sort_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a SortDecl) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for p in &x.params {
        v.visit_generic_param(p);
    }
    if let Some((_, sorts)) = &x.extends {
        for s in sorts {
            v.visit_sort(s);
        }
    }
}

pub fn visit_predicate_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a PredicateDecl) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for p in &x.params {
        v.visit_generic_param(p);
    }
    if let Some(b) = &x.bind {
        v.visit_sort(&b.sort);
    }
    for s in &x.arg_sorts {
        v.visit_sort(s);
    }
}

pub fn visit_sort<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Sort) {
    v.visit_id(&x.id);
    v.visit_sort_kind(&x.kind);
}

pub fn visit_sort_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a SortKind) {
    match &x {
        SortKind::Simple(ident) => v.visit_path(ident),
        SortKind::Parametric(ident, generic_args) => {
            v.visit_path(ident);
            for a in generic_args {
                v.visit_generic_arg(a);
            }
        }
    }
}

pub fn visit_generic_arg<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GenericArg) {
    match &x {
        GenericArg::Sort(sort) => v.visit_sort(sort),
        GenericArg::Const(term, ..) => {
            v.visit_term(term);
        }
    }
}

pub fn visit_generic_param<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GenericParam) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    v.visit_generic_param_kind(&x.kind);
}

pub fn visit_generic_param_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GenericParamKind) {
    match &x {
        GenericParamKind::Sort => {}
        GenericParamKind::Const { sort, .. } => v.visit_sort(sort),
    }
}

pub fn visit_term<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Term) {
    v.visit_id(&x.id);
    v.visit_term_kind(&x.kind);
}

pub fn visit_term_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a TermKind) {
    match &x {
        TermKind::Path(path) => v.visit_path(path),
        TermKind::Call(path, terms) => {
            v.visit_path(path);
            for t in terms {
                v.visit_term(t);
            }
        }
        TermKind::ParametricPath(path, args) => {
            v.visit_path(path);
            for a in args {
                v.visit_generic_arg(a);
            }
        }
        TermKind::ParametricCall(path, args, terms) => {
            v.visit_path(path);
            for a in args {
                v.visit_generic_arg(a);
            }
            for t in terms {
                v.visit_term(t);
            }
        }
        TermKind::Num(_) => {}
    }
}

pub fn visit_rule<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Rule) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for s in &x.schema_vars {
        v.visit_schema_var_decl(s);
    }
    if let Some(a) = &x.assumes {
        v.visit_term_or_seq(&a.node);
    }
    if let Some(f) = &x.find {
        v.visit_term_or_seq(&f.node);
    }
    v.visit_goal_specs(&x.goal_specs);
    if let Some(rs) = &x.rule_sets {
        v.visit_rule_sets(rs);
    }
}

pub fn visit_schema_var_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a SchemaVarDecl) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    v.visit_sort(&x.sort);
}

pub fn visit_term_or_seq<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a TermOrSeq) {
    match &x {
        TermOrSeq::Term(term) => v.visit_term(term),
        TermOrSeq::Seq(seq) => v.visit_seq(seq),
    }
}

pub fn visit_seq<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Seq) {
    v.visit_id(&x.id);
    for a in &x.ante {
        v.visit_term(a);
    }
    for s in &x.succ {
        v.visit_term(s);
    }
}

pub fn visit_goal_specs<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GoalSpecs) {
    match &x {
        GoalSpecs::CloseGoal(node_id, ..) => v.visit_id(node_id),
        GoalSpecs::Specs(goal_specs, ..) => {
            for s in goal_specs {
                v.visit_goal_spec(s);
            }
        }
    }
}

pub fn visit_goal_spec<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GoalSpec) {
    v.visit_id(&x.id);
    if let Some(n) = &x.name {
        v.visit_ident(n);
    }
    if let Some(r) = &x.replace_with {
        v.visit_term_or_seq(r);
    }
    if let Some(a) = &x.add {
        v.visit_term_or_seq(a);
    }
}

pub fn visit_rule_sets<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Spanned<Vec<RuleSetRef>>) {
    for r in &x.node {
        v.visit_rule_set_ref(r);
    }
}

pub fn visit_rule_set_ref<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a RuleSetRef) {
    v.visit_id(&x.id);
    v.visit_path(&x.path);
    for a in &x.args {
        v.visit_expr(a);
    }
}

pub fn visit_rule_set_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a RuleSetDecl) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    for p in &x.params {
        v.visit_cost_param(p);
    }
    if let Some(s) = &x.stage {
        v.visit_ident(s);
    }
    for g in &x.guards {
        v.visit_guard(g);
    }
    v.visit_expr(&x.body);
}

pub fn visit_guard<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Guard) {
    v.visit_id(&x.id);
    v.visit_path(&x.path);
}

pub fn visit_cost_item<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a CostItem) {
    v.visit_id(&x.id);
    match &x.kind {
        CostItemKind::Const { name, ty, value } => {
            v.visit_ident(name);
            v.visit_cost_ty(ty);
            v.visit_expr(value);
        }
        CostItemKind::Fn {
            name,
            params,
            ret,
            body,
        } => {
            v.visit_ident(name);
            for p in params {
                v.visit_cost_param(p);
            }
            v.visit_cost_ty(ret);
            v.visit_expr(body);
        }
    }
}

pub fn visit_cost_param<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a CostParam) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    v.visit_cost_ty(&x.ty);
}

pub fn visit_cost_ty<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a CostTy) {
    v.visit_id(&x.id);
    v.visit_ident(&x.name);
    if let Some(arg) = &x.arg {
        v.visit_sort(arg);
    }
}

pub fn visit_expr<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Expr) {
    v.visit_id(&x.id);
    match &x.kind {
        ExprKind::Int(_) | ExprKind::Float(_) | ExprKind::Top => {}
        ExprKind::Path(p) => v.visit_path(p),
        ExprKind::Call(p, args) => {
            v.visit_path(p);
            for a in args {
                v.visit_call_arg(a);
            }
        }
        ExprKind::Unary(_, e) => v.visit_expr(e),
        ExprKind::Binary(_, a, b) => {
            v.visit_expr(a);
            v.visit_expr(b);
        }
        ExprKind::If(c, a, b) => {
            v.visit_expr(c);
            v.visit_expr(a);
            v.visit_expr(b);
        }
        ExprKind::Let(id, name, value, body) => {
            v.visit_id(id);
            v.visit_ident(name);
            v.visit_expr(value);
            v.visit_expr(body);
        }
    }
}

pub fn visit_call_arg<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a CallArg) {
    match x {
        CallArg::Expr(e) => v.visit_expr(e),
        CallArg::Binder(b) => v.visit_binder(b),
    }
}

pub fn visit_binder<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Binder) {
    v.visit_id(&x.id);
    v.visit_ident(&x.var);
    v.visit_expr(&x.generator);
    v.visit_expr(&x.body);
}
