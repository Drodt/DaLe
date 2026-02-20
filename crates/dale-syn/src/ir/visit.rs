use paste::paste;

use super::*;

pub trait BaseVisit<'ir> {
    fn visit_use_item(&mut self, p: &'ir UsePath<'ir>, k: &'ir UseKind);
}

impl<'ir, T> BaseVisit<'ir> for T
where
    T: Visit<'ir>,
{
    fn visit_use_item(&mut self, p: &'ir UsePath<'ir>, k: &'ir UseKind) {
        visit_use_item(self, p, k);
    }
}

macro_rules! create_visitor_traits {
    ($($name:ident : $ty:ty), *) => {
        /// Syntax tree traversal to walk a shared borrow of a [Term] syntax tree.
        pub trait Visit<'ir>: BaseVisit<'ir> {
            $(
                paste!{
                    fn [< visit_ $name >] (&mut self, x: &'ir $ty) {
                        [< visit_ $name >](self, x)
                    }
                }
            )*
        }

        $(
            impl<'ir> Visitable<'ir> for $ty {
                fn visit<V: Visit<'ir>>(&'ir self, v: &mut V) {
                    paste!{
                        v.[< visit_ $name >](self)
                    }
                }
            }
        )*
    };
}

/// Conveniance trait for traversal of a shared borrow of a syntax tree.
pub trait Visitable<'ir> {
    fn visit<V: Visit<'ir>>(&'ir self, v: &mut V);
}

create_visitor_traits! {
  def_id: DefId,
  file: File<'ir>,
  file_id: FileId,
  theory: Theory<'ir>,
  path: Path<'ir>,
  path_segment: PathSegment,
  id: IrId,
  ident: Ident,
  item: Item<'ir>,
  item_id: ItemId,
  item_kind: ItemKind<'ir>,
  use_path: UsePath<'ir>,
  operator_decl: OperatorDecl<'ir>,
  function_decl: FunctionDecl<'ir>,
  sort_decl: SortDecl<'ir>,
  predicate_decl: PredicateDecl<'ir>,
  sort_ref: SortRef<'ir>,
  generic_arg: GenericArg<'ir>,
  generic_param: GenericParam<'ir>,
  generic_param_kind: GenericParamKind<'ir>,
  term: Term<'ir>,
  term_kind: TermKind<'ir>,
  rule: Rule<'ir>,
  schema_var_decl: SchemaVarDecl<'ir>,
  term_or_seq: TermOrSeq<'ir>,
  seq: Seq<'ir>,
  goal_specs: GoalSpecs<'ir>,
  goal_spec: GoalSpec<'ir>,
  rule_sets: [Path<'ir>],
  rule_set_decl: RuleSetDecl
}

pub fn visit_file<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a File) {
    v.visit_file_id(&x.id);
    for t in x.theories {
        v.visit_theory(t);
    }
}

pub fn visit_def_id<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a DefId) {
    v.visit_file_id(&x.file);
}

pub fn visit_file_id<'a, V: Visit<'a> + ?Sized>(_: &mut V, _: &'a FileId) {}

pub fn visit_theory<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Theory) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    for i in x.items {
        v.visit_item_id(i);
    }
}

pub fn visit_path<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Path) {
    for s in x.segments {
        v.visit_path_segment(s);
    }
}

pub fn visit_path_segment<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a PathSegment) {
    v.visit_id(&x.id);
    v.visit_ident(&x.ident);
}

pub fn visit_id<'a, V: Visit<'a> + ?Sized>(_: &mut V, _: &'a IrId) {}

pub fn visit_ident<'a, V: Visit<'a> + ?Sized>(_: &mut V, _: &'a Ident) {}

pub fn visit_item<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Item) {
    v.visit_item_kind(&x.kind);
}

pub fn visit_item_id<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ItemId) {
    v.visit_id(&x.id);
}

pub fn visit_item_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a ItemKind) {
    match x {
        ItemKind::Use(path, use_kind) => v.visit_use_item(path, use_kind),
        ItemKind::Operators(operator_decls) => {
            for d in *operator_decls {
                v.visit_operator_decl(d);
            }
        }
        ItemKind::Sorts(sort_decls) => {
            for d in *sort_decls {
                v.visit_sort_decl(d);
            }
        }
        ItemKind::DataTypes(_) => todo!(),
        ItemKind::Functions(function_decls) => {
            for d in *function_decls {
                v.visit_function_decl(d);
            }
        }
        ItemKind::Predicates(predicate_decls) => {
            for d in *predicate_decls {
                v.visit_predicate_decl(d);
            }
        }
        ItemKind::Axioms() => todo!(),
        ItemKind::Rules(rules) => {
            for r in *rules {
                v.visit_rule(r);
            }
        }
        ItemKind::RuleSets(rule_set_decls) => {
            for d in *rule_set_decls {
                v.visit_rule_set_decl(d);
            }
        }
    }
}

pub fn visit_use_item<'a, V: Visit<'a> + ?Sized>(v: &mut V, p: &'a UsePath<'a>, _: &'a UseKind) {
    v.visit_use_path(p);
}

pub fn visit_use_path<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a UsePath<'a>) {
    for s in x.segments {
        v.visit_path_segment(s);
    }
}

pub fn visit_operator_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a OperatorDecl<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    for p in x.params {
        v.visit_generic_param(p);
    }
    for s in x.arg_sort_refs {
        v.visit_sort_ref(s);
    }
    v.visit_sort_ref(x.sort_ref);
}

pub fn visit_function_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a FunctionDecl<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    for p in x.params {
        v.visit_generic_param(p);
    }
    for s in x.arg_sort_refs {
        v.visit_sort_ref(s);
    }
    v.visit_sort_ref(x.sort_ref);
}

pub fn visit_sort_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a SortDecl<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    for p in x.params {
        v.visit_generic_param(p);
    }
    if let Some(e) = x.extends {
        for s in e.1 {
            v.visit_sort_ref(s);
        }
    }
}

pub fn visit_predicate_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a PredicateDecl<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    for p in x.params {
        v.visit_generic_param(p);
    }
    for s in x.arg_sort_refs {
        v.visit_sort_ref(s);
    }
}

pub fn visit_sort_ref<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a SortRef) {
    v.visit_id(&x.id);
    v.visit_path(x.path);
    for a in x.args {
        v.visit_generic_arg(a);
    }
}

pub fn visit_generic_arg<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GenericArg<'a>) {
    match x {
        GenericArg::Sort(sort_ref) => v.visit_sort_ref(sort_ref),
        GenericArg::Const(term, ..) => v.visit_term(term),
    }
}

pub fn visit_generic_param<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GenericParam<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    v.visit_generic_param_kind(&x.kind);
}

pub fn visit_generic_param_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GenericParamKind<'a>) {
    if let GenericParamKind::Const { sort, .. } = x {
        v.visit_sort_ref(sort)
    }
}

pub fn visit_term<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Term<'a>) {
    v.visit_id(&x.id);
    v.visit_term_kind(&x.kind);
}

pub fn visit_term_kind<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a TermKind<'a>) {
    match x {
        TermKind::Path(path, generic_args) => {
            v.visit_path(path);
            for a in *generic_args {
                v.visit_generic_arg(a);
            }
        }
        TermKind::Call(path, generic_args, terms) => {
            v.visit_path(path);
            for a in *generic_args {
                v.visit_generic_arg(a);
            }
            for t in *terms {
                v.visit_term(t);
            }
        }
    }
}

pub fn visit_rule<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Rule<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    for sv in x.schema_vars {
        v.visit_schema_var_decl(sv);
    }
    if let Some(a) = &x.assumes {
        v.visit_term_or_seq(&a.1);
    }
    if let Some(a) = &x.find {
        v.visit_term_or_seq(&a.1);
    }
    v.visit_goal_specs(&x.goal_specs);
    if let Some(a) = &x.rule_sets {
        v.visit_rule_sets(a.1);
    }
    if let Some(i) = &x.display_name {
        v.visit_ident(i);
    }
}

pub fn visit_schema_var_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a SchemaVarDecl<'a>) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
    v.visit_sort_ref(x.sort);
}

pub fn visit_term_or_seq<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a TermOrSeq<'a>) {
    match x {
        TermOrSeq::Term(term) => v.visit_term(*term),
        TermOrSeq::Seq(seq) => v.visit_seq(*seq),
    }
}

pub fn visit_seq<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a Seq<'a>) {
    v.visit_id(&x.id);
    for t in x.ante {
        v.visit_term(t);
    }
    for t in x.succ {
        v.visit_term(t);
    }
}

pub fn visit_goal_specs<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GoalSpecs<'a>) {
    match x {
        GoalSpecs::CloseGoal(ir_id, ..) => v.visit_id(ir_id),
        GoalSpecs::Specs(goal_specs, ..) => {
            for s in *goal_specs {
                v.visit_goal_spec(s);
            }
        }
    }
}

pub fn visit_goal_spec<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a GoalSpec<'a>) {
    v.visit_id(&x.id);
    if let Some(name) = &x.name {
        v.visit_ident(name);
    }
    if let Some(a) = &x.replace_with {
        v.visit_term_or_seq(&a);
    }
    if let Some(a) = &x.add {
        v.visit_term_or_seq(&a);
    }
}

pub fn visit_rule_sets<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a [Path<'a>]) {
    for p in x {
        v.visit_path(p);
    }
}

pub fn visit_rule_set_decl<'a, V: Visit<'a> + ?Sized>(v: &mut V, x: &'a RuleSetDecl) {
    v.visit_def_id(&x.id);
    v.visit_ident(&x.name);
}
