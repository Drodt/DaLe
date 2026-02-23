use std::collections::{HashMap, hash_map::Entry};

use dale_util::arena::DroplessArena;

use crate::ir::{
    self, Def, DefId, DefKind, FunctionDecl, GenericArg, GenericParam, GenericParamKind, IrId, Map,
    OperatorDecl, PredicateDecl, Res, SchemaVarDecl, SortDecl, SortModifiers, SortRef, Span, Term,
    TermKind,
    visit::{Visit, visit_item, visit_sort_decl, visit_term},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sort<'ir> {
    pub def_id: DefId,
    pub modifiers: SortModifiers,
    pub args: &'ir [GenArg<'ir>],
    pub extends: Option<&'ir [Sort<'ir>]>,
}

impl<'ir> Sort<'ir> {
    pub fn sub_sort(&self, s: &Sort<'ir>) -> bool {
        if s.modifiers.top || self == s {
            return true;
        }
        match self.extends {
            Some(e) => e.iter().any(|sort| sort.sub_sort(s)),
            None => false,
        }
    }
}

impl<'ir> Sort<'ir> {
    pub fn new(
        def_id: DefId,
        args: &'ir [GenArg<'ir>],
        modifiers: SortModifiers,
        extends: Option<&'ir [Sort<'ir>]>,
    ) -> Self {
        Self {
            def_id,
            args,
            modifiers,
            extends,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenArg<'ir> {
    Sort(Sort<'ir>),
    Term(Term<'ir>),
}

pub fn check_ir<'ir>(map: &'ir mut Map<'ir>, arena: &'ir DroplessArena) -> Vec<CheckErr> {
    let mut checker = Checker::new(map, arena);
    checker.visit_file(&map.file);
    checker.errors
}

#[derive(Debug, Clone, Copy)]
pub enum CheckErr {
    IncorrentNumberOfGenArgs(IrId, DefId),
    IncorrentNumberOfArgs(IrId, DefId),
    ArgKindMismatch(IrId, DefId, usize),
    ConstArgNotSubsort(IrId, DefId, usize),
    CallArgNotSubsort(IrId, DefId, usize),
    MultipleFormulaSorts(DefId, DefId),
}

impl CheckErr {
    pub fn format(&self) -> String {
        match self {
            CheckErr::IncorrentNumberOfGenArgs(ir_id, def_id) => {
                format!("Incorrect number of generic arguments.")
            }
            CheckErr::IncorrentNumberOfArgs(ir_id, def_id) => {
                format!("Incorrect number of arguments.")
            }
            CheckErr::ArgKindMismatch(ir_id, def_id, _) => format!("Generic arguement mismatch"),
            CheckErr::ConstArgNotSubsort(ir_id, def_id, _) => {
                format!("Const generic argument is not subsort of parameter sort")
            }
            CheckErr::CallArgNotSubsort(ir_id, def_id, _) => {
                format!("Call argument is not subsort")
            }
            CheckErr::MultipleFormulaSorts(new_, old) => {
                format!("Multiple formula sort definitions")
            }
        }
    }

    pub fn id(&self) -> Option<IrId> {
        match self {
            CheckErr::IncorrentNumberOfGenArgs(ir_id, ..) => Some(*ir_id),
            CheckErr::IncorrentNumberOfArgs(ir_id, ..) => Some(*ir_id),
            CheckErr::ArgKindMismatch(ir_id, ..) => Some(*ir_id),
            CheckErr::ConstArgNotSubsort(ir_id, ..) => Some(*ir_id),
            CheckErr::CallArgNotSubsort(ir_id, ..) => Some(*ir_id),
            _ => None,
        }
    }
}

struct Checker<'ir> {
    map: &'ir Map<'ir>,
    arena: &'ir DroplessArena,
    ir_id_to_sort: HashMap<IrId, Sort<'ir>>,
    def_id_args_to_sort: HashMap<(DefId, &'ir [GenArg<'ir>]), Sort<'ir>>,
    formula_sort: Option<SortDecl<'ir>>,
    errors: Vec<CheckErr>,
}

impl<'ir> Checker<'ir> {
    fn new(map: &'ir Map<'ir>, arena: &'ir DroplessArena) -> Self {
        Self {
            map,
            arena,
            ir_id_to_sort: Default::default(),
            def_id_args_to_sort: Default::default(),
            formula_sort: Default::default(),
            errors: Default::default(),
        }
    }

    fn term_sort(&mut self, t: &'ir Term<'ir>) -> Sort<'ir> {
        if let Some(s) = self.ir_id_to_sort.get(&t.id) {
            return *s;
        }

        let (path, args, subs) = match t.kind {
            TermKind::Path(path, generic_args) => (path, generic_args, None),
            TermKind::Call(path, generic_args, terms) => (path, generic_args, Some(terms)),
        };
        let Res::Def(_, def_id) = path.res else {
            panic!("{t:?}")
        };
        let (params, sort_ref, arg_sort_refs) = match self.map.get_def(def_id) {
            Def::SchemaVar(SchemaVarDecl { sort, .. })
            | Def::GenParam(GenericParam {
                kind: GenericParamKind::Const { sort, .. },
                ..
            }) => (None, Some(sort), None),
            Def::FnDecl(FunctionDecl {
                params,
                sort_ref,
                arg_sort_refs,
                ..
            })
            | Def::OpDecl(OperatorDecl {
                params,
                sort_ref,
                arg_sort_refs,
                ..
            }) => (Some(params), Some(sort_ref), Some(arg_sort_refs)),
            Def::PredDecl(PredicateDecl {
                params,
                arg_sort_refs,
                ..
            }) => (Some(params), None, Some(arg_sort_refs)),
            d => panic!("Expected operator, got {d:?}"),
        };
        let args = self.convert_args(args);
        if let Some(params) = params {
            self.check_generic_args(t.id, def_id, args, params);
        }

        match (subs, arg_sort_refs) {
            (None, None) => {}
            (Some(s), None) => {
                if !s.is_empty() {
                    self.errors
                        .push(CheckErr::IncorrentNumberOfArgs(t.id, def_id))
                }
            }
            (None, Some(a)) => {
                if !a.is_empty() {
                    self.errors
                        .push(CheckErr::IncorrentNumberOfArgs(t.id, def_id))
                }
            }
            (Some(subs), Some(srs)) => {
                if subs.len() != srs.len() {
                    self.errors
                        .push(CheckErr::IncorrentNumberOfArgs(t.id, def_id));
                }
                for (idx, (s, r)) in subs.iter().zip(srs.iter()).enumerate() {
                    let ts = self.term_sort(s);
                    let rs = self.ref_to_sort(r);
                    if !ts.sub_sort(&rs) {
                        self.errors
                            .push(CheckErr::CallArgNotSubsort(t.id, def_id, idx));
                    }
                }
            }
        }

        let sort = self.ref_to_sort(sort_ref.unwrap());
        self.ir_id_to_sort.insert(t.id, sort);
        sort
    }

    fn check_generic_args(
        &mut self,
        ir_id: IrId,
        def_id: DefId,
        args: &'ir [GenArg<'ir>],
        params: &'ir [GenericParam<'ir>],
    ) {
        for (idx, (a, p)) in args.iter().zip(params.iter()).enumerate() {
            match (a, p.kind) {
                (GenArg::Sort(_), GenericParamKind::Sort) => {}
                (GenArg::Sort(_), GenericParamKind::Const { .. })
                | (GenArg::Term(_), GenericParamKind::Sort) => self
                    .errors
                    .push(CheckErr::ArgKindMismatch(ir_id, def_id, idx)),
                (GenArg::Term(term), GenericParamKind::Const { sort: sr, .. }) => {
                    let ts = self.term_sort(term);
                    let sort = self.ref_to_sort(sr);
                    if !ts.sub_sort(&sort) {
                        self.errors
                            .push(CheckErr::ConstArgNotSubsort(ir_id, def_id, idx));
                    }
                }
            }
        }
    }

    fn ref_to_sort(&mut self, r: &'ir SortRef<'ir>) -> Sort<'ir> {
        let res = r.path.res;
        let Res::Def(DefKind::Sort, def_id) = res else {
            panic!("Encountered error")
        };
        let args = self.convert_args(r.args);
        if let Some(e) = self.def_id_args_to_sort.get(&(def_id, args)) {
            return *e;
        }

        let decl = self.map.get_sort(def_id);
        if args.len() != decl.params.len() {
            self.errors
                .push(CheckErr::IncorrentNumberOfGenArgs(r.id, def_id));
        }
        let extends = if let Some((_, e)) = decl.extends {
            Some(
                self.arena
                    .alloc_from_iter(e.iter().map(|s| self.ref_to_sort(s)))
                    as &[Sort<'ir>],
            )
        } else {
            None
        };
        let result = Sort::new(def_id, args, decl.modifiers, extends);
        self.def_id_args_to_sort.insert((def_id, args), result);
        self.check_generic_args(r.id, def_id, args, decl.params);
        result
    }

    fn convert_args(&mut self, args: &'ir [GenericArg<'ir>]) -> &'ir [GenArg<'ir>] {
        self.arena.alloc_from_iter(args.iter().map(|a| match a {
            GenericArg::Sort(sort_ref) => GenArg::Sort(self.ref_to_sort(*sort_ref)),
            GenericArg::Const(term, _) => GenArg::Term(**term),
        }))
    }
}

impl<'ir> Visit<'ir> for Checker<'ir> {
    fn visit_sort_decl(&mut self, x: &'ir SortDecl<'ir>) {
        if x.modifiers.formula {
            if let Some(old) = self.formula_sort {
                self.errors
                    .push(CheckErr::MultipleFormulaSorts(x.id, old.id));
            }
            self.formula_sort = Some(*x);
        }
        visit_sort_decl(self, x);
    }

    fn visit_operator_decl(&mut self, x: &'ir OperatorDecl<'ir>) {
        // Must use formula sort as an arg; otherwise, why not just a function/predicate?
    }

    fn visit_predicate_decl(&mut self, x: &'ir PredicateDecl<'ir>) {
        // Must not use formula as arg; should be an operator, then
    }

    fn visit_function_decl(&mut self, x: &'ir FunctionDecl<'ir>) {
        // Must not use formula as arg or result; should be an operator, then
    }

    fn visit_term(&mut self, x: &'ir Term<'ir>) {
        visit_term(self, x);
        self.term_sort(x);
    }

    fn visit_item_id(&mut self, x: &'ir ir::ItemId) {
        self.visit_item(self.map.items[x]);
    }
}
