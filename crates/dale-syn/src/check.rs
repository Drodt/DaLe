use std::collections::{HashMap, hash_map::Entry};

use dale_util::arena::DroplessArena;

use crate::ir::{
    self, Def, DefId, DefKind, FunctionDecl, GenericArg, GenericParam, GenericParamKind, IrId, Map,
    OperatorDecl, PredicateDecl, Res, SortDecl, SortRef, Span, Term, TermKind,
    visit::{Visit, visit_term},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sort<'ir> {
    pub def_id: DefId,
    pub args: &'ir [GenArg<'ir>],
}

impl<'ir> Sort<'ir> {
    pub fn sub_sort(&self, s: &Sort<'ir>) -> bool {
        todo!()
    }
}

impl<'ir> Sort<'ir> {
    pub fn new(def_id: DefId, args: &'ir [GenArg<'ir>]) -> Self {
        Self { def_id, args }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenArg<'ir> {
    Sort(Sort<'ir>),
    Term(Term<'ir>),
}

pub fn check_ir<'ir>(map: Map<'ir>) {}

pub enum CheckErr {
    IncorrentNumberOfGenArgs(IrId, DefId),
    IncorrentNumberOfArgs(IrId, DefId),
    ArgKindMismatch(IrId, DefId, usize),
    ConstArgNotSubsort(IrId, DefId, usize),
    CallArgNotSubsort(IrId, DefId, usize),
}

struct Checker<'ir> {
    map: Map<'ir>,
    arena: &'ir DroplessArena,
    ir_id_to_sort: HashMap<IrId, Sort<'ir>>,
    def_id_args_to_sort: HashMap<(DefId, &'ir [GenArg<'ir>]), Sort<'ir>>,
    errors: Vec<CheckErr>,
}

impl<'ir> Checker<'ir> {
    fn term_sort(&mut self, t: &'ir Term<'ir>) -> Sort<'ir> {
        if let Some(s) = self.ir_id_to_sort.get(&t.id) {
            return *s;
        }

        let (path, args, subs) = match t.kind {
            TermKind::Path(path, generic_args) => (path, generic_args, None),
            TermKind::Call(path, generic_args, terms) => (path, generic_args, Some(terms)),
        };
        let Res::Def(_, def_id) = path.res else {
            panic!()
        };
        let (params, sort_ref, arg_sort_refs) = match self.map.get_def(def_id) {
            Def::SchemaVar(d) => (None, Some(d.sort), None),
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
            _ => panic!("Expected operator"),
        };
        let args = self.convert_args(args);
        if let Some(params) = params {
            self.check_generic_args(t.id, def_id, args, params);
        }

        match (subs, arg_sort_refs) {
            (None, None) => {}
            (Some(_), None) | (None, Some(_)) => self
                .errors
                .push(CheckErr::IncorrentNumberOfArgs(t.id, def_id)),
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
        match self.def_id_args_to_sort.entry((def_id, args)) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let decl = self.map.get_sort(def_id);
                if args.len() != decl.params.len() {
                    self.errors
                        .push(CheckErr::IncorrentNumberOfGenArgs(r.id, def_id));
                }
                let result = Sort::new(def_id, args);
                e.insert(result);
                self.check_generic_args(r.id, def_id, args, decl.params);
                result
            }
        }
    }

    fn convert_args(&mut self, args: &'ir [GenericArg<'ir>]) -> &'ir [GenArg<'ir>] {
        self.arena.alloc_from_iter(args.iter().map(|a| match a {
            GenericArg::Sort(sort_ref) => GenArg::Sort(self.ref_to_sort(*sort_ref)),
            GenericArg::Const(term, _) => GenArg::Term(**term),
        }))
    }
}

impl<'ir> Visit<'ir> for Checker<'ir> {
    fn visit_term(&mut self, x: &'ir Term<'ir>) {
        visit_term(self, x);
        self.term_sort(x);
    }
}
