use std::collections::{HashMap, hash_map::Entry};

use dale_util::arena::DroplessArena;

use crate::ir::{
    DefId, DefKind, GenericArg, IrId, Map, Res, SortDecl, SortRef, Term, visit::Visit,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sort<'ir> {
    pub def_id: DefId,
    pub args: &'ir [GenArg<'ir>],
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

struct Checker<'ir> {
    map: Map<'ir>,
    arena: &'ir DroplessArena,
    ir_id_to_sort: HashMap<IrId, Sort<'ir>>,
    def_id_args_to_sort: HashMap<(DefId, &'ir [GenArg<'ir>]), Sort<'ir>>,
}

impl<'ir> Checker<'ir> {
    fn ref_to_sort(&mut self, r: &'ir SortRef<'ir>) -> Sort<'ir> {
        let res = r.path.res;
        let Res::Def(DefKind::Sort, def_id) = res else {
            panic!("Encountered error")
        };
        let args = self.convert_args(r.args);
        match self.def_id_args_to_sort.entry((def_id, args)) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => *e.insert(Sort::new(def_id, args)),
        }
    }

    fn convert_args(&mut self, args: &'ir [GenericArg<'ir>]) -> &'ir [GenArg<'ir>] {
        self.arena.alloc_from_iter(args.iter().map(|a| match a {
            GenericArg::Sort(sort_ref) => GenArg::Sort(self.ref_to_sort(*sort_ref)),
            GenericArg::Const(term, _) => GenArg::Term(**term),
        }))
    }
}

impl<'ir> Visit<'ir> for Checker<'ir> {}
