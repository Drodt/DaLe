use indexmap::IndexMap;

use crate::{
    ctx::Ctx,
    ir,
    raw_theory::{Ident, NodeId, Visit},
};

type Res = ir::Res<NodeId>;

pub struct Scope<R = Res> {
    pub bindings: IndexMap<Ident, R>,
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

pub enum ScopeKind {
    Normal,
    File,
    Theory,
    PossiblyGenericDecl,
    Rule,
}

pub struct PerNS<T> {
    pub op_ns: T,
    pub sv_ns: T,
    pub theory_ns: T,
    pub rule_set_ns: T,
}

impl<T> PerNS<T> {
    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> PerNS<U> {
        PerNS {
            op_ns: f(self.op_ns),
            sv_ns: f(self.sv_ns),
            theory_ns: f(self.theory_ns),
            rule_set_ns: f(self.rule_set_ns),
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
            sv_ns: Default::default(),
            theory_ns: Default::default(),
            rule_set_ns: Default::default(),
        }
    }
}

pub struct ScopeBuilder<'cx> {
    cx: Ctx<'cx>,
    scopes: PerNS<Vec<Scope>>,
}

impl<'cx> ScopeBuilder<'cx> {
    fn op_ns_opt_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.op_ns.last_mut()
    }

    fn op_ns_mut(&mut self) -> &mut Scope {
        self.op_ns_opt_mut()
            .expect("operator/function/predicate scopes are not empty")
    }

    fn sv_ns_opt_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.sv_ns.last_mut()
    }

    fn sv_ns_mut(&mut self) -> &mut Scope {
        self.sv_ns_opt_mut()
            .expect("schema var scopes are not empty")
    }

    fn theory_ns_opt_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.theory_ns.last_mut()
    }

    fn theory_ns_mut(&mut self) -> &mut Scope {
        self.theory_ns_opt_mut()
            .expect("theory scopes are not empty")
    }

    fn add_theory(&mut self, ident: Ident, id: NodeId) {
        match self.theory_ns_mut().bindings.entry(ident.clone()) {
            indexmap::map::Entry::Occupied(_) => {
                panic!(
                    "Theory of name {} is already declared",
                    self.cx.interner.get(ident.node)
                )
            }
            indexmap::map::Entry::Vacant(v) => v.insert(id),
        };
    }
}

impl<'a, 'cx> Visit<'a> for ScopeBuilder<'cx> {
    fn visit_file(&mut self, _: &'a crate::raw_theory::File) {
        self.scopes = PerNS::default().map(|mut s: Vec<Scope<_>>| {
            s.push(Scope::new(ScopeKind::File));
            s
        });
    }

    fn visit_theory(&mut self, x: &'a crate::raw_theory::Theory) {}
}
