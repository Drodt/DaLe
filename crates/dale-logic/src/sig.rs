//! Declarations: the immutable description of the sorts and operators a logic offers.
//!
//! A [`Signature`] only contains *declarations*, which may be generic (`Seq<E>`,
//! `ifThenElse<S>`). Only instantiated sorts and operators exist in a [`TermPool`]
//! (`Seq<int>`, `ifThenElse<int>`); a pool instantiates declarations on demand.
//!
//! [`TermPool`]: crate::TermPool

use dale_util::symbol::Symbol;

use crate::ids::{OpDeclId, SortDeclId};

/// The kind of a generic parameter.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenKind {
    /// Instantiated with a sort.
    Sort,
    /// `const N: int`: instantiated with a closed term of the given sort.
    // TODO: the sort of a const parameter is only checked when instantiating; also decide whether
    // constants should be restricted to literals instead of arbitrary closed terms.
    Const(SortPat),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenParam {
    pub name: Symbol,
    pub kind: GenKind,
}

/// A sort in a declaration, which may mention the generic parameters of the declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SortPat {
    /// The i-th generic parameter, which must be a sort parameter.
    Param(u32),
    /// A declared sort applied to arguments (none for a non-generic sort).
    Inst {
        decl: SortDeclId,
        args: Vec<GenArgPat>,
    },
}

impl SortPat {
    /// A non-generic sort.
    pub fn simple(decl: SortDeclId) -> Self {
        SortPat::Inst {
            decl,
            args: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenArgPat {
    Sort(SortPat),
    /// The i-th generic parameter, which must be a const parameter.
    // TODO: allow literal terms here, e.g. `Array<E, 3>` in a declaration.
    Const(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SortKind {
    Normal,
    /// `\top`: the one sort every sort is a subsort of.
    Top,
    /// `\formula`: the sort of formulas.
    Formula,
}

#[derive(Clone, Debug)]
pub struct SortDecl {
    pub name: Symbol,
    pub kind: SortKind,
    pub params: Vec<GenParam>,
    /// The direct supersorts; they may use the parameters of this declaration. Every sort is
    /// implicitly a subsort of the top sort.
    // TODO: restrict `extends` to a single sort (the hierarchy becomes a tree).
    pub extends: Vec<SortPat>,
}

impl SortDecl {
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            kind: SortKind::Normal,
            params: Vec::new(),
            extends: Vec::new(),
        }
    }
}

/// The kind of a meta argument of an operator (`\meta NUMBER`). Meta arguments are part of the
/// operator, not subterms: `var<int>(0)` and `var<int>(1)` are different operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MetaKind {
    /// A `u64`.
    Nat,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MetaValue {
    Nat(u64),
}

impl MetaValue {
    pub fn kind(self) -> MetaKind {
        match self {
            MetaValue::Nat(_) => MetaKind::Nat,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpKind {
    Normal,
    /// The bound variable operator (`var<S>(NUMBER) -> S`): one sort parameter, one `Nat` meta
    /// argument (the de Bruijn index; 0 is the closest enclosing binder), no subterms.
    BoundVar,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct OpFlags {
    pub rigid: bool,
    pub unique: bool,
    pub skolem: bool,
}

/// An operator that binds a variable (`all<S>{0}(Formula)`): the variable is bound in the
/// subterm at index `arg`, and has sort `var_sort`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub arg: u32,
    // TODO: the theory syntax does not state the sort of the bound variable; by convention it
    // would be the first generic parameter.
    pub var_sort: SortPat,
}

#[derive(Clone, Debug)]
pub struct OpDecl {
    pub name: Symbol,
    pub kind: OpKind,
    pub flags: OpFlags,
    pub generics: Vec<GenParam>,
    pub meta: Vec<MetaKind>,
    /// The sorts of the subterms; their number is the arity.
    pub args: Vec<SortPat>,
    pub result: SortPat,
    pub bind: Option<Binding>,
}

impl OpDecl {
    pub fn new(name: Symbol, args: Vec<SortPat>, result: SortPat) -> Self {
        Self {
            name,
            kind: OpKind::Normal,
            flags: OpFlags::default(),
            generics: Vec::new(),
            meta: Vec::new(),
            args,
            result,
            bind: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SigError {
    TopRedeclared,
    FormulaRedeclared,
    MissingTop,
    MissingFormula,
    /// `\top` and `\formula` sorts cannot be generic or extend anything.
    SpecialSortShape(Symbol),
    /// A pattern mentions a sort declared later (or itself), an unknown parameter, a parameter
    /// of the wrong kind, or applies a sort to the wrong number or kind of arguments.
    BadSortPattern(Symbol),
    /// A binding for an argument that does not exist.
    BindingOutOfRange(Symbol),
    /// A bound variable operator that does not have the required shape.
    BadBoundVar(Symbol),
}

/// The declarations of a logic. Built with a [`SignatureBuilder`], immutable afterwards.
#[derive(Debug)]
pub struct Signature {
    sorts: Vec<SortDecl>,
    ops: Vec<OpDecl>,
    top: SortDeclId,
    formula: SortDeclId,
}

impl Signature {
    pub fn sort_decl(&self, id: SortDeclId) -> &SortDecl {
        &self.sorts[id.index()]
    }

    pub fn op_decl(&self, id: OpDeclId) -> &OpDecl {
        &self.ops[id.index()]
    }

    pub fn top(&self) -> SortDeclId {
        self.top
    }

    pub fn formula(&self) -> SortDeclId {
        self.formula
    }

    pub fn sort_decls(&self) -> impl Iterator<Item = (SortDeclId, &SortDecl)> {
        self.sorts
            .iter()
            .enumerate()
            .map(|(i, d)| (SortDeclId::from_u32(i as u32), d))
    }

    pub fn op_decls(&self) -> impl Iterator<Item = (OpDeclId, &OpDecl)> {
        self.ops
            .iter()
            .enumerate()
            .map(|(i, d)| (OpDeclId::from_u32(i as u32), d))
    }

    /// The first declared bound variable operator, if any.
    pub fn bound_var(&self) -> Option<OpDeclId> {
        self.op_decls()
            .find(|(_, d)| d.kind == OpKind::BoundVar)
            .map(|(id, _)| id)
    }
}

#[derive(Default)]
pub struct SignatureBuilder {
    sorts: Vec<SortDecl>,
    ops: Vec<OpDecl>,
    top: Option<SortDeclId>,
    formula: Option<SortDeclId>,
}

impl SignatureBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Declares a sort. Sorts can only refer to sorts declared before them, which keeps the
    /// hierarchy free of cycles.
    pub fn add_sort(&mut self, decl: SortDecl) -> Result<SortDeclId, SigError> {
        let id = SortDeclId::from_u32(self.sorts.len() as u32);
        match decl.kind {
            SortKind::Normal => {}
            SortKind::Top | SortKind::Formula => {
                if !decl.params.is_empty() || !decl.extends.is_empty() {
                    return Err(SigError::SpecialSortShape(decl.name));
                }
                let slot = if decl.kind == SortKind::Top {
                    (&mut self.top, SigError::TopRedeclared)
                } else {
                    (&mut self.formula, SigError::FormulaRedeclared)
                };
                if slot.0.is_some() {
                    return Err(slot.1);
                }
                *slot.0 = Some(id);
            }
        }
        self.check_params(decl.name, &decl.params)?;
        for e in &decl.extends {
            self.check_pat(decl.name, e, &decl.params, decl.params.len())?;
        }
        self.sorts.push(decl);
        Ok(id)
    }

    /// Declares an operator.
    pub fn add_op(&mut self, decl: OpDecl) -> Result<OpDeclId, SigError> {
        let n = decl.generics.len();
        self.check_params(decl.name, &decl.generics)?;
        for a in decl.args.iter().chain([&decl.result]) {
            self.check_pat(decl.name, a, &decl.generics, n)?;
        }
        if let Some(b) = &decl.bind {
            if b.arg as usize >= decl.args.len() {
                return Err(SigError::BindingOutOfRange(decl.name));
            }
            self.check_pat(decl.name, &b.var_sort, &decl.generics, n)?;
        }
        if decl.kind == OpKind::BoundVar {
            let ok = decl.generics.len() == 1
                && decl.generics[0].kind == GenKind::Sort
                && decl.meta == [MetaKind::Nat]
                && decl.args.is_empty()
                && decl.result == SortPat::Param(0)
                && decl.bind.is_none();
            if !ok {
                return Err(SigError::BadBoundVar(decl.name));
            }
        }
        let id = OpDeclId::from_u32(self.ops.len() as u32);
        self.ops.push(decl);
        Ok(id)
    }

    /// The sort of a predicate: formulas. Fails if no formula sort has been declared yet.
    pub fn formula_pat(&self) -> Result<SortPat, SigError> {
        self.formula
            .map(SortPat::simple)
            .ok_or(SigError::MissingFormula)
    }

    pub fn finish(self) -> Result<Signature, SigError> {
        Ok(Signature {
            top: self.top.ok_or(SigError::MissingTop)?,
            formula: self.formula.ok_or(SigError::MissingFormula)?,
            sorts: self.sorts,
            ops: self.ops,
        })
    }

    /// Const parameters may name the sort of the constant, using the parameters before them.
    fn check_params(&self, name: Symbol, params: &[GenParam]) -> Result<(), SigError> {
        for (i, p) in params.iter().enumerate() {
            if let GenKind::Const(s) = &p.kind {
                self.check_pat(name, s, params, i)?;
            }
        }
        Ok(())
    }

    /// Checks a sort pattern; only the first `visible` parameters may be used.
    fn check_pat(
        &self,
        name: Symbol,
        pat: &SortPat,
        params: &[GenParam],
        visible: usize,
    ) -> Result<(), SigError> {
        let bad = || SigError::BadSortPattern(name);
        match pat {
            SortPat::Param(i) => match params.get(*i as usize) {
                Some(p) if (*i as usize) < visible && p.kind == GenKind::Sort => Ok(()),
                _ => Err(bad()),
            },
            SortPat::Inst { decl, args } => {
                let d = self.sorts.get(decl.index()).ok_or_else(bad)?;
                if d.params.len() != args.len() {
                    return Err(bad());
                }
                for (p, a) in d.params.iter().zip(args) {
                    match (&p.kind, a) {
                        (GenKind::Sort, GenArgPat::Sort(s)) => {
                            self.check_pat(name, s, params, visible)?
                        }
                        (GenKind::Const(_), GenArgPat::Const(i)) => match params.get(*i as usize) {
                            Some(q)
                                if (*i as usize) < visible
                                    && matches!(q.kind, GenKind::Const(_)) => {}
                            _ => return Err(bad()),
                        },
                        _ => return Err(bad()),
                    }
                }
                Ok(())
            }
        }
    }
}
