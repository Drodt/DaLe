//! Rule patterns: what the `\find` and `\assumes` parts of a rule look like.
//!
//! Patterns are a separate layer on top of the term pool, not terms with schema variable
//! operators: the pool only holds *instantiated* sorts and operators, while a pattern has to say
//! "any `all<S>`" or "a term of any subsort of `int`". Subpatterns without schema variables are
//! interned terms ([`PatNode::Ground`]) and match by index comparison.
//!
//! A [`Rule`] owns one arena of [`PatNode`]s shared by the find and assumes patterns, so that all
//! of its schema variables and generic slots are numbered once.
//!
//! Binders: patterns use de Bruijn indices like terms. A schema variable under `d` pattern
//! binders may match a term that mentions these binders (the permissive KeY semantics); a rule
//! that must not allow that says so with [`Cond::NoFreeVar`].

use std::sync::Arc;

use dale_util::symbol::Symbol;

use crate::ids::{OpDeclId, SortIdx, TermIdx};
use crate::pool::GenArg;
use crate::sequent::Side;
use crate::sig::{MetaValue, Signature};

/// A schema variable of a rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SvId(pub u32);

/// A node of a rule's pattern arena.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PatId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SvKind {
    /// Matches terms that are not formulas.
    Term,
    /// Matches formulas.
    Formula,
}

/// Restriction on the sort of what a schema variable matches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SortReq {
    Any,
    /// The sort of the matched term is this sort or a subsort of it.
    Sub(SortIdx),
    /// The sort is bound to (or must equal) generic slot `i`; sorts are invariant, so equality.
    Slot(u32),
}

#[derive(Clone, Debug)]
pub struct SvDecl {
    pub name: Symbol,
    pub kind: SvKind,
    pub sort: SortReq,
}

/// A generic argument of an operator pattern.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenPat {
    Exact(GenArg),
    /// Binds generic slot `i` on first use, must be equal on later ones.
    Slot(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MetaPat {
    Exact(MetaValue),
    Any,
}

#[derive(Clone, Debug)]
pub enum PatNode {
    Sv(SvId),
    /// A term without schema variables.
    Ground(TermIdx),
    App {
        decl: OpDeclId,
        gens: Box<[GenPat]>,
        meta: Box<[MetaPat]>,
        args: Box<[PatId]>,
        /// The argument that is under the operator's binder, from the declaration.
        bind_arg: Option<u32>,
    },
}

/// Where a rule's `\find` looks.
#[derive(Clone, Copy, Debug)]
pub enum Find {
    /// Any subterm (or formula) of the sequent; the position is chosen by the caller.
    Term(PatId),
    /// `\find(a ==> b)`: top-level formulas of the antecedent and/or succedent.
    // TODO: updates (`\sameUpdateLevel`, `\ignoreUpdateLevel`) are not modelled yet.
    Sequent {
        ante: Option<PatId>,
        succ: Option<PatId>,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct Assumes {
    pub side: Side,
    pub pat: PatId,
}

/// Conditions that are checked after the patterns matched.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub enum Cond {
    /// The variable with de Bruijn index `index` does not occur free in what `sv` matched. The
    /// index is relative to the position of `sv` in the pattern: `\all(phi)` with `NoFreeVar { phi,
    /// 0 }` is "the variable bound by this quantifier is unused in `phi`".
    NoFreeVar { sv: SvId, index: u32 },
}

#[derive(Debug)]
pub struct Rule {
    pub name: Symbol,
    pub(crate) svs: Vec<SvDecl>,
    pub(crate) slots: u32,
    pub(crate) nodes: Vec<PatNode>,
    pub(crate) find: Find,
    pub(crate) assumes: Vec<Assumes>,
    pub(crate) conds: Vec<Cond>,
}

impl Rule {
    pub fn sv(&self, id: SvId) -> &SvDecl {
        &self.svs[id.0 as usize]
    }
    pub fn num_svs(&self) -> usize {
        self.svs.len()
    }
    pub fn num_slots(&self) -> usize {
        self.slots as usize
    }
    pub fn find(&self) -> Find {
        self.find
    }
    pub fn assumes(&self) -> &[Assumes] {
        &self.assumes
    }
    pub fn conds(&self) -> &[Cond] {
        &self.conds
    }
    pub fn node(&self, id: PatId) -> &PatNode {
        &self.nodes[id.0 as usize]
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternError {
    ArgCount {
        expected: usize,
        found: usize,
    },
    GenCount {
        expected: usize,
        found: usize,
    },
    MetaCount {
        expected: usize,
        found: usize,
    },
    UnknownSv(SvId),
    UnknownNode(PatId),
    /// A condition mentions a schema variable that occurs in no pattern.
    UnboundInCond(SvId),
    /// `Find` is a sequent find without any formula.
    EmptyFind,
    /// The rule has no `\find`.
    NoFind,
}

/// Builds a [`Rule`], checking each node against the signature.
pub struct RuleBuilder {
    sig: Arc<Signature>,
    name: Symbol,
    svs: Vec<SvDecl>,
    nodes: Vec<PatNode>,
    slots: u32,
    find: Option<Find>,
    assumes: Vec<Assumes>,
    conds: Vec<Cond>,
}

impl RuleBuilder {
    pub fn new(sig: Arc<Signature>, name: Symbol) -> Self {
        Self {
            sig,
            name,
            svs: Vec::new(),
            nodes: Vec::new(),
            slots: 0,
            find: None,
            assumes: Vec::new(),
            conds: Vec::new(),
        }
    }

    /// Declares a schema variable.
    pub fn declare_sv(&mut self, name: Symbol, kind: SvKind, sort: SortReq) -> SvId {
        self.note_sort(sort);
        self.svs.push(SvDecl { name, kind, sort });
        SvId(self.svs.len() as u32 - 1)
    }

    /// A pattern node for a schema variable.
    pub fn sv(&mut self, sv: SvId) -> Result<PatId, PatternError> {
        if sv.0 as usize >= self.svs.len() {
            return Err(PatternError::UnknownSv(sv));
        }
        Ok(self.push(PatNode::Sv(sv)))
    }

    /// A ground subpattern.
    pub fn ground(&mut self, t: TermIdx) -> PatId {
        self.push(PatNode::Ground(t))
    }

    /// An operator application pattern. `gens` and `meta` must have the declaration's lengths
    /// (use `[]` for none).
    pub fn app(
        &mut self,
        decl: OpDeclId,
        gens: &[GenPat],
        meta: &[MetaPat],
        args: &[PatId],
    ) -> Result<PatId, PatternError> {
        let d = self.sig.op_decl(decl);
        if d.generics.len() != gens.len() {
            return Err(PatternError::GenCount {
                expected: d.generics.len(),
                found: gens.len(),
            });
        }
        if d.meta.len() != meta.len() {
            return Err(PatternError::MetaCount {
                expected: d.meta.len(),
                found: meta.len(),
            });
        }
        if d.args.len() != args.len() {
            return Err(PatternError::ArgCount {
                expected: d.args.len(),
                found: args.len(),
            });
        }
        for a in args {
            if a.0 as usize >= self.nodes.len() {
                return Err(PatternError::UnknownNode(*a));
            }
        }
        for g in gens {
            if let GenPat::Slot(i) = g {
                self.slots = self.slots.max(i + 1);
            }
        }
        let bind_arg = d.bind.as_ref().map(|b| b.arg);
        Ok(self.push(PatNode::App {
            decl,
            gens: gens.into(),
            meta: meta.into(),
            args: args.into(),
            bind_arg,
        }))
    }

    pub fn find_term(&mut self, p: PatId) {
        self.find = Some(Find::Term(p));
    }

    pub fn find_sequent(&mut self, ante: Option<PatId>, succ: Option<PatId>) {
        self.find = Some(Find::Sequent { ante, succ });
    }

    pub fn assume(&mut self, side: Side, pat: PatId) {
        self.assumes.push(Assumes { side, pat });
    }

    pub fn cond(&mut self, c: Cond) {
        self.conds.push(c);
    }

    pub fn finish(self) -> Result<Rule, PatternError> {
        let find = self.find.ok_or(PatternError::NoFind)?;
        if let Find::Sequent {
            ante: None,
            succ: None,
        } = find
        {
            return Err(PatternError::EmptyFind);
        }
        let mut seen = vec![false; self.svs.len()];
        for n in &self.nodes {
            if let PatNode::Sv(s) = n {
                seen[s.0 as usize] = true;
            }
        }
        for c in &self.conds {
            let Cond::NoFreeVar { sv, .. } = c;
            match seen.get(sv.0 as usize) {
                Some(true) => {}
                _ => return Err(PatternError::UnboundInCond(*sv)),
            }
        }
        Ok(Rule {
            name: self.name,
            svs: self.svs,
            slots: self.slots,
            nodes: self.nodes,
            find,
            assumes: self.assumes,
            conds: self.conds,
        })
    }

    fn push(&mut self, n: PatNode) -> PatId {
        self.nodes.push(n);
        PatId(self.nodes.len() as u32 - 1)
    }

    fn note_sort(&mut self, s: SortReq) {
        if let SortReq::Slot(i) = s {
            self.slots = self.slots.max(i + 1);
        }
    }
}
