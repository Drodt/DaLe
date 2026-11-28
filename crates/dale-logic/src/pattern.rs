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
        /// The arguments that are under the operator's binder, from the declaration (see
        /// [`binds_arg`](crate::binds_arg)).
        bind_mask: u64,
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
    /// No `\find` (`cut`, `int_induction`, lemmas with `\assumes`): the rule is applied to a goal
    /// as a whole, matching only its `\assumes` patterns, and may only add formulas
    /// ([`Replace::Nothing`]).
    None,
}

#[derive(Clone, Copy, Debug)]
pub struct Assumes {
    pub side: Side,
    pub pat: PatId,
}

/// A node of a rule's template arena: what the rule builds from a match.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TmplId(pub u32);

/// A generic argument of an operator in a template.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenTmpl {
    Exact(GenArg),
    /// The generic argument bound to this slot by the patterns.
    Slot(u32),
    /// Inferred from the arguments (as [`TermPool::mk_inferred`](crate::TermPool::mk_inferred)).
    /// Either all or none of the generic arguments of an operator are inferred.
    Infer,
}

#[derive(Clone, Debug)]
pub enum TmplNode {
    /// What the schema variable matched. A schema variable that ends up under a different number
    /// of binders than where it matched is shifted accordingly (see [`crate::apply_rule`]).
    Sv(SvId),
    Ground(TermIdx),
    App {
        decl: OpDeclId,
        gens: Box<[GenTmpl]>,
        meta: Box<[MetaValue]>,
        args: Box<[TmplId]>,
        bind_mask: u64,
    },
}

/// What a goal template does with the found formula or term.
#[derive(Clone, Debug)]
pub enum Replace {
    /// Keep it (only `\add`).
    Nothing,
    /// For a [`Find::Term`]: the new term at the position of the found one.
    Term(TmplId),
    /// For a [`Find::Sequent`]: the formulas that replace the found formula of the antecedent /
    /// succedent. A side that has no find part must have no formulas.
    Sequent {
        ante: Vec<TmplId>,
        succ: Vec<TmplId>,
    },
}

/// One goal template of a rule: the rule application yields one new goal per template.
#[derive(Clone, Debug)]
pub struct GoalTemplate {
    pub label: Option<Symbol>,
    pub replace: Replace,
    /// Formulas added to the antecedent / succedent (`\add`); they must be closed.
    pub add_ante: Vec<TmplId>,
    pub add_succ: Vec<TmplId>,
}

impl GoalTemplate {
    pub fn new(replace: Replace) -> Self {
        Self {
            label: None,
            replace,
            add_ante: Vec::new(),
            add_succ: Vec::new(),
        }
    }
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
    pub(crate) tmpls: Vec<TmplNode>,
    pub(crate) goals: Vec<GoalTemplate>,
    /// The schema variables no pattern binds, in declaration order.
    pub(crate) free: Vec<SvId>,
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
    /// The goal templates; none means that applying the rule closes the goal.
    pub fn goals(&self) -> &[GoalTemplate] {
        &self.goals
    }
    pub fn tmpl(&self, id: TmplId) -> &TmplNode {
        &self.tmpls[id.0 as usize]
    }
    pub fn node(&self, id: PatId) -> &PatNode {
        &self.nodes[id.0 as usize]
    }
    /// The schema variables that no pattern binds, such as `cutFormula` of `cut`: matching leaves
    /// them unbound, and whoever applies the rule (user or strategy) supplies them with
    /// [`Bindings::set`](crate::Bindings::set) first. The instantiation of the
    /// [`Application`](crate::Application) records them like the others.
    pub fn free_svs(&self) -> &[SvId] {
        &self.free
    }
    /// Whether no pattern binds `sv` (see [`free_svs`](Self::free_svs)).
    pub fn is_free(&self, sv: SvId) -> bool {
        self.free.contains(&sv)
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
    /// A condition mentions a schema variable that occurs in no pattern (conditions on free
    /// schema variables are not supported yet).
    UnboundInCond(SvId),
    /// `Find` is a sequent find without any formula.
    EmptyFind,
    /// No find was set; rules without `\find` call [`RuleBuilder::no_find`].
    NoFind,
    UnknownTemplate(TmplId),
    /// `Replace::Term` for a sequent find, `Replace::Sequent` for a term find, formulas for a
    /// side without find part, or anything but `Replace::Nothing` for a rule without find.
    BadReplace,
    /// Some but not all generic arguments of a template operator are inferred.
    MixedInfer,
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
    tmpls: Vec<TmplNode>,
    goals: Vec<GoalTemplate>,
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
            tmpls: Vec::new(),
            goals: Vec::new(),
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
        let bind_mask = d.bind.as_ref().map_or(0, |b| b.mask());
        Ok(self.push(PatNode::App {
            decl,
            gens: gens.into(),
            meta: meta.into(),
            args: args.into(),
            bind_mask,
        }))
    }

    pub fn find_term(&mut self, p: PatId) {
        self.find = Some(Find::Term(p));
    }

    pub fn find_sequent(&mut self, ante: Option<PatId>, succ: Option<PatId>) {
        self.find = Some(Find::Sequent { ante, succ });
    }

    /// The rule has no `\find` ([`Find::None`]).
    pub fn no_find(&mut self) {
        self.find = Some(Find::None);
    }

    pub fn assume(&mut self, side: Side, pat: PatId) {
        self.assumes.push(Assumes { side, pat });
    }

    pub fn cond(&mut self, c: Cond) {
        self.conds.push(c);
    }

    /// A template node for what a schema variable matched.
    pub fn tmpl_sv(&mut self, sv: SvId) -> Result<TmplId, PatternError> {
        if sv.0 as usize >= self.svs.len() {
            return Err(PatternError::UnknownSv(sv));
        }
        Ok(self.push_tmpl(TmplNode::Sv(sv)))
    }

    pub fn tmpl_ground(&mut self, t: TermIdx) -> TmplId {
        self.push_tmpl(TmplNode::Ground(t))
    }

    /// An operator application in a template.
    pub fn tmpl_app(
        &mut self,
        decl: OpDeclId,
        gens: &[GenTmpl],
        meta: &[MetaValue],
        args: &[TmplId],
    ) -> Result<TmplId, PatternError> {
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
        let infer = gens.iter().filter(|g| matches!(g, GenTmpl::Infer)).count();
        if infer != 0 && infer != gens.len() {
            return Err(PatternError::MixedInfer);
        }
        for a in args {
            if a.0 as usize >= self.tmpls.len() {
                return Err(PatternError::UnknownTemplate(*a));
            }
        }
        for g in gens {
            if let GenTmpl::Slot(i) = g {
                self.slots = self.slots.max(i + 1);
            }
        }
        let bind_mask = d.bind.as_ref().map_or(0, |b| b.mask());
        Ok(self.push_tmpl(TmplNode::App {
            decl,
            gens: gens.into(),
            meta: meta.into(),
            args: args.into(),
            bind_mask,
        }))
    }

    /// Adds a goal template. A rule without any closes the goal it is applied to.
    pub fn goal(&mut self, g: GoalTemplate) {
        self.goals.push(g);
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
        // Schema variables that no pattern binds are free: supplied from outside when applying.
        let free = (0..self.svs.len() as u32)
            .map(SvId)
            .filter(|s| !seen[s.0 as usize])
            .collect();
        // templates: consistent replacement
        let n_tmpl = self.tmpls.len() as u32;
        let (has_ante, has_succ) = match find {
            Find::Term(_) | Find::None => (false, false),
            Find::Sequent { ante, succ } => (ante.is_some(), succ.is_some()),
        };
        for g in &self.goals {
            let all = g.add_ante.iter().chain(&g.add_succ);
            let all: Vec<_> = match &g.replace {
                Replace::Nothing => all.copied().collect(),
                Replace::Term(t) => {
                    if !matches!(find, Find::Term(_)) {
                        return Err(PatternError::BadReplace);
                    }
                    all.copied().chain([*t]).collect()
                }
                Replace::Sequent { ante, succ } => {
                    if matches!(find, Find::Term(_) | Find::None)
                        || (!has_ante && !ante.is_empty())
                        || (!has_succ && !succ.is_empty())
                    {
                        return Err(PatternError::BadReplace);
                    }
                    all.chain(ante).chain(succ).copied().collect()
                }
            };
            if let Some(t) = all.iter().find(|t| t.0 >= n_tmpl) {
                return Err(PatternError::UnknownTemplate(*t));
            }
        }
        Ok(Rule {
            tmpls: self.tmpls,
            goals: self.goals,
            name: self.name,
            svs: self.svs,
            slots: self.slots,
            nodes: self.nodes,
            find,
            assumes: self.assumes,
            conds: self.conds,
            free,
        })
    }

    fn push_tmpl(&mut self, n: TmplNode) -> TmplId {
        self.tmpls.push(n);
        TmplId(self.tmpls.len() as u32 - 1)
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
