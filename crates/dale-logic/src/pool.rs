//! The [`TermPool`]: one container for the instantiated sorts, instantiated operators and the
//! hash-consed terms of a logic.
//!
//! * Everything is append-only; an index handed out stays valid (until a [`rollback`] past it,
//!   which is only possible back to the last fork).
//! * Equal sorts / operators / terms have equal indices, so comparison is `==` on a `u32`.
//! * A term is a small header; its arguments live in one shared vector.
//!
//! Concurrency: a pool is single-threaded, but it can be *forked* ([`TermPool::fork`]): the
//! sorts, operators and terms created so far become a shared read-only part, and the two pools
//! then grow independently (ids created after the fork may coincide in the two pools; nothing may
//! mix them). Parallel proof search gives every worker its own pool and forks when a goal is
//! handed to another worker, so the hot path needs no locks. See [`arena`](crate::arena).
//!
//! [`rollback`]: TermPool::rollback

use std::collections::HashSet;
use std::ops::Index;
use std::sync::Arc;

use crate::arena::Arena;
use crate::error::LogicError;
use crate::ids::{OpDeclId, OpIdx, SortDeclId, SortIdx, TermIdx};
use crate::sig::{
    GenArgPat, GenKind, GenParam, MetaValue, OpFlags, OpKind, Signature, SortPat, binds_arg,
};
use crate::table::{Interner, mix};

/// An argument of a generic declaration.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum GenArg {
    Sort(SortIdx),
    /// A closed term (for now of any sort accepted by the parameter).
    // TODO: const generics are terms of sort `int` for now; revisit (literals only? other sorts?).
    Const(TermIdx),
}

/// An instantiated sort, e.g. `Seq<int>`.
#[derive(Clone, Debug)]
pub struct SortData {
    decl: SortDeclId,
    args: Box<[GenArg]>,
    /// All proper ancestors except the top sort, sorted by index.
    ancestors: Box<[SortIdx]>,
}

impl SortData {
    /// The (possibly generic) declaration this sort instantiates.
    #[inline]
    pub fn decl(&self) -> SortDeclId {
        self.decl
    }
    /// The generic arguments of the declaration (empty for non-generic sorts).
    #[inline]
    pub fn args(&self) -> &[GenArg] {
        &self.args
    }
    /// All proper supersorts except the top sort, sorted by index.
    #[inline]
    pub fn ancestors(&self) -> &[SortIdx] {
        &self.ancestors
    }
}

/// An instantiated operator, e.g. `ifThenElse<int>` or `var<int>(0)`.
#[derive(Clone, Debug)]
pub struct OpData {
    decl: OpDeclId,
    generics: Box<[GenArg]>,
    meta: Box<[MetaValue]>,
    arg_sorts: Box<[SortIdx]>,
    result: SortIdx,
    flags: OpFlags,
    bind: Option<(u64, SortIdx)>,
    var_index: Option<u64>,
}

impl OpData {
    /// The (possibly generic) declaration this operator instantiates.
    #[inline]
    pub fn decl(&self) -> OpDeclId {
        self.decl
    }
    #[inline]
    pub fn generics(&self) -> &[GenArg] {
        &self.generics
    }
    #[inline]
    pub fn meta(&self) -> &[MetaValue] {
        &self.meta
    }
    /// The sorts the subterms must be subsorts of.
    #[inline]
    pub fn arg_sorts(&self) -> &[SortIdx] {
        &self.arg_sorts
    }
    #[inline]
    pub fn arity(&self) -> usize {
        self.arg_sorts.len()
    }
    /// The sort of every term with this operator.
    #[inline]
    pub fn result(&self) -> SortIdx {
        self.result
    }
    #[inline]
    pub fn flags(&self) -> OpFlags {
        self.flags
    }
    /// `(mask of the bound argument positions, sort of the bound variable)` for binders; see
    /// [`binds_arg`](crate::binds_arg).
    #[inline]
    pub fn bind(&self) -> Option<(u64, SortIdx)> {
        self.bind
    }

    /// Whether the subterm at `i` is under the operator's binder.
    #[inline]
    pub fn binds(&self, i: usize) -> bool {
        matches!(self.bind, Some((m, _)) if binds_arg(m, i))
    }
    /// The de Bruijn index, for bound variable operators.
    #[inline]
    pub fn var_index(&self) -> Option<u64> {
        self.var_index
    }
}

/// The header of a term.
#[derive(Clone, Copy, Debug)]
pub struct Term {
    op: OpIdx,
    sort: SortIdx,
    args_start: u32,
    args_len: u32,
    /// The number of enclosing binders the term needs to be closed (0: closed).
    loose: u32,
    rigid: bool,
    /// Bit `k` (`k < 63`): the variable with index `k` may occur free in the term. Bit 63 stands for
    /// all indices `>= 63`. Exact for `k < 62`; above that, bits may be set conservatively (see
    /// [`Term::may_have_free`]). Gives "does the term mention the innermost bound variable" with
    /// one AND.
    free_mask: u64,
}

impl Term {
    #[inline]
    pub fn op(&self) -> OpIdx {
        self.op
    }
    #[inline]
    pub fn sort(&self) -> SortIdx {
        self.sort
    }
    #[inline]
    pub fn arity(&self) -> usize {
        self.args_len as usize
    }
    /// The number of enclosing binders needed to bind all variables; 0 means closed.
    #[inline]
    pub fn loose(&self) -> u32 {
        self.loose
    }
    #[inline]
    pub fn is_closed(&self) -> bool {
        self.loose == 0
    }
    /// The raw free-variable mask; see [`Term::may_have_free`].
    #[inline]
    pub fn free_mask(&self) -> u64 {
        self.free_mask
    }
    /// Whether the variable with de Bruijn index `k` (relative to the term's own context) may occur
    /// free. `false` is definite. `true` is exact for `k < 62`; for larger `k` it may be a false
    /// positive.
    #[inline]
    pub fn may_have_free(&self, k: u32) -> bool {
        self.free_mask >> k.min(63) & 1 != 0
    }
    /// Whether any of the variables `0..n` may occur free.
    #[inline]
    pub fn may_have_free_below(&self, n: u32) -> bool {
        let m = if n >= 63 {
            self.free_mask
        } else {
            self.free_mask & ((1u64 << n) - 1)
        };
        m != 0
    }
    /// Operator and all subterms rigid (independent of the program state).
    #[inline]
    pub fn is_rigid(&self) -> bool {
        self.rigid
    }
}

/// A snapshot of how much a [`TermPool`] holds, taken with [`TermPool::checkpoint`] and used with
/// [`TermPool::rollback`] to throw away everything created in between.
///
/// Because the pool is append-only and everything refers only to *earlier* entries, forgetting
/// the most recent entries never leaves a dangling reference among the survivors. This is meant
/// for speculative work, e.g. building the terms of a rule application that is then rejected, or
/// the instances of a failed proof branch, without keeping the garbage around.
///
/// Indices created after the checkpoint (sorts, operators and terms) are invalid after the
/// rollback and must not be used any more; using one is either a panic or, if the slot has been
/// reused by a new entry, a silently different term. A checkpoint can only be used on the pool
/// that created it, only while the pool has not been rolled back past it, and only until the pool
/// is forked or frozen (see [`TermPool::fork`]).
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint {
    sorts: u32,
    ops: u32,
    terms: u32,
    args: u32,
}

// Hashing of intern keys.
//
// The tables only store `u32` indices, so the hash of an entry is recomputed from its key (the
// functions below take the key's parts, which is exactly what the entry's data holds). All of
// them fold the parts of a key into one `u64` with [`mix`], starting from a different seed per
// kind of key so that e.g. a sort and an operator with the same numbers do not collide. The
// table scrambles the high bits into the low ones itself, so these only have to be cheap and
// order-sensitive. They are not meant to be collision resistant against adversarial input.

/// Folds one generic argument into `h`. The tag (1 = sort, 2 = constant) keeps `Sort(5)` and
/// `Const(5)` apart.
fn hash_gen(h: u64, a: &GenArg) -> u64 {
    match a {
        GenArg::Sort(s) => mix(mix(h, 1), s.as_u32() as u64),
        GenArg::Const(t) => mix(mix(h, 2), t.as_u32() as u64),
    }
}

/// Key of an instantiated sort: the declaration and its generic arguments.
fn hash_sort(decl: SortDeclId, args: &[GenArg]) -> u64 {
    args.iter()
        .fold(mix(0x5017, decl.as_u32() as u64), hash_gen)
}

/// Key of an instantiated operator: the declaration, generic arguments and meta values. The meta
/// values are part of the key because `var<int>(0)` and `var<int>(1)` are different operators.
fn hash_op(decl: OpDeclId, generics: &[GenArg], meta: &[MetaValue]) -> u64 {
    let h = generics
        .iter()
        .fold(mix(0x0b, decl.as_u32() as u64), hash_gen);
    meta.iter()
        .fold(h, |h, MetaValue::Nat(n)| mix(mix(h, 3), *n))
}

/// Key of a term: its operator and the indices of its subterms, in order. Since subterms are
/// themselves hash-consed, comparing indices is comparing the whole subterms, so this is `O(arity)`
/// and does not depend on the size of the term.
#[inline]
fn hash_term(op: OpIdx, args: &[TermIdx]) -> u64 {
    args.iter().fold(mix(0x7e, op.as_u32() as u64), |h, a| {
        mix(h, a.as_u32() as u64)
    })
}

pub struct TermPool {
    pub(crate) sig: Arc<Signature>,
    sorts: Arena<SortData>,
    sort_table: Interner,
    ops: Arena<OpData>,
    op_table: Interner,
    terms: Arena<Term>,
    args: Arena<TermIdx>,
    term_table: Interner,
    top: SortIdx,
    formula: SortIdx,
    pub(crate) var_decl: Option<OpDeclId>,
}

impl Index<TermIdx> for TermPool {
    type Output = Term;
    #[inline]
    fn index(&self, i: TermIdx) -> &Term {
        self.terms.get(i.as_u32())
    }
}

impl Index<SortIdx> for TermPool {
    type Output = SortData;
    #[inline]
    fn index(&self, i: SortIdx) -> &SortData {
        self.sorts.get(i.as_u32())
    }
}

impl Index<OpIdx> for TermPool {
    type Output = OpData;
    #[inline]
    fn index(&self, i: OpIdx) -> &OpData {
        self.ops.get(i.as_u32())
    }
}

impl TermPool {
    pub fn new(sig: Arc<Signature>) -> Self {
        let mut pool = TermPool {
            var_decl: sig.bound_var(),
            sig,
            sorts: Arena::default(),
            sort_table: Interner::new(),
            ops: Arena::default(),
            op_table: Interner::new(),
            terms: Arena::default(),
            args: Arena::default(),
            term_table: Interner::new(),
            top: SortIdx::MAX,
            formula: SortIdx::MAX,
        };
        let (top, formula) = (pool.sig.top(), pool.sig.formula());
        // The signature builder guarantees that both are non-generic and extend nothing.
        pool.top = pool.mk_sort(top, &[]).expect("top sort");
        pool.formula = pool.mk_sort(formula, &[]).expect("formula sort");
        pool
    }

    pub fn signature(&self) -> &Arc<Signature> {
        &self.sig
    }

    #[inline]
    pub fn top(&self) -> SortIdx {
        self.top
    }

    #[inline]
    pub fn formula(&self) -> SortIdx {
        self.formula
    }

    // ---------------------------------------------------------------- access

    /// The subterms of `t`.
    #[inline]
    pub fn subs_of(&self, t: TermIdx) -> &[TermIdx] {
        let t = self.terms.get(t.as_u32());
        self.args.slice(t.args_start, t.args_len as usize)
    }

    #[inline]
    pub fn sort_of(&self, t: TermIdx) -> SortIdx {
        self.terms.get(t.as_u32()).sort
    }

    #[inline]
    pub fn op_of(&self, t: TermIdx) -> OpIdx {
        self.terms.get(t.as_u32()).op
    }

    /// Whether `t` is a formula, i.e. its sort is the formula sort or a subsort of it.
    #[inline]
    pub fn is_formula(&self, t: TermIdx) -> bool {
        self.is_subsort(self.sort_of(t), self.formula)
    }

    pub fn num_terms(&self) -> usize {
        self.terms.len()
    }

    pub fn num_sorts(&self) -> usize {
        self.sorts.len()
    }

    pub fn num_ops(&self) -> usize {
        self.ops.len()
    }

    /// How many frozen layers a lookup of a new term has to probe (a statistic; stays small
    /// because forks merge layers of similar size).
    pub fn term_table_layers(&self) -> usize {
        self.term_table.layers()
    }

    /// `a <= b` in the sort hierarchy (reflexive).
    #[inline]
    pub fn is_subsort(&self, a: SortIdx, b: SortIdx) -> bool {
        a == b
            || b == self.top
            || self
                .sorts
                .get(a.as_u32())
                .ancestors
                .binary_search(&b)
                .is_ok()
    }

    // ---------------------------------------------------------------- sorts

    /// The sort `decl<args>`. Sorts are invariant in their arguments.
    pub fn mk_sort(&mut self, decl: SortDeclId, args: &[GenArg]) -> Result<SortIdx, LogicError> {
        let h = hash_sort(decl, args);
        let found = self.sort_table.find(h, |i| {
            let d = self.sorts.get(i);
            d.decl == decl && *d.args == *args
        });
        if let Some(i) = found {
            return Ok(SortIdx::from_u32(i));
        }
        let sig = self.sig.clone();
        let d = sig.sort_decl(decl);
        self.check_generics(&d.params, args)?;
        let mut anc = Vec::new();
        for e in &d.extends {
            let s = self.inst_sort(e, args)?;
            anc.push(s);
            anc.extend_from_slice(&self.sorts.get(s.as_u32()).ancestors);
        }
        anc.retain(|&s| s != self.top);
        anc.sort_unstable();
        anc.dedup();
        // Instantiating the supersorts cannot have created this sort: they are declared before it.
        let idx = self.sorts.push(SortData {
            decl,
            args: args.into(),
            ancestors: anc.into(),
        });
        let sorts = &self.sorts;
        self.sort_table.insert(h, idx, |i| {
            let d = sorts.get(i);
            hash_sort(d.decl, &d.args)
        });
        Ok(SortIdx::from_u32(idx))
    }

    /// Instantiates a pattern of a declaration with the given generic arguments.
    pub(crate) fn inst_sort(
        &mut self,
        pat: &SortPat,
        generics: &[GenArg],
    ) -> Result<SortIdx, LogicError> {
        match pat {
            SortPat::Param(i) => match generics.get(*i as usize) {
                Some(GenArg::Sort(s)) => Ok(*s),
                _ => Err(LogicError::GenericKind(*i as usize)),
            },
            SortPat::Inst { decl, args } => {
                let mut v = Vec::with_capacity(args.len());
                for a in args {
                    v.push(match a {
                        GenArgPat::Sort(p) => GenArg::Sort(self.inst_sort(p, generics)?),
                        GenArgPat::Const(i) => match generics.get(*i as usize) {
                            Some(c @ GenArg::Const(_)) => *c,
                            _ => return Err(LogicError::GenericKind(*i as usize)),
                        },
                    });
                }
                self.mk_sort(*decl, &v)
            }
        }
    }

    fn check_generics(&mut self, params: &[GenParam], args: &[GenArg]) -> Result<(), LogicError> {
        if params.len() != args.len() {
            return Err(LogicError::GenericArity {
                expected: params.len(),
                found: args.len(),
            });
        }
        for (i, (p, a)) in params.iter().zip(args).enumerate() {
            match (&p.kind, a) {
                (GenKind::Sort, GenArg::Sort(_)) => {}
                (GenKind::Const(pat), GenArg::Const(t)) => {
                    let term = self.terms.get(t.as_u32());
                    if term.loose != 0 {
                        return Err(LogicError::ConstNotClosed(i));
                    }
                    let found = term.sort;
                    let expected = self.inst_sort(pat, &args[..i])?;
                    if !self.is_subsort(found, expected) {
                        return Err(LogicError::ConstSort {
                            index: i,
                            expected,
                            found,
                        });
                    }
                }
                _ => return Err(LogicError::GenericKind(i)),
            }
        }
        Ok(())
    }

    // ------------------------------------------------------------ operators

    /// The operator `decl<generics>(meta)`.
    pub fn mk_op(
        &mut self,
        decl: OpDeclId,
        generics: &[GenArg],
        meta: &[MetaValue],
    ) -> Result<OpIdx, LogicError> {
        let h = hash_op(decl, generics, meta);
        let eq = |ops: &Arena<OpData>, i: u32| {
            let d = ops.get(i);
            d.decl == decl && *d.generics == *generics && *d.meta == *meta
        };
        if let Some(i) = self.op_table.find(h, |i| eq(&self.ops, i)) {
            return Ok(OpIdx::from_u32(i));
        }
        let sig = self.sig.clone();
        let d = sig.op_decl(decl);
        self.check_generics(&d.generics, generics)?;
        if d.meta.len() != meta.len() {
            return Err(LogicError::MetaArity {
                expected: d.meta.len(),
                found: meta.len(),
            });
        }
        for (i, (k, m)) in d.meta.iter().zip(meta).enumerate() {
            if *k != m.kind() {
                return Err(LogicError::MetaKind(i));
            }
        }
        let mut arg_sorts = Vec::with_capacity(d.args.len());
        for a in &d.args {
            arg_sorts.push(self.inst_sort(a, generics)?);
        }
        let result = self.inst_sort(&d.result, generics)?;
        let bind = match &d.bind {
            Some(b) => Some((b.mask(), self.inst_sort(&b.var_sort, generics)?)),
            None => None,
        };
        let var_index = match (d.kind, meta) {
            (OpKind::BoundVar, [MetaValue::Nat(n)]) => Some(*n),
            _ => None,
        };
        let idx = self.ops.push(OpData {
            decl,
            generics: generics.into(),
            meta: meta.into(),
            arg_sorts: arg_sorts.into(),
            result,
            flags: d.flags,
            bind,
            var_index,
        });
        let ops = &self.ops;
        self.op_table.insert(h, idx, |i| {
            let d = ops.get(i);
            hash_op(d.decl, &d.generics, &d.meta)
        });
        Ok(OpIdx::from_u32(idx))
    }

    // ---------------------------------------------------------------- terms

    /// Creates (or finds) the term `op(args)`, checking the number and sorts of the arguments
    /// and, for binders, the sorts of the variables bound in the bound argument.
    pub fn mk(&mut self, op: OpIdx, args: &[TermIdx]) -> Result<TermIdx, LogicError> {
        let od = self.ops.get(op.as_u32());
        if od.arg_sorts.len() != args.len() {
            return Err(LogicError::ArgCount {
                expected: od.arg_sorts.len(),
                found: args.len(),
            });
        }
        for (index, (a, &expected)) in args.iter().zip(&od.arg_sorts).enumerate() {
            let found = self.terms.get(a.as_u32()).sort;
            if !self.is_subsort(found, expected) {
                return Err(LogicError::ArgSort {
                    index,
                    expected,
                    found,
                });
            }
        }
        if matches!(od.var_index, Some(n) if n >= u32::MAX as u64) {
            return Err(LogicError::VarIndexTooLarge);
        }
        let bind = od.bind;
        let h = hash_term(op, args);
        if let Some(t) = self.find_term(h, op, args) {
            return Ok(t);
        }
        // Only new terms need the binder check: an existing one has passed it.
        if let Some((mask, var_sort)) = bind {
            let mut seen = HashSet::new();
            for (i, &a) in args.iter().enumerate() {
                if binds_arg(mask, i) {
                    self.check_bound(a, 0, var_sort, &mut seen)?;
                }
            }
        }
        Ok(self.insert_term(h, op, args))
    }

    /// Like [`mk`](Self::mk) but without any check. The caller guarantees the argument count,
    /// argument sorts and binder sorts. Violations are not memory unsafe but make the term
    /// ill-formed.
    pub fn mk_unchecked(&mut self, op: OpIdx, args: &[TermIdx]) -> TermIdx {
        debug_assert_eq!(self.ops.get(op.as_u32()).arg_sorts.len(), args.len());
        let h = hash_term(op, args);
        match self.find_term(h, op, args) {
            Some(t) => t,
            None => self.insert_term(h, op, args),
        }
    }

    #[inline]
    fn find_term(&self, h: u64, op: OpIdx, args: &[TermIdx]) -> Option<TermIdx> {
        self.term_table
            .find(h, |i| {
                let t = self.terms.get(i);
                t.op == op && self.args_of(t) == args
            })
            .map(TermIdx::from_u32)
    }

    #[inline]
    fn args_of(&self, t: &Term) -> &[TermIdx] {
        self.args.slice(t.args_start, t.args_len as usize)
    }

    fn insert_term(&mut self, h: u64, op: OpIdx, args: &[TermIdx]) -> TermIdx {
        let od = self.ops.get(op.as_u32());
        let mut loose = match od.var_index {
            Some(n) => u32::try_from(n + 1).expect("de Bruijn index too large"),
            None => 0,
        };
        let mut rigid = od.flags.rigid;
        let mut free_mask = match od.var_index {
            Some(n) => 1u64 << n.min(63),
            None => 0,
        };
        for (i, a) in args.iter().enumerate() {
            let t = self.terms.get(a.as_u32());
            let (l, m) = match od.bind {
                // the variable bound here disappears, the others move one index down; the "63 and
                // above" bit stays set
                Some((m, _)) if binds_arg(m, i) => (
                    t.loose.saturating_sub(1),
                    (t.free_mask >> 1) | (t.free_mask & (1 << 63)),
                ),
                _ => (t.loose, t.free_mask),
            };
            loose = loose.max(l);
            free_mask |= m;
            rigid &= t.rigid;
        }
        let sort = od.result;
        let args_start = self.args.push_slice(args);
        let idx = self.terms.push(Term {
            op,
            sort,
            args_start,
            args_len: args.len() as u32,
            loose,
            rigid,
            free_mask,
        });
        let (terms, all_args) = (&self.terms, &self.args);
        self.term_table.insert(h, idx, |i| {
            let t = terms.get(i);
            hash_term(t.op, all_args.slice(t.args_start, t.args_len as usize))
        });
        TermIdx::from_u32(idx)
    }

    /// Checks that every variable of `t` that is bound by the binder being built (index `depth`
    /// at this position) has sort `var_sort`. Only paths containing such variables are visited.
    fn check_bound(
        &self,
        t: TermIdx,
        depth: u32,
        var_sort: SortIdx,
        seen: &mut HashSet<(TermIdx, u32)>,
    ) -> Result<(), LogicError> {
        let term = self.terms.get(t.as_u32());
        if term.loose <= depth || !seen.insert((t, depth)) {
            return Ok(());
        }
        let od = self.ops.get(term.op.as_u32());
        if let Some(n) = od.var_index {
            // `loose > depth` means n >= depth.
            // TODO: relax the equality to subsorts if bound variables may be instantiated with
            // subsorts anyway (`instantiate` already accepts those).
            if n == depth as u64 && term.sort != var_sort {
                return Err(LogicError::BindSort {
                    expected: var_sort,
                    found: term.sort,
                });
            }
            return Ok(());
        }
        for (i, a) in self.args_of(term).iter().enumerate() {
            let d = depth + od.binds(i) as u32;
            self.check_bound(*a, d, var_sort, seen)?;
        }
        Ok(())
    }

    // ------------------------------------------------------------ inference

    /// Creates `decl(args)`, inferring the generic arguments from the sorts of `args`. Sort
    /// parameters get the most general sort of the arguments using them (e.g. `int` for
    /// `ifThenElse(p, nat, int)`); const parameters are inferred from sort arguments like
    /// `Array<E, N>`. Parameters that occur in no argument sort need [`mk_op`](Self::mk_op).
    pub fn mk_inferred(
        &mut self,
        decl: OpDeclId,
        meta: &[MetaValue],
        args: &[TermIdx],
    ) -> Result<TermIdx, LogicError> {
        let sig = self.sig.clone();
        let d = sig.op_decl(decl);
        if d.args.len() != args.len() {
            return Err(LogicError::ArgCount {
                expected: d.args.len(),
                found: args.len(),
            });
        }
        let mut b: Vec<Option<GenArg>> = vec![None; d.generics.len()];
        for (i, (p, a)) in d.args.iter().zip(args).enumerate() {
            if !self.unify(p, self.sort_of(*a), false, &mut b) {
                return Err(LogicError::InferMismatch(i));
            }
        }
        let mut generics = Vec::with_capacity(b.len());
        for (i, g) in b.into_iter().enumerate() {
            generics.push(g.ok_or(LogicError::CannotInfer(i))?);
        }
        let op = self.mk_op(decl, &generics, meta)?;
        self.mk(op, args)
    }

    fn unify(&self, p: &SortPat, s: SortIdx, exact: bool, b: &mut [Option<GenArg>]) -> bool {
        match p {
            SortPat::Param(i) => match b[*i as usize] {
                None => {
                    b[*i as usize] = Some(GenArg::Sort(s));
                    true
                }
                Some(GenArg::Sort(cur)) => {
                    if cur == s {
                        true
                    } else if exact {
                        false
                    } else if self.is_subsort(s, cur) {
                        true
                    } else if self.is_subsort(cur, s) {
                        b[*i as usize] = Some(GenArg::Sort(s));
                        true
                    } else {
                        false
                    }
                }
                Some(GenArg::Const(_)) => false,
            },
            SortPat::Inst { decl, args } => {
                let sd = self.sorts.get(s.as_u32());
                let target = if sd.decl == *decl {
                    Some(s)
                } else if exact {
                    None
                } else {
                    sd.ancestors
                        .iter()
                        .copied()
                        .find(|a| self.sorts.get(a.as_u32()).decl == *decl)
                };
                let Some(t) = target else { return false };
                let sargs = &self.sorts.get(t.as_u32()).args;
                if sargs.len() != args.len() {
                    return false;
                }
                for (pa, sa) in args.iter().zip(sargs.iter()) {
                    match (pa, sa) {
                        (GenArgPat::Sort(p2), GenArg::Sort(s2)) => {
                            if !self.unify(p2, *s2, true, b) {
                                return false;
                            }
                        }
                        (GenArgPat::Const(i), GenArg::Const(c)) => match b[*i as usize] {
                            None => b[*i as usize] = Some(GenArg::Const(*c)),
                            Some(GenArg::Const(cur)) if cur == *c => {}
                            _ => return false,
                        },
                        _ => return false,
                    }
                }
                true
            }
        }
    }

    // ------------------------------------------------------------- rollback

    /// Remembers the current extent of the pool.
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            sorts: self.sorts.next(),
            ops: self.ops.next(),
            terms: self.terms.next(),
            args: self.args.next(),
        }
    }

    /// Drops everything created after `cp`. All indices created since are invalid afterwards.
    ///
    /// Fails with [`LogicError::RollbackPastFork`], changing nothing, if the pool has been forked
    /// (or [frozen](Self::freeze)) since the checkpoint: what was created before the fork is
    /// shared with other pools and cannot be dropped.
    pub fn rollback(&mut self, cp: Checkpoint) -> Result<(), LogicError> {
        if cp.sorts < self.sorts.base()
            || cp.ops < self.ops.base()
            || cp.terms < self.terms.base()
            || cp.args < self.args.base()
        {
            return Err(LogicError::RollbackPastFork);
        }
        assert!(
            cp.sorts <= self.sorts.next()
                && cp.ops <= self.ops.next()
                && cp.terms <= self.terms.next(),
            "checkpoint from the future"
        );
        self.sorts.truncate(cp.sorts);
        self.ops.truncate(cp.ops);
        self.terms.truncate(cp.terms);
        self.args.truncate(cp.args);
        self.sort_table.truncate(cp.sorts);
        self.op_table.truncate(cp.ops);
        self.term_table.truncate(cp.terms);
        Ok(())
    }

    // ------------------------------------------------------------------ fork

    /// Moves everything created so far into the read-only part that forks share.
    fn seal(&mut self) {
        self.sorts.seal();
        self.ops.seal();
        self.terms.seal();
        self.args.seal();
        let (sorts, ops, terms, args) = (&self.sorts, &self.ops, &self.terms, &self.args);
        self.sort_table.seal(|i| {
            let d = sorts.get(i);
            hash_sort(d.decl, &d.args)
        });
        self.op_table.seal(|i| {
            let d = ops.get(i);
            hash_op(d.decl, &d.generics, &d.meta)
        });
        self.term_table.seal(|i| {
            let t = terms.get(i);
            hash_term(t.op, args.slice(t.args_start, t.args_len as usize))
        });
    }

    /// The same read-only part, nothing of our own. Only valid right after [`seal`](Self::seal).
    fn shallow_clone(&self) -> TermPool {
        TermPool {
            sig: self.sig.clone(),
            sorts: self.sorts.shallow_clone(),
            sort_table: self.sort_table.shallow_clone(),
            ops: self.ops.shallow_clone(),
            op_table: self.op_table.shallow_clone(),
            terms: self.terms.shallow_clone(),
            args: self.args.shallow_clone(),
            term_table: self.term_table.shallow_clone(),
            top: self.top,
            formula: self.formula,
            var_decl: self.var_decl,
        }
    }

    /// Splits the pool in two: everything created so far becomes a shared read-only part, and the
    /// returned pool and `self` continue from it independently. Cost: proportional to what was
    /// created since the last fork, not to the size of the pool.
    ///
    /// Indices created before the fork mean the same in both pools. Indices created after it do
    /// not (the same index can denote different terms), so terms of one pool must never be used
    /// with the other. Checkpoints taken before the fork can no longer be rolled back to.
    pub fn fork(&mut self) -> TermPool {
        self.seal();
        self.shallow_clone()
    }

    /// Ends the mutable life of the pool: the result can be shared between threads and read, and
    /// [thawed](FrozenPool::thaw) any number of times.
    pub fn freeze(mut self) -> FrozenPool {
        self.seal();
        FrozenPool(self)
    }
}

/// An immutable snapshot of a pool: all its sorts, operators and terms, readable (it derefs to
/// [`TermPool`]'s `&self` methods) and cheap to clone. [`thaw`](Self::thaw) starts a new mutable
/// pool on top of it, without copying.
pub struct FrozenPool(TermPool);

impl FrozenPool {
    /// A new pool that contains everything of the snapshot and can be extended.
    pub fn thaw(&self) -> TermPool {
        self.0.shallow_clone()
    }
}

impl std::fmt::Debug for FrozenPool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FrozenPool")
            .field("terms", &self.0.num_terms())
            .finish()
    }
}

impl Clone for FrozenPool {
    fn clone(&self) -> Self {
        FrozenPool(self.0.shallow_clone())
    }
}

impl std::ops::Deref for FrozenPool {
    type Target = TermPool;
    fn deref(&self) -> &TermPool {
        &self.0
    }
}
