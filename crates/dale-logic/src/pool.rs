//! The [`TermPool`]: one container for the instantiated sorts, instantiated operators and the
//! hash-consed terms of a logic.
//!
//! * Everything is append-only; an index handed out stays valid (until a [`rollback`] past it).
//! * Equal sorts / operators / terms have equal indices, so comparison is `==` on a `u32`.
//! * A term is a small header; its arguments live in one shared vector.
//!
//! Concurrency: the pool is single-threaded for now. The plan is to keep the same layout but put
//! the vectors in chunked (address-stable) storage and shard the intern tables behind locks, so
//! that readers need no lock at all.
//!
//! [`rollback`]: TermPool::rollback

use std::collections::HashSet;
use std::ops::Index;
use std::sync::Arc;

use crate::error::LogicError;
use crate::ids::{OpDeclId, OpIdx, SortDeclId, SortIdx, TermIdx};
use crate::sig::{GenArgPat, GenKind, GenParam, MetaValue, OpFlags, OpKind, Signature, SortPat};
use crate::table::{RawTable, mix};

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
    pub decl: SortDeclId,
    pub args: Box<[GenArg]>,
    /// All proper ancestors except the top sort, sorted by index.
    ancestors: Box<[SortIdx]>,
}

impl SortData {
    pub fn ancestors(&self) -> &[SortIdx] {
        &self.ancestors
    }
}

/// An instantiated operator, e.g. `ifThenElse<int>` or `var<int>(0)`.
#[derive(Clone, Debug)]
pub struct OpData {
    pub decl: OpDeclId,
    pub generics: Box<[GenArg]>,
    pub meta: Box<[MetaValue]>,
    pub arg_sorts: Box<[SortIdx]>,
    pub result: SortIdx,
    pub flags: OpFlags,
    /// `(argument position, sort of the bound variable)`.
    pub bind: Option<(u32, SortIdx)>,
    /// The de Bruijn index for bound variable operators.
    pub var_index: Option<u64>,
}

impl OpData {
    #[inline]
    pub fn arity(&self) -> usize {
        self.arg_sorts.len()
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
    /// Operator and all subterms rigid (independent of the program state).
    #[inline]
    pub fn is_rigid(&self) -> bool {
        self.rigid
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Checkpoint {
    sorts: usize,
    ops: usize,
    terms: usize,
    args: usize,
}

fn hash_gen(h: u64, a: &GenArg) -> u64 {
    match a {
        GenArg::Sort(s) => mix(mix(h, 1), s.0 as u64),
        GenArg::Const(t) => mix(mix(h, 2), t.0 as u64),
    }
}

fn hash_sort(decl: SortDeclId, args: &[GenArg]) -> u64 {
    args.iter().fold(mix(0x5017, decl.0 as u64), hash_gen)
}

fn hash_op(decl: OpDeclId, generics: &[GenArg], meta: &[MetaValue]) -> u64 {
    let h = generics.iter().fold(mix(0x0b, decl.0 as u64), hash_gen);
    meta.iter()
        .fold(h, |h, MetaValue::Nat(n)| mix(mix(h, 3), *n))
}

#[inline]
fn hash_term(op: OpIdx, args: &[TermIdx]) -> u64 {
    args.iter()
        .fold(mix(0x7e, op.0 as u64), |h, a| mix(h, a.0 as u64))
}

pub struct TermPool {
    pub(crate) sig: Arc<Signature>,
    sorts: Vec<SortData>,
    sort_table: RawTable,
    ops: Vec<OpData>,
    op_table: RawTable,
    terms: Vec<Term>,
    args: Vec<TermIdx>,
    term_table: RawTable,
    top: SortIdx,
    formula: SortIdx,
    pub(crate) var_decl: Option<OpDeclId>,
}

impl Index<TermIdx> for TermPool {
    type Output = Term;
    #[inline]
    fn index(&self, i: TermIdx) -> &Term {
        &self.terms[i.0 as usize]
    }
}

impl Index<SortIdx> for TermPool {
    type Output = SortData;
    #[inline]
    fn index(&self, i: SortIdx) -> &SortData {
        &self.sorts[i.0 as usize]
    }
}

impl Index<OpIdx> for TermPool {
    type Output = OpData;
    #[inline]
    fn index(&self, i: OpIdx) -> &OpData {
        &self.ops[i.0 as usize]
    }
}

impl TermPool {
    pub fn new(sig: Arc<Signature>) -> Self {
        let mut pool = TermPool {
            var_decl: sig.bound_var(),
            sig,
            sorts: Vec::new(),
            sort_table: RawTable::new(),
            ops: Vec::new(),
            op_table: RawTable::new(),
            terms: Vec::new(),
            args: Vec::new(),
            term_table: RawTable::new(),
            top: SortIdx(u32::MAX),
            formula: SortIdx(u32::MAX),
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
        let t = &self.terms[t.0 as usize];
        &self.args[t.args_start as usize..(t.args_start + t.args_len) as usize]
    }

    #[inline]
    pub fn sort_of(&self, t: TermIdx) -> SortIdx {
        self.terms[t.0 as usize].sort
    }

    #[inline]
    pub fn op_of(&self, t: TermIdx) -> OpIdx {
        self.terms[t.0 as usize].op
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

    /// `a <= b` in the sort hierarchy (reflexive).
    #[inline]
    pub fn is_subsort(&self, a: SortIdx, b: SortIdx) -> bool {
        a == b || b == self.top || self.sorts[a.0 as usize].ancestors.binary_search(&b).is_ok()
    }

    // ---------------------------------------------------------------- sorts

    /// The sort `decl<args>`. Sorts are invariant in their arguments.
    pub fn mk_sort(&mut self, decl: SortDeclId, args: &[GenArg]) -> Result<SortIdx, LogicError> {
        let h = hash_sort(decl, args);
        let found = self.sort_table.find(h, |i| {
            let d = &self.sorts[i as usize];
            d.decl == decl && *d.args == *args
        });
        if let Ok(i) = found {
            return Ok(SortIdx(i));
        }
        let sig = self.sig.clone();
        let d = sig.sort_decl(decl);
        self.check_generics(&d.params, args)?;
        let mut anc = Vec::new();
        for e in &d.extends {
            let s = self.inst_sort(e, args)?;
            anc.push(s);
            anc.extend_from_slice(&self.sorts[s.0 as usize].ancestors);
        }
        anc.retain(|&s| s != self.top);
        anc.sort_unstable();
        anc.dedup();
        // Creating the supersorts may have changed the table: look the slot up again.
        let slot = self
            .sort_table
            .find(h, |i| {
                let d = &self.sorts[i as usize];
                d.decl == decl && *d.args == *args
            })
            .expect_err("sort created while instantiating its supersorts");
        let idx = self.sorts.len() as u32;
        self.sorts.push(SortData {
            decl,
            args: args.into(),
            ancestors: anc.into(),
        });
        let sorts = &self.sorts;
        self.sort_table.insert_at(slot, idx, |i| {
            let d = &sorts[i as usize];
            hash_sort(d.decl, &d.args)
        });
        Ok(SortIdx(idx))
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
                    let term = &self.terms[t.0 as usize];
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
        let eq = |ops: &Vec<OpData>, i: u32| {
            let d = &ops[i as usize];
            d.decl == decl && *d.generics == *generics && *d.meta == *meta
        };
        if let Ok(i) = self.op_table.find(h, |i| eq(&self.ops, i)) {
            return Ok(OpIdx(i));
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
            Some(b) => Some((b.arg, self.inst_sort(&b.var_sort, generics)?)),
            None => None,
        };
        let var_index = match (d.kind, meta) {
            (OpKind::BoundVar, [MetaValue::Nat(n)]) => Some(*n),
            _ => None,
        };
        // Instantiating sorts does not touch the operator table, so the slot is still valid.
        let slot = self
            .op_table
            .find(h, |i| eq(&self.ops, i))
            .expect_err("operator created twice");
        let idx = self.ops.len() as u32;
        self.ops.push(OpData {
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
        self.op_table.insert_at(slot, idx, |i| {
            let d = &ops[i as usize];
            hash_op(d.decl, &d.generics, &d.meta)
        });
        Ok(OpIdx(idx))
    }

    // ---------------------------------------------------------------- terms

    /// Creates (or finds) the term `op(args)`, checking the number and sorts of the arguments
    /// and, for binders, the sorts of the variables bound in the bound argument.
    pub fn mk(&mut self, op: OpIdx, args: &[TermIdx]) -> Result<TermIdx, LogicError> {
        let od = &self.ops[op.0 as usize];
        if od.arg_sorts.len() != args.len() {
            return Err(LogicError::ArgCount {
                expected: od.arg_sorts.len(),
                found: args.len(),
            });
        }
        for (index, (a, &expected)) in args.iter().zip(&od.arg_sorts).enumerate() {
            let found = self.terms[a.0 as usize].sort;
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
        let slot = match self.find_term(h, op, args) {
            Ok(t) => return Ok(t),
            Err(slot) => slot,
        };
        // Only new terms need the binder check: an existing one has passed it.
        if let Some((i, var_sort)) = bind {
            self.check_bound(args[i as usize], 0, var_sort, &mut HashSet::new())?;
        }
        Ok(self.insert_term(slot, h, op, args))
    }

    /// Like [`mk`](Self::mk) but without any check. The caller guarantees the argument count,
    /// argument sorts and binder sorts. Violations are not memory unsafe but make the term
    /// ill-formed.
    pub fn mk_unchecked(&mut self, op: OpIdx, args: &[TermIdx]) -> TermIdx {
        debug_assert_eq!(self.ops[op.0 as usize].arg_sorts.len(), args.len());
        let h = hash_term(op, args);
        match self.find_term(h, op, args) {
            Ok(t) => t,
            Err(slot) => self.insert_term(slot, h, op, args),
        }
    }

    #[inline]
    fn find_term(&self, h: u64, op: OpIdx, args: &[TermIdx]) -> Result<TermIdx, usize> {
        self.term_table
            .find(h, |i| {
                let t = &self.terms[i as usize];
                t.op == op && self.args_of(t) == args
            })
            .map(TermIdx)
    }

    #[inline]
    fn args_of(&self, t: &Term) -> &[TermIdx] {
        &self.args[t.args_start as usize..(t.args_start + t.args_len) as usize]
    }

    fn insert_term(&mut self, slot: usize, h: u64, op: OpIdx, args: &[TermIdx]) -> TermIdx {
        let _ = h;
        let od = &self.ops[op.0 as usize];
        let mut loose = match od.var_index {
            Some(n) => u32::try_from(n + 1).expect("de Bruijn index too large"),
            None => 0,
        };
        let mut rigid = od.flags.rigid;
        for (i, a) in args.iter().enumerate() {
            let t = &self.terms[a.0 as usize];
            let l = match od.bind {
                Some((b, _)) if b as usize == i => t.loose.saturating_sub(1),
                _ => t.loose,
            };
            loose = loose.max(l);
            rigid &= t.rigid;
        }
        let sort = od.result;
        let idx = self.terms.len() as u32;
        let args_start = self.args.len() as u32;
        self.args.extend_from_slice(args);
        self.terms.push(Term {
            op,
            sort,
            args_start,
            args_len: args.len() as u32,
            loose,
            rigid,
        });
        let (terms, all_args) = (&self.terms, &self.args);
        self.term_table.insert_at(slot, idx, |i| {
            let t = &terms[i as usize];
            hash_term(
                t.op,
                &all_args[t.args_start as usize..(t.args_start + t.args_len) as usize],
            )
        });
        TermIdx(idx)
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
        let term = &self.terms[t.0 as usize];
        if term.loose <= depth || !seen.insert((t, depth)) {
            return Ok(());
        }
        let od = &self.ops[term.op.0 as usize];
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
            let d = match od.bind {
                Some((b, _)) if b as usize == i => depth + 1,
                _ => depth,
            };
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
                let sd = &self.sorts[s.0 as usize];
                let target = if sd.decl == *decl {
                    Some(s)
                } else if exact {
                    None
                } else {
                    sd.ancestors
                        .iter()
                        .copied()
                        .find(|a| self.sorts[a.0 as usize].decl == *decl)
                };
                let Some(t) = target else { return false };
                let sargs = &self.sorts[t.0 as usize].args;
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
            sorts: self.sorts.len(),
            ops: self.ops.len(),
            terms: self.terms.len(),
            args: self.args.len(),
        }
    }

    /// Drops everything created after `cp`. All indices created since are invalid afterwards.
    pub fn rollback(&mut self, cp: Checkpoint) {
        assert!(
            cp.sorts <= self.sorts.len()
                && cp.ops <= self.ops.len()
                && cp.terms <= self.terms.len(),
            "checkpoint from the future"
        );
        self.sorts.truncate(cp.sorts);
        self.ops.truncate(cp.ops);
        self.terms.truncate(cp.terms);
        self.args.truncate(cp.args);
        let sorts = &self.sorts;
        self.sort_table.reset_to(cp.sorts as u32, |i| {
            let d = &sorts[i as usize];
            hash_sort(d.decl, &d.args)
        });
        let ops = &self.ops;
        self.op_table.reset_to(cp.ops as u32, |i| {
            let d = &ops[i as usize];
            hash_op(d.decl, &d.generics, &d.meta)
        });
        let (terms, args) = (&self.terms, &self.args);
        self.term_table.reset_to(cp.terms as u32, |i| {
            let t = &terms[i as usize];
            hash_term(
                t.op,
                &args[t.args_start as usize..(t.args_start + t.args_len) as usize],
            )
        });
    }
}
