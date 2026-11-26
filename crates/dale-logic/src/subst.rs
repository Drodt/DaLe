//! Operations on de Bruijn variables: building variables, lifting and instantiating binders.

use std::collections::HashMap;

use crate::error::LogicError;
use crate::ids::{SortIdx, TermIdx};
use crate::pool::{GenArg, TermPool};
use crate::sig::MetaValue;

type Cache = HashMap<(TermIdx, u32), TermIdx>;

impl TermPool {
    /// The bound variable `var<sort>(index)`; index 0 refers to the closest enclosing binder.
    pub fn mk_var(&mut self, sort: SortIdx, index: u64) -> Result<TermIdx, LogicError> {
        let decl = self.var_decl.ok_or(LogicError::NoBoundVarOp)?;
        let op = self.mk_op(decl, &[GenArg::Sort(sort)], &[MetaValue::Nat(index)])?;
        self.mk(op, &[])
    }

    /// Adds `amount` to the index of every variable of `t` that is not bound within `t`.
    pub fn lift(&mut self, t: TermIdx, amount: u32) -> Result<TermIdx, LogicError> {
        self.shift(t, amount, 0, &mut Cache::new())
    }

    fn shift(
        &mut self,
        t: TermIdx,
        amount: u32,
        cutoff: u32,
        cache: &mut Cache,
    ) -> Result<TermIdx, LogicError> {
        let term = self[t];
        if term.loose() <= cutoff || amount == 0 {
            return Ok(t);
        }
        if let Some(&r) = cache.get(&(t, cutoff)) {
            return Ok(r);
        }
        let od = &self[term.op()];
        let (var_index, bind) = (od.var_index(), od.bind());
        let r = if let Some(n) = var_index {
            self.mk_var(term.sort(), n + amount as u64)?
        } else {
            let old = self.subs_of(t).to_vec();
            let mut new = Vec::with_capacity(old.len());
            for (i, a) in old.into_iter().enumerate() {
                let c = match bind {
                    Some((b, _)) if b as usize == i => cutoff + 1,
                    _ => cutoff,
                };
                new.push(self.shift(a, amount, c, cache)?);
            }
            self.mk_unchecked(term.op(), &new)
        };
        cache.insert((t, cutoff), r);
        Ok(r)
    }

    /// For a binder term `Q(.., body, ..)`, the body with the bound variable replaced by `repl`
    /// (whose sort must be a subsort of the variable's sort). Variables of `repl` that are free
    /// refer to the binders around `binder`.
    pub fn instantiate(&mut self, binder: TermIdx, repl: TermIdx) -> Result<TermIdx, LogicError> {
        let (b, var_sort) = self[self.op_of(binder)]
            .bind()
            .ok_or(LogicError::NotABinder)?;
        let found = self.sort_of(repl);
        if !self.is_subsort(found, var_sort) {
            return Err(LogicError::BindSort {
                expected: var_sort,
                found,
            });
        }
        let body = self.subs_of(binder)[b as usize];
        self.subst(body, 0, repl, &mut Cache::new())
    }

    fn subst(
        &mut self,
        t: TermIdx,
        depth: u32,
        repl: TermIdx,
        cache: &mut Cache,
    ) -> Result<TermIdx, LogicError> {
        let term = self[t];
        if term.loose() <= depth {
            return Ok(t);
        }
        if let Some(&r) = cache.get(&(t, depth)) {
            return Ok(r);
        }
        let od = &self[term.op()];
        let (var_index, bind) = (od.var_index(), od.bind());
        let r = if let Some(n) = var_index {
            if n == depth as u64 {
                self.lift(repl, depth)?
            } else {
                self.mk_var(term.sort(), n - 1)?
            }
        } else {
            let old = self.subs_of(t).to_vec();
            let mut new = Vec::with_capacity(old.len());
            for (i, a) in old.into_iter().enumerate() {
                let d = match bind {
                    Some((b, _)) if b as usize == i => depth + 1,
                    _ => depth,
                };
                new.push(self.subst(a, d, repl, cache)?);
            }
            self.mk_unchecked(term.op(), &new)
        };
        cache.insert((t, depth), r);
        Ok(r)
    }
}
