//! Printing of sorts, operators and terms. Names live in the signature as interned symbols, so
//! printing needs the interner.

use std::fmt::{self, Display, Formatter};

use dale_util::symbol::{Interner, Symbol};

use crate::ids::{OpIdx, SortIdx, TermIdx};
use crate::pool::{GenArg, TermPool};
use crate::sig::MetaValue;

pub struct Show<'a, T> {
    pool: &'a TermPool,
    names: &'a Interner<Symbol>,
    what: T,
}

impl TermPool {
    pub fn show_term<'a>(&'a self, names: &'a Interner<Symbol>, t: TermIdx) -> Show<'a, TermIdx> {
        Show {
            pool: self,
            names,
            what: t,
        }
    }
    pub fn show_sort<'a>(&'a self, names: &'a Interner<Symbol>, s: SortIdx) -> Show<'a, SortIdx> {
        Show {
            pool: self,
            names,
            what: s,
        }
    }
    pub fn show_op<'a>(&'a self, names: &'a Interner<Symbol>, o: OpIdx) -> Show<'a, OpIdx> {
        Show {
            pool: self,
            names,
            what: o,
        }
    }
}

fn write_gen_args(
    pool: &TermPool,
    names: &Interner<Symbol>,
    args: &[GenArg],
    f: &mut Formatter<'_>,
) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }
    f.write_str("<")?;
    for (i, a) in args.iter().enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        match a {
            GenArg::Sort(s) => write!(f, "{}", pool.show_sort(names, *s))?,
            GenArg::Const(t) => write!(f, "{}", pool.show_term(names, *t))?,
        }
    }
    f.write_str(">")
}

impl Display for Show<'_, SortIdx> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let d = &self.pool[self.what];
        f.write_str(name_of(self, self.pool.sig.sort_decl(d.decl()).name))?;
        write_gen_args(self.pool, self.names, d.args(), f)
    }
}

fn name_of<'a, T>(s: &'a Show<'_, T>, sym: Symbol) -> &'a str {
    s.names.get(sym)
}

impl Display for Show<'_, OpIdx> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let d = &self.pool[self.what];
        f.write_str(name_of(self, self.pool.sig.op_decl(d.decl()).name))?;
        write_gen_args(self.pool, self.names, d.generics(), f)?;
        for MetaValue::Nat(n) in d.meta().iter() {
            write!(f, "[{n}]")?;
        }
        Ok(())
    }
}

impl Display for Show<'_, TermIdx> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op = self.pool.op_of(self.what);
        write!(f, "{}", self.pool.show_op(self.names, op))?;
        let args = self.pool.subs_of(self.what);
        if !args.is_empty() {
            f.write_str("(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{}", self.pool.show_term(self.names, *a))?;
            }
            f.write_str(")")?;
        }
        Ok(())
    }
}
