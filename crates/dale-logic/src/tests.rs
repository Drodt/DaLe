use std::sync::Arc;

use dale_util::symbol::{Interner, Symbol};

use crate::*;

struct Fx {
    names: Interner<Symbol>,
    pool: TermPool,
    any: SortDeclId,
    int: SortDeclId,
    nat: SortDeclId,
    seq: SortDeclId,
    array: SortDeclId,
    and: OpDeclId,
    all: OpDeclId,
    ite: OpDeclId,
    mkseq: OpDeclId,
    p: OpDeclId,
    zero: OpDeclId,
    three: OpDeclId,
    one_int: OpDeclId,
    var: OpDeclId,
    le: OpDeclId,
}

fn fx() -> Fx {
    let mut n = Interner::<Symbol>::default();
    let mut b = SignatureBuilder::new();
    let s = |b: &mut SignatureBuilder,
             n: &mut Interner<Symbol>,
             name: &str,
             kind: SortKind,
             params: Vec<GenParam>,
             extends: Vec<SortPat>| {
        let mut d = SortDecl::new(n.intern(name));
        d.kind = kind;
        d.params = params;
        d.extends = extends;
        b.add_sort(d).unwrap()
    };
    let any = s(&mut b, &mut n, "Any", SortKind::Top, vec![], vec![]);
    let form = s(&mut b, &mut n, "Formula", SortKind::Formula, vec![], vec![]);
    let int = s(&mut b, &mut n, "int", SortKind::Normal, vec![], vec![]);
    let nat = s(
        &mut b,
        &mut n,
        "nat",
        SortKind::Normal,
        vec![],
        vec![SortPat::simple(int)],
    );
    let e = n.intern("E");
    let seq = s(
        &mut b,
        &mut n,
        "Seq",
        SortKind::Normal,
        vec![GenParam {
            name: e,
            kind: GenKind::Sort,
        }],
        vec![],
    );
    let nn = n.intern("N");
    let array = s(
        &mut b,
        &mut n,
        "Array",
        SortKind::Normal,
        vec![
            GenParam {
                name: e,
                kind: GenKind::Sort,
            },
            GenParam {
                name: nn,
                kind: GenKind::Const(SortPat::simple(int)),
            },
        ],
        vec![],
    );
    let f = SortPat::simple(form);
    let op = |b: &mut SignatureBuilder, d: OpDecl| b.add_op(d).unwrap();
    let and = op(
        &mut b,
        OpDecl::new(n.intern("and"), vec![f.clone(), f.clone()], f.clone()),
    );
    let le = op(
        &mut b,
        OpDecl::new(
            n.intern("le"),
            vec![SortPat::simple(int), SortPat::simple(int)],
            f.clone(),
        ),
    );
    let p = op(&mut b, OpDecl::new(n.intern("p"), vec![], f.clone()));
    let zero = op(
        &mut b,
        OpDecl::new(n.intern("zero"), vec![], SortPat::simple(nat)),
    );
    let one_int = op(
        &mut b,
        OpDecl::new(n.intern("negone"), vec![], SortPat::simple(int)),
    );
    let three = op(
        &mut b,
        OpDecl::new(n.intern("three"), vec![], SortPat::simple(int)),
    );
    let sg = |nm: &mut Interner<Symbol>| GenParam {
        name: nm.intern("S"),
        kind: GenKind::Sort,
    };
    let mut d = OpDecl::new(n.intern("all"), vec![f.clone()], f.clone());
    d.generics = vec![sg(&mut n)];
    d.bind = Some(Binding {
        arg: 0,
        var_sort: SortPat::Param(0),
    });
    let all = op(&mut b, d);
    let mut d = OpDecl::new(
        n.intern("ifThenElse"),
        vec![f.clone(), SortPat::Param(0), SortPat::Param(0)],
        SortPat::Param(0),
    );
    d.generics = vec![sg(&mut n)];
    let ite = op(&mut b, d);
    let mut d = OpDecl::new(
        n.intern("mkSeq"),
        vec![SortPat::Param(0)],
        SortPat::Inst {
            decl: seq,
            args: vec![GenArgPat::Sort(SortPat::Param(0))],
        },
    );
    d.generics = vec![sg(&mut n)];
    let mkseq = op(&mut b, d);
    let mut d = OpDecl::new(n.intern("var"), vec![], SortPat::Param(0));
    d.kind = OpKind::BoundVar;
    d.generics = vec![sg(&mut n)];
    d.meta = vec![MetaKind::Nat];
    let var = op(&mut b, d);
    let sig = Arc::new(b.finish().unwrap());
    Fx {
        names: n,
        pool: TermPool::new(sig),
        any,
        int,
        nat,
        seq,
        array,
        and,
        all,
        ite,
        mkseq,
        p,
        zero,
        three,
        one_int,
        var,
        le,
    }
}

impl Fx {
    fn sort(&mut self, d: SortDeclId) -> SortIdx {
        self.pool.mk_sort(d, &[]).unwrap()
    }
    fn c(&mut self, d: OpDeclId) -> TermIdx {
        self.pool.mk_inferred(d, &[], &[]).unwrap()
    }
    fn and(&mut self, a: TermIdx, b: TermIdx) -> TermIdx {
        self.pool.mk_inferred(self.and, &[], &[a, b]).unwrap()
    }
    fn all(&mut self, s: SortIdx, body: TermIdx) -> Result<TermIdx, LogicError> {
        let op = self.pool.mk_op(self.all, &[GenArg::Sort(s)], &[])?;
        self.pool.mk(op, &[body])
    }
    fn var(&mut self, s: SortIdx, i: u64) -> TermIdx {
        self.pool.mk_var(s, i).unwrap()
    }
}

#[test]
fn sharing_and_display() {
    let mut f = fx();
    let p = f.c(f.p);
    let a = f.and(p, p);
    let b = f.and(p, p);
    assert_eq!(a, b);
    let c = f.and(a, p);
    assert_eq!(f.pool.subs_of(c), &[a, p]);
    assert_eq!(
        f.pool.show_term(&f.names, c).to_string(),
        "and(and(p, p), p)"
    );
    assert!(f.pool.is_formula(c));
    assert!(f.pool[c].is_closed());
}

#[test]
fn arg_sort_errors() {
    let mut f = fx();
    let z = f.c(f.zero);
    let op = f.pool.mk_op(f.and, &[], &[]).unwrap();
    assert!(matches!(
        f.pool.mk(op, &[z, z]),
        Err(LogicError::ArgSort { index: 0, .. })
    ));
    let p = f.c(f.p);
    assert!(matches!(
        f.pool.mk(op, &[p]),
        Err(LogicError::ArgCount {
            expected: 2,
            found: 1
        })
    ));
}

#[test]
fn subsorts() {
    let mut f = fx();
    let (any, int, nat, form) = (
        f.sort(f.any),
        f.sort(f.int),
        f.sort(f.nat),
        f.pool.formula(),
    );
    assert_eq!(f.pool.top(), any);
    assert!(f.pool.is_subsort(nat, int));
    assert!(!f.pool.is_subsort(int, nat));
    assert!(
        f.pool.is_subsort(nat, any) && f.pool.is_subsort(form, any) && f.pool.is_subsort(any, any)
    );
    assert!(!f.pool.is_subsort(nat, form));
}

#[test]
fn parametric_sorts_are_invariant() {
    let mut f = fx();
    let (int, nat) = (f.sort(f.int), f.sort(f.nat));
    let si = f.pool.mk_sort(f.seq, &[GenArg::Sort(int)]).unwrap();
    let sn = f.pool.mk_sort(f.seq, &[GenArg::Sort(nat)]).unwrap();
    assert_ne!(si, sn);
    assert!(!f.pool.is_subsort(sn, si));
    assert_eq!(f.pool.mk_sort(f.seq, &[GenArg::Sort(int)]).unwrap(), si);
    assert_eq!(f.pool.show_sort(&f.names, si).to_string(), "Seq<int>");
    assert!(matches!(
        f.pool.mk_sort(f.seq, &[]),
        Err(LogicError::GenericArity {
            expected: 1,
            found: 0
        })
    ));
}

#[test]
fn inference() {
    let mut f = fx();
    let (z, m) = (f.c(f.zero), f.c(f.one_int));
    let p = f.c(f.p);
    let t = f.pool.mk_inferred(f.ite, &[], &[p, z, m]).unwrap();
    let int = f.sort(f.int);
    assert_eq!(f.pool.sort_of(t), int);
    let t2 = f.pool.mk_inferred(f.ite, &[], &[p, m, z]).unwrap();
    assert_ne!(t, t2);
    assert_eq!(f.pool.op_of(t), f.pool.op_of(t2));
    assert_eq!(
        f.pool.show_op(&f.names, f.pool.op_of(t)).to_string(),
        "ifThenElse<int>"
    );
    // mismatching sorts: no common sort without lub
    assert!(matches!(
        f.pool.mk_inferred(f.ite, &[], &[p, z, p]),
        Err(LogicError::InferMismatch(2))
    ));
    let s = f.pool.mk_inferred(f.mkseq, &[], &[z]).unwrap();
    assert_eq!(
        f.pool.show_sort(&f.names, f.pool.sort_of(s)).to_string(),
        "Seq<nat>"
    );
    // `p` has no arguments to infer from
    assert!(matches!(
        f.pool.mk_inferred(f.var, &[MetaValue::Nat(0)], &[]),
        Err(LogicError::CannotInfer(0))
    ));
}

#[test]
fn const_generics() {
    let mut f = fx();
    let int = f.sort(f.int);
    let three = f.c(f.three);
    let a = f
        .pool
        .mk_sort(f.array, &[GenArg::Sort(int), GenArg::Const(three)])
        .unwrap();
    assert_eq!(
        f.pool.show_sort(&f.names, a).to_string(),
        "Array<int, three>"
    );
    let v = f.var(int, 0);
    assert!(matches!(
        f.pool
            .mk_sort(f.array, &[GenArg::Sort(int), GenArg::Const(v)]),
        Err(LogicError::ConstNotClosed(1))
    ));
    let p = f.c(f.p);
    assert!(matches!(
        f.pool
            .mk_sort(f.array, &[GenArg::Sort(int), GenArg::Const(p)]),
        Err(LogicError::ConstSort { index: 1, .. })
    ));
    assert!(matches!(
        f.pool
            .mk_sort(f.array, &[GenArg::Const(three), GenArg::Const(three)]),
        Err(LogicError::GenericKind(0))
    ));
}

#[test]
fn de_bruijn_closedness_and_binder_sorts() {
    let mut f = fx();
    let (int, nat) = (f.sort(f.int), f.sort(f.nat));
    let v0 = f.var(int, 0);
    assert_eq!(f.pool[v0].loose(), 1);
    let v2 = f.var(int, 2);
    assert_eq!(f.pool[v2].loose(), 3);
    let z = f.c(f.zero);
    // le(var0, 0) needs one binder
    let body = f.pool.mk_inferred(f.le, &[], &[v0, z]).unwrap();
    assert_eq!(f.pool[body].loose(), 1);
    let q = f.all(int, body).unwrap();
    assert!(f.pool[q].is_closed());
    assert_eq!(
        f.pool.show_term(&f.names, q).to_string(),
        "all<int>(le(var<int>[0], zero))"
    );
    // nested: the inner binder closes var0 only, var1 still needs one
    let v1 = f.var(int, 1);
    let inner_body = f.pool.mk_inferred(f.le, &[], &[v0, v1]).unwrap();
    let inner = f.all(int, inner_body).unwrap();
    assert_eq!(f.pool[inner].loose(), 1);
    let outer = f.all(int, inner).unwrap();
    assert!(f.pool[outer].is_closed());
    // wrong variable sort: a nat variable bound by an int quantifier
    let vn = f.var(nat, 0);
    let bad = f.pool.mk_inferred(f.le, &[], &[vn, z]).unwrap();
    assert!(matches!(f.all(int, bad), Err(LogicError::BindSort { .. })));
    // ... but a nat variable that refers to an outer binder is fine
    let vn1 = f.var(nat, 1);
    let ok = f.pool.mk_inferred(f.le, &[], &[vn1, v0]).unwrap();
    let inner = f.all(int, ok).unwrap();
    assert!(f.all(nat, inner).is_ok());
    // index too large
    assert!(matches!(
        f.pool.mk_var(int, u32::MAX as u64),
        Err(LogicError::VarIndexTooLarge)
    ));
}

#[test]
fn instantiate_and_lift() {
    let mut f = fx();
    let (int, nat) = (f.sort(f.int), f.sort(f.nat));
    let z = f.c(f.zero);
    let m = f.c(f.one_int);
    let v0 = f.var(int, 0);
    let v1 = f.var(int, 1);
    // all x. all y. le(x, y)  == all(all(le(var1, var0)))
    let body = f.pool.mk_inferred(f.le, &[], &[v1, v0]).unwrap();
    let inner = f.all(int, body).unwrap();
    let outer = f.all(int, inner).unwrap();
    // instantiate the outer variable with 0: all y. le(0, y)
    let r = f.pool.instantiate(outer, z).unwrap();
    let want_body = f.pool.mk_inferred(f.le, &[], &[z, v0]).unwrap();
    assert_eq!(r, f.all(int, want_body).unwrap());
    // a replacement with a free variable is lifted under the inner binder
    let free = f.var(int, 0);
    let r = f.pool.instantiate(outer, free).unwrap();
    let v1b = f.var(int, 1);
    let want_body = f.pool.mk_inferred(f.le, &[], &[v1b, v0]).unwrap();
    assert_eq!(r, f.all(int, want_body).unwrap());
    // instantiating with a subsort is fine, a wrong sort is not
    assert!(f.pool.instantiate(outer, z).is_ok());
    let p = f.c(f.p);
    assert!(matches!(
        f.pool.instantiate(outer, p),
        Err(LogicError::BindSort { .. })
    ));
    assert!(matches!(
        f.pool.instantiate(p, z),
        Err(LogicError::NotABinder)
    ));
    // lift
    let l = f.pool.lift(v0, 3).unwrap();
    assert_eq!(l, f.var(int, 3));
    assert_eq!(f.pool.lift(z, 3).unwrap(), z);
    let lifted = f.pool.lift(inner, 2).unwrap();
    // inner = all(le(var1, var0)): var1 is free; lifted by 2 -> var3
    let v3 = f.var(int, 3);
    let wb = f.pool.mk_inferred(f.le, &[], &[v3, v0]).unwrap();
    assert_eq!(lifted, f.all(int, wb).unwrap());
    let _ = (m, nat);
}

#[test]
fn rollback() {
    let mut f = fx();
    let p = f.c(f.p);
    let cp = f.pool.checkpoint();
    let (sorts, ops, terms) = (f.pool.num_sorts(), f.pool.num_ops(), f.pool.num_terms());
    let a = f.and(p, p);
    let int = f.sort(f.int);
    f.pool.mk_sort(f.seq, &[GenArg::Sort(int)]).unwrap();
    let _ = a;
    f.pool.rollback(cp);
    assert_eq!(
        (f.pool.num_sorts(), f.pool.num_ops(), f.pool.num_terms()),
        (sorts, ops, terms)
    );
    let a2 = f.and(p, p);
    assert_eq!(f.pool.subs_of(a2), &[p, p]);
    assert_eq!(f.pool.mk_sort(f.any, &[]).unwrap(), f.pool.top());
}

#[test]
fn many_terms_grow_the_table() {
    let mut f = fx();
    let mut t = f.c(f.p);
    let p = t;
    for _ in 0..2000 {
        t = f.and(t, p);
    }
    let mut u = p;
    for _ in 0..2000 {
        u = f.and(u, p);
    }
    assert_eq!(t, u);
    assert_eq!(f.pool.num_terms(), 2001);
}

#[test]
fn signature_errors() {
    let mut n = Interner::<Symbol>::default();
    let mut b = SignatureBuilder::new();
    let top = SortDecl {
        kind: SortKind::Top,
        ..SortDecl::new(n.intern("Any"))
    };
    b.add_sort(top.clone()).unwrap();
    assert_eq!(b.add_sort(top).unwrap_err(), SigError::TopRedeclared);
    // forward reference
    let mut d = SortDecl::new(n.intern("x"));
    d.extends = vec![SortPat::simple(SortDeclId::from_u32(7))];
    assert!(matches!(b.add_sort(d), Err(SigError::BadSortPattern(_))));
    // malformed bound variable
    let mut d = OpDecl::new(
        n.intern("var"),
        vec![],
        SortPat::simple(SortDeclId::from_u32(0)),
    );
    d.kind = OpKind::BoundVar;
    assert!(matches!(b.add_op(d), Err(SigError::BadBoundVar(_))));
    // binding of a missing argument
    let mut d = OpDecl::new(
        n.intern("q"),
        vec![],
        SortPat::simple(SortDeclId::from_u32(0)),
    );
    d.bind = Some(Binding {
        arg: 0,
        var_sort: SortPat::simple(SortDeclId::from_u32(0)),
    });
    assert!(matches!(b.add_op(d), Err(SigError::BindingOutOfRange(_))));
    assert_eq!(b.finish().unwrap_err(), SigError::MissingFormula);
}

// ------------------------------------------------------------------ sequents

mod sequents {
    use super::*;

    fn atoms(f: &mut Fx) -> (TermIdx, TermIdx, TermIdx) {
        let p = f.c(f.p);
        let pp = f.and(p, p);
        let ppp = f.and(pp, p);
        (p, pp, ppp)
    }

    #[test]
    fn add_remove_and_sharing() {
        let mut f = fx();
        let (p, pp, ppp) = atoms(&mut f);
        let s0 = Sequent::new();
        let c1 = s0.add(&f.pool, Side::Antecedent, p).unwrap();
        assert_eq!(c1.antecedent.added, [p]);
        let s1 = c1.sequent;
        // adding again is rejected and returns an equal sequent
        let c2 = s1.add(&f.pool, Side::Antecedent, p).unwrap();
        assert!(!c2.has_changed());
        assert_eq!(c2.antecedent.rejected, [p]);
        // the same formula on the other side is fine
        let s2 = s1
            .add_all(&f.pool, Side::Succedent, &[pp, ppp, pp])
            .unwrap();
        assert_eq!(s2.succedent.added, [pp, ppp]);
        assert_eq!(s2.succedent.rejected, [pp]);
        let s2 = s2.sequent;
        assert_eq!(s2.succedent().as_slice(), &[pp, ppp]);
        // the untouched side is shared, the old sequent is unchanged
        assert!(std::ptr::eq(
            s1.antecedent().as_slice(),
            s2.antecedent().as_slice()
        ));
        assert!(s1.succedent().is_empty());
        let r = s2.remove(Side::Succedent, 0).unwrap();
        assert_eq!(r.succedent.removed, [pp]);
        assert_eq!(r.sequent.succedent().as_slice(), &[ppp]);
        assert_eq!(s2.len(), 3);
        assert!(matches!(
            s2.remove(Side::Antecedent, 5),
            Err(SequentError::IndexOutOfRange { index: 5, .. })
        ));
        assert_eq!(
            f.pool.show_sequent(&f.names, &s2).to_string(),
            "p ==> and(p, p), and(and(p, p), p)"
        );
    }

    #[test]
    fn only_closed_formulas() {
        let mut f = fx();
        let int = f.sort(f.int);
        let z = f.c(f.zero);
        let v0 = f.var(int, 0);
        let open = f.pool.mk_inferred(f.le, &[], &[v0, z]).unwrap();
        let s = Sequent::new();
        assert_eq!(
            s.add(&f.pool, Side::Antecedent, open).unwrap_err(),
            SequentError::NotClosed(open)
        );
        assert_eq!(
            s.add(&f.pool, Side::Antecedent, z).unwrap_err(),
            SequentError::NotAFormula(z)
        );
        let q = f.all(int, open).unwrap();
        assert!(s.add(&f.pool, Side::Succedent, q).is_ok());
        assert!(
            Sequent::from_formulas(&f.pool, &[q, q], &[])
                .unwrap()
                .antecedent()
                .len()
                == 1
        );
    }

    #[test]
    fn replace_formula_cases() {
        let mut f = fx();
        let (p, pp, ppp) = atoms(&mut f);
        let s = Sequent::from_formulas(&f.pool, &[p, pp, ppp], &[]).unwrap();
        // one-to-one
        let c = s
            .replace_formula(&f.pool, Side::Antecedent, 1, &[ppp])
            .unwrap();
        // ppp is already present: the replaced formula is just removed
        assert_eq!(c.antecedent.rejected, [ppp]);
        assert_eq!(c.antecedent.removed, [pp]);
        assert_eq!(c.sequent.antecedent().as_slice(), &[p, ppp]);
        // one-to-many keeps the place
        let q = f.and(p, ppp);
        let c = s
            .replace_formula(&f.pool, Side::Antecedent, 0, &[q, pp])
            .unwrap();
        assert_eq!(c.antecedent.modified, [(p, q)]);
        assert_eq!(c.antecedent.rejected, [pp]);
        assert_eq!(c.sequent.antecedent().as_slice(), &[q, pp, ppp]);
        // replacing with itself changes nothing
        assert!(
            !s.replace_formula(&f.pool, Side::Antecedent, 2, &[ppp])
                .unwrap()
                .has_changed()
        );
        // to nothing
        let c = s
            .replace_formula(&f.pool, Side::Antecedent, 2, &[])
            .unwrap();
        assert_eq!(c.antecedent.removed, [ppp]);
    }

    #[test]
    fn replace_subterm_and_checks() {
        let mut f = fx();
        let (p, pp, _) = atoms(&mut f);
        let s = Sequent::from_formulas(&f.pool, &[], &[pp]).unwrap();
        // pp = and(p, p): replace the second argument by and(p, p)
        let pos = PosInOccurrence::new(Side::Succedent, 0, PosInTerm::top().down(1));
        assert_eq!(pos.sub_term(&f.pool, &s), Some(p));
        let c = s
            .replace_subterm(&mut f.pool, &pos, pp, SortCheck::Enforce)
            .unwrap();
        let want = f.and(p, pp);
        assert_eq!(c.sequent.succedent().as_slice(), &[want]);
        assert_eq!(c.succedent.modified, [(pp, want)]);
        // ill-sorted replacement: a nat constant where a formula is needed
        let z = f.c(f.zero);
        let bad = s.replace_subterm(&mut f.pool, &pos, z, SortCheck::Enforce);
        assert!(matches!(
            bad,
            Err(SequentError::Term(LogicError::ArgSort { index: 1, .. }))
        ));
        // without sort checks it is the caller's problem; the formula is still a closed formula
        assert!(
            s.replace_subterm(&mut f.pool, &pos, z, SortCheck::Skip)
                .is_ok()
        );
        // nonexistent position
        let nope = PosInOccurrence::new(Side::Succedent, 0, PosInTerm::top().down(7));
        assert_eq!(
            s.replace_subterm(&mut f.pool, &nope, p, SortCheck::Skip)
                .unwrap_err(),
            SequentError::BadPosition
        );
        // replacing the root by a non-formula is caught even without sort checks
        let root = PosInOccurrence::top_level(Side::Succedent, 0);
        assert!(matches!(
            s.replace_subterm(&mut f.pool, &root, z, SortCheck::Skip),
            Err(SequentError::NotAFormula(_))
        ));
        assert_eq!(PosInTerm::top().down(2).up(), Some(PosInTerm::top()));
        assert_eq!(PosInTerm::top().up(), None);
    }

    #[test]
    fn move_between_sides() {
        let mut f = fx();
        let (p, pp, _) = atoms(&mut f);
        let s = Sequent::from_formulas(&f.pool, &[p, pp], &[p]).unwrap();
        let c = s.move_to_other_side(Side::Antecedent, 1).unwrap();
        assert_eq!(c.antecedent.removed, [pp]);
        assert_eq!(c.succedent.added, [pp]);
        assert_eq!(c.sequent.antecedent().as_slice(), &[p]);
        assert_eq!(c.sequent.succedent().as_slice(), &[p, pp]);
        // already on the other side: only removed
        let c = s.move_to_other_side(Side::Antecedent, 0).unwrap();
        assert_eq!(c.succedent.rejected, [p]);
        assert_eq!(c.sequent.succedent().as_slice(), &[p]);
        assert_eq!(c.sequent.antecedent().as_slice(), &[pp]);
    }
}

mod matching {
    use std::ops::ControlFlow;

    use super::*;

    fn rb(f: &mut Fx, name: &str) -> RuleBuilder {
        RuleBuilder::new(f.pool.signature().clone(), f.names.intern(name))
    }

    /// All matches of `rule` at `t`, as the terms bound to `svs`.
    fn matches(f: &Fx, rule: &Rule, seq: &Sequent, t: TermIdx, svs: &[SvId]) -> Vec<Vec<TermIdx>> {
        let mut b = Bindings::new(rule);
        let mut out = Vec::new();
        let _ = match_term(&f.pool, rule, seq, t, &mut b, &mut |b| {
            out.push(svs.iter().map(|s| b.get(*s).unwrap()).collect());
            ControlFlow::Continue(())
        });
        out
    }

    #[test]
    fn repeated_schema_variable() {
        let mut f = fx();
        let mut r = rb(&mut f, "eq_and");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let and = r.app(f.and, &[], &[], &[pv, pv]).unwrap();
        r.find_term(and);
        let rule = r.finish().unwrap();
        let p = f.c(f.p);
        let (c3, c0) = (f.c(f.three), f.c(f.zero));
        let q = f.pool.mk_inferred(f.le, &[], &[c3, c0]).unwrap();
        let (pp, pq) = (f.and(p, p), f.and(p, q));
        let big = f.and(pq, pq);
        let s = Sequent::new();
        assert_eq!(matches(&f, &rule, &s, pp, &[phi]), vec![vec![p]]);
        assert!(matches(&f, &rule, &s, pq, &[phi]).is_empty());
        assert_eq!(matches(&f, &rule, &s, big, &[phi]), vec![vec![pq]]);
        // not an `and` at all
        assert!(matches(&f, &rule, &s, p, &[phi]).is_empty());
    }

    #[test]
    fn sort_requirements_and_slots() {
        let mut f = fx();
        let nat = f.sort(f.nat);
        let mut r = rb(&mut f, "le_nat");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Term, SortReq::Sub(nat));
        let y = r.declare_sv(f.names.intern("y"), SvKind::Term, SortReq::Any);
        let (px, py) = (r.sv(x).unwrap(), r.sv(y).unwrap());
        let le = r.app(f.le, &[], &[], &[px, py]).unwrap();
        r.find_term(le);
        let rule = r.finish().unwrap();
        let (z, three) = (f.c(f.zero), f.c(f.three));
        let ok = f.pool.mk_inferred(f.le, &[], &[z, three]).unwrap();
        let bad = f.pool.mk_inferred(f.le, &[], &[three, z]).unwrap();
        let s = Sequent::new();
        assert_eq!(matches(&f, &rule, &s, ok, &[x, y]), vec![vec![z, three]]);
        assert!(matches(&f, &rule, &s, bad, &[x, y]).is_empty());

        // ifThenElse<S>(c, a, a) with the sort of `a` tied to the generic slot S
        let mut r = rb(&mut f, "ite_same");
        let c = r.declare_sv(f.names.intern("c"), SvKind::Formula, SortReq::Any);
        let a = r.declare_sv(f.names.intern("a"), SvKind::Term, SortReq::Slot(0));
        let (pc, pa) = (r.sv(c).unwrap(), r.sv(a).unwrap());
        let ite = r
            .app(f.ite, &[GenPat::Slot(0)], &[], &[pc, pa, pa])
            .unwrap();
        r.find_term(ite);
        let rule = r.finish().unwrap();
        let p = f.c(f.p);
        let t = f.pool.mk_inferred(f.ite, &[], &[p, three, three]).unwrap();
        let u = f.pool.mk_inferred(f.ite, &[], &[p, three, z]).unwrap();
        let mut b = Bindings::new(&rule);
        let s = Sequent::new();
        let mut hits = 0;
        let int = f.sort(f.int);
        let _ = match_term(&f.pool, &rule, &s, t, &mut b, &mut |b| {
            hits += 1;
            assert_eq!(b.slot(0), Some(GenArg::Sort(int)));
            ControlFlow::Continue(())
        });
        assert_eq!(hits, 1);
        // bindings are restored: the next match starts clean
        assert!(b.slot(0).is_none() && b.get(a).is_none());
        assert!(matches(&f, &rule, &s, u, &[a]).is_empty());
    }

    #[test]
    fn no_free_var_and_lower() {
        let mut f = fx();
        let int = f.sort(f.int);
        // \all(phi) with \noFreeVar(phi, 0)
        let mut r = rb(&mut f, "all_unused");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let all = r.app(f.all, &[GenPat::Slot(0)], &[], &[pv]).unwrap();
        r.find_term(all);
        r.cond(Cond::NoFreeVar { sv: phi, index: 0 });
        let rule = r.finish().unwrap();
        let s = Sequent::new();
        let (p, three) = (f.c(f.p), f.c(f.three));
        let v0 = f.var(int, 0);
        let v1 = f.var(int, 1);
        // body mentions the bound variable: no match
        let uses = f.pool.mk_inferred(f.le, &[], &[v0, three]).unwrap();
        let t_uses = f.all(int, uses).unwrap();
        assert!(matches(&f, &rule, &s, t_uses, &[phi]).is_empty());
        // body only mentions the next variable out: matches, and lowering gives the new body
        let outer = f.pool.mk_inferred(f.le, &[], &[v1, three]).unwrap();
        let t_outer = f.all(int, outer).unwrap();
        let m = matches(&f, &rule, &s, t_outer, &[phi]);
        assert_eq!(m, vec![vec![outer]]);
        let lowered = f.pool.lower(outer, 1).unwrap();
        let expect = f.pool.mk_inferred(f.le, &[], &[v0, three]).unwrap();
        assert_eq!(lowered, expect);
        // closed body
        let t_closed = f.all(int, p).unwrap();
        assert_eq!(matches(&f, &rule, &s, t_closed, &[phi]), vec![vec![p]]);
        // lowering a term that uses the variable fails, lowering is the inverse of lifting
        assert_eq!(f.pool.lower(uses, 1), Err(LogicError::CannotLower(0)));
        let up = f.pool.lift(expect, 2).unwrap();
        assert_eq!(f.pool.lower(up, 2).unwrap(), expect);
        assert!(f.pool[uses].may_have_free(0) && !f.pool[uses].may_have_free(1));
        assert!(!f.pool[outer].may_have_free(0) && f.pool[outer].may_have_free(1));
        assert!(!f.pool[t_outer].may_have_free(1) && f.pool[t_outer].may_have_free(0));
    }

    #[test]
    fn different_binder_depths() {
        let mut f = fx();
        let int = f.sort(f.int);
        // and(phi, all<S>(phi)): phi at depth 0 and depth 1
        let mut r = rb(&mut f, "depths");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let all = r.app(f.all, &[GenPat::Slot(0)], &[], &[pv]).unwrap();
        let and = r.app(f.and, &[], &[], &[pv, all]).unwrap();
        r.find_term(and);
        let rule = r.finish().unwrap();
        let s = Sequent::new();
        let (p, three) = (f.c(f.p), f.c(f.three));
        let t = {
            let a = f.all(int, p).unwrap();
            f.and(p, a)
        };
        assert_eq!(matches(&f, &rule, &s, t, &[phi]), vec![vec![p]]);
        // a term with a free outer variable must be lifted when it moves under the binder
        let (v0, v1) = (f.var(int, 0), f.var(int, 1));
        let q = f.pool.mk_inferred(f.le, &[], &[v0, three]).unwrap();
        let q_lifted = f.pool.mk_inferred(f.le, &[], &[v1, three]).unwrap();
        let (aq, aql) = (f.all(int, q_lifted).unwrap(), f.all(int, q).unwrap());
        let good = f.and(q, aq);
        let bad = f.and(q, aql);
        assert_eq!(matches(&f, &rule, &s, good, &[phi]), vec![vec![q]]);
        assert!(matches(&f, &rule, &s, bad, &[phi]).is_empty());
    }

    #[test]
    fn assumes_backtracking_and_sequent_find() {
        let mut f = fx();
        let (z, three, neg) = (f.c(f.zero), f.c(f.three), f.c(f.one_int));
        // \assumes(le(a, b) ==>) \find(a): a is a bare term variable (sort int or below)
        let int = f.sort(f.int);
        let mut r = rb(&mut f, "apply_le");
        let a = r.declare_sv(f.names.intern("a"), SvKind::Term, SortReq::Sub(int));
        let b = r.declare_sv(f.names.intern("b"), SvKind::Term, SortReq::Any);
        let (pa, pb) = (r.sv(a).unwrap(), r.sv(b).unwrap());
        let le = r.app(f.le, &[], &[], &[pa, pb]).unwrap();
        r.assume(Side::Antecedent, le);
        r.find_term(pa);
        let rule = r.finish().unwrap();
        let l1 = f.pool.mk_inferred(f.le, &[], &[three, z]).unwrap();
        let l2 = f.pool.mk_inferred(f.le, &[], &[three, neg]).unwrap();
        let l3 = f.pool.mk_inferred(f.le, &[], &[z, neg]).unwrap();
        let seq = Sequent::new()
            .add_all(&f.pool, Side::Antecedent, &[l1, l2, l3])
            .unwrap()
            .sequent;
        // at `three`: two assumes candidates, enumerated in sequent order
        assert_eq!(
            matches(&f, &rule, &seq, three, &[a, b]),
            vec![vec![three, z], vec![three, neg]]
        );
        // at `zero`: only l3
        assert_eq!(matches(&f, &rule, &seq, z, &[a, b]), vec![vec![z, neg]]);
        // no assumes candidate for `negone`
        assert!(matches(&f, &rule, &seq, neg, &[a, b]).is_empty());
        // `Break` stops the search
        let mut bnd = Bindings::new(&rule);
        let mut n = 0;
        let r = match_term(&f.pool, &rule, &seq, three, &mut bnd, &mut |_| {
            n += 1;
            ControlFlow::Break(())
        });
        assert!(r.is_break() && n == 1);

        // one formula may match two assumes patterns: and(x, y) on the antecedent twice
        let mut r = rb(&mut f, "twice");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Formula, SortReq::Any);
        let y = r.declare_sv(f.names.intern("y"), SvKind::Formula, SortReq::Any);
        let (px, py) = (r.sv(x).unwrap(), r.sv(y).unwrap());
        let both = r.app(f.and, &[], &[], &[px, py]).unwrap();
        r.find_sequent(Some(both), None);
        r.assume(Side::Antecedent, both);
        let rule = r.finish().unwrap();
        let p = f.c(f.p);
        let pp = f.and(p, p);
        let seq = Sequent::new()
            .add(&f.pool, Side::Antecedent, pp)
            .unwrap()
            .sequent;
        let mut bnd = Bindings::new(&rule);
        let mut n = 0;
        let _ = match_sequent(&f.pool, &rule, &seq, &mut bnd, &mut |_| {
            n += 1;
            ControlFlow::Continue(())
        });
        // the repeated schema variables x, y must agree: and(p, p) matches itself
        assert_eq!(n, 1);
    }

    #[test]
    fn index_candidates() {
        let mut f = fx();
        let int = f.sort(f.int);
        let mut rules = Vec::new();
        // 0: all<S>(phi)
        let mut r = rb(&mut f, "all_any");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let all = r.app(f.all, &[GenPat::Slot(0)], &[], &[pv]).unwrap();
        r.find_term(all);
        rules.push(r.finish().unwrap());
        // 1: all<S>(and(x, y))
        let mut r = rb(&mut f, "all_and");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Formula, SortReq::Any);
        let y = r.declare_sv(f.names.intern("y"), SvKind::Formula, SortReq::Any);
        let (px, py) = (r.sv(x).unwrap(), r.sv(y).unwrap());
        let and = r.app(f.and, &[], &[], &[px, py]).unwrap();
        let all = r.app(f.all, &[GenPat::Slot(0)], &[], &[and]).unwrap();
        r.find_term(all);
        rules.push(r.finish().unwrap());
        // 2: any formula
        let mut r = rb(&mut f, "any_formula");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        r.find_term(pv);
        rules.push(r.finish().unwrap());
        // 3: any term of sort int or below
        let mut r = rb(&mut f, "any_int");
        let t = r.declare_sv(f.names.intern("t"), SvKind::Term, SortReq::Sub(int));
        let pv = r.sv(t).unwrap();
        r.find_term(pv);
        rules.push(r.finish().unwrap());
        // 4: le(x, y)
        let mut r = rb(&mut f, "le");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Term, SortReq::Any);
        let y = r.declare_sv(f.names.intern("y"), SvKind::Term, SortReq::Any);
        let (px, py) = (r.sv(x).unwrap(), r.sv(y).unwrap());
        let le = r.app(f.le, &[], &[], &[px, py]).unwrap();
        r.find_term(le);
        rules.push(r.finish().unwrap());

        let ix = RuleIndex::build(&f.pool, &rules);
        let (p, three) = (f.c(f.p), f.c(f.three));
        let pp = f.and(p, p);
        let le_t = f.pool.mk_inferred(f.le, &[], &[three, three]).unwrap();
        let all_and = f.all(int, pp).unwrap();
        let all_le = f.all(int, le_t).unwrap();
        let mut out = Vec::new();
        let mut cand = |t: TermIdx| {
            ix.candidates(&f.pool, t, &mut out);
            let mut v = out.clone();
            v.sort();
            v
        };
        assert_eq!(cand(all_and), vec![0, 1, 2]);
        assert_eq!(cand(all_le), vec![0, 2]);
        assert_eq!(cand(le_t), vec![2, 4]);
        assert_eq!(cand(three), vec![3]);
        assert_eq!(cand(p), vec![2]);
        assert_eq!(ix.num_wild(), 2);
    }
}

mod proof_tree {
    use super::*;

    fn app(rule: u32) -> Application {
        Application {
            rule,
            find: None,
            assumes: Box::new([]),
            inst: Instantiation {
                svs: Box::new([]),
                slots: Box::new([]),
            },
        }
    }

    fn seq(f: &mut Fx, ante: &[TermIdx]) -> Sequent {
        Sequent::new()
            .add_all(&f.pool, Side::Antecedent, ante)
            .unwrap()
            .sequent
    }

    #[test]
    fn grows_branches_and_closes() {
        let mut f = fx();
        let p = f.c(f.p);
        let pp = f.and(p, p);
        let root = seq(&mut f, &[pp]);
        let left = seq(&mut f, &[p]);
        let mut t = ProofTree::new(root.clone());
        let r = t.root();
        assert_eq!(t.open_goals(), &[r]);
        assert!(!t.is_closed(r) && t.is_open_goal(r) && t.step_of(r).is_none());
        assert!(t.children(r).is_empty());

        // a rule with two goal templates branches
        let name = f.names.intern("Use case");
        let step = t
            .apply(
                r,
                app(7),
                vec![
                    Branch {
                        label: Some(name),
                        sequent: left.clone(),
                    },
                    Branch::new(Sequent::new()),
                ],
            )
            .unwrap();
        assert!(!t.is_open_goal(r) && t.step(step).node() == r);
        assert_eq!(t.step(step).application().rule, 7);
        let kids: Vec<_> = t.children(r).into_iter().collect();
        assert_eq!(kids.len(), 2);
        let (a, b) = (kids[0], kids[1]);
        assert_eq!(t.parent(a), Some(r));
        assert_eq!(t.node(a).depth(), 1);
        assert_eq!(t.node(a).created_by(), Some((step, 0)));
        assert_eq!(t.node(b).created_by(), Some((step, 1)));
        assert_eq!(t.node(a).label(), Some(name));
        assert_eq!(t.sequent(a), &left);
        assert_eq!(t.sequent(r), &root); // the parent stays as it was
        let mut goals = t.open_goals().to_vec();
        goals.sort();
        assert_eq!(goals, vec![a, b]);

        // a rule with one goal template does not branch
        let c = {
            let s = t
                .apply(a, app(1), vec![Branch::new(Sequent::new())])
                .unwrap();
            t.step(s).children().first().unwrap()
        };
        assert_eq!(t.node(c).depth(), 2);
        assert_eq!(t.ancestors(c).collect::<Vec<_>>(), vec![c, a, r]);
        assert!(!t.is_closed(a) && !t.is_closed(c));

        // closing the grandchild closes its parent, but not the root: b is still open
        let s = t.apply(c, app(2), vec![]).unwrap();
        assert!(t.step(s).closes());
        assert!(t.is_closed(c) && t.is_closed(a) && !t.is_closed(r));
        assert!(t.is_closed(c) && !t.is_open_goal(c));
        assert_eq!(t.open_goals(), &[b]);
        assert!(!t.is_proof_closed());
        t.apply(b, app(2), vec![]).unwrap();
        assert!(t.is_closed(b) && t.is_proof_closed());
        assert!(t.open_goals().is_empty());
        assert_eq!(t.num_nodes(), 4);
        assert_eq!(t.num_steps(), 4);
    }

    #[test]
    fn errors_and_trivial_proofs() {
        let mut f = fx();
        let mut t = ProofTree::new(Sequent::new());
        let r = t.root();
        // closing the root closes the proof
        t.apply(r, app(0), vec![]).unwrap();
        assert!(t.is_proof_closed());
        assert_eq!(
            t.apply(r, app(0), vec![]),
            Err(ProofError::NotAnOpenGoal(r))
        );
        let bogus = NodeId::from_u32(5);
        assert_eq!(
            t.apply(bogus, app(0), vec![]),
            Err(ProofError::UnknownNode(bogus))
        );
        // goals are swap-removed: the bookkeeping stays consistent
        let p = f.c(f.p);
        let s = seq(&mut f, &[p]);
        let mut t = ProofTree::new(s.clone());
        let step = t
            .apply(
                t.root(),
                app(1),
                (0..4).map(|_| Branch::new(s.clone())).collect(),
            )
            .unwrap();
        let kids: Vec<_> = t.step(step).children().into_iter().collect();
        t.apply(kids[1], app(2), vec![]).unwrap();
        t.apply(kids[0], app(2), vec![]).unwrap();
        let mut left = t.open_goals().to_vec();
        left.sort();
        assert_eq!(left, vec![kids[2], kids[3]]);
        assert!(left.iter().all(|&g| t.is_open_goal(g)));
        assert!(!t.is_closed(t.root()));
        t.apply(kids[3], app(2), vec![]).unwrap();
        t.apply(kids[2], app(2), vec![]).unwrap();
        assert!(t.is_proof_closed());
    }
}
