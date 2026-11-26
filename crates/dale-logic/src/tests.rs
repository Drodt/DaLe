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
