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
             extends: Option<SortPat>| {
        let mut d = SortDecl::new(n.intern(name));
        d.kind = kind;
        d.params = params;
        d.extends = extends;
        b.add_sort(d).unwrap()
    };
    let any = s(&mut b, &mut n, "Any", SortKind::Top, vec![], None);
    let form = s(&mut b, &mut n, "Formula", SortKind::Formula, vec![], None);
    let int = s(&mut b, &mut n, "int", SortKind::Normal, vec![], None);
    let nat = s(
        &mut b,
        &mut n,
        "nat",
        SortKind::Normal,
        vec![],
        Some(SortPat::simple(int)),
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
        None,
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
        None,
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
        args: vec![0],
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
    f.pool.rollback(cp).unwrap();
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
    d.extends = Some(SortPat::simple(SortDeclId::from_u32(7)));
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
        args: vec![0],
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

    /// Rules with sequent finds, for the index and [`match_sequent_at`]: `0: and(x, y) ==>`,
    /// `1: ==> and(x, y)`, `2: \assumes(phi ==>) ==> phi`, `3: and(x, y) ==> phi`, and the term
    /// find `4: and(x, y)`.
    fn sequent_rules(f: &mut Fx) -> Vec<Rule> {
        let mut rules = Vec::new();
        for i in 0..5 {
            let mut r = rb(f, &format!("r{i}"));
            let x = r.declare_sv(f.names.intern("x"), SvKind::Formula, SortReq::Any);
            let y = r.declare_sv(f.names.intern("y"), SvKind::Formula, SortReq::Any);
            let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
            let (px, py, pphi) = (r.sv(x).unwrap(), r.sv(y).unwrap(), r.sv(phi).unwrap());
            let and = r.app(f.and, &[], &[], &[px, py]).unwrap();
            match i {
                0 => r.find_sequent(Some(and), None),
                1 => r.find_sequent(None, Some(and)),
                2 => {
                    r.find_sequent(None, Some(pphi));
                    r.assume(Side::Antecedent, pphi);
                }
                3 => r.find_sequent(Some(and), Some(pphi)),
                _ => r.find_term(and),
            }
            rules.push(r.finish().unwrap());
        }
        rules
    }

    #[test]
    fn index_sequent_candidates() {
        let mut f = fx();
        let rules = sequent_rules(&mut f);
        let ix = RuleIndex::build(&f.pool, &rules);
        let p = f.c(f.p);
        let pp = f.and(p, p);
        let mut out = Vec::new();
        let mut cand = |side: Option<Side>, t: TermIdx| {
            match side {
                Some(side) => ix.sequent_candidates(&f.pool, side, t, &mut out),
                None => ix.candidates(&f.pool, t, &mut out),
            }
            let mut v = out.clone();
            v.sort();
            v
        };
        assert_eq!(cand(Some(Side::Antecedent), pp), vec![0, 3]);
        assert_eq!(cand(Some(Side::Antecedent), p), Vec::<RuleId>::new());
        assert_eq!(cand(Some(Side::Succedent), pp), vec![1, 2, 3]);
        assert_eq!(cand(Some(Side::Succedent), p), vec![2, 3]);
        // term finds and sequent finds do not mix
        assert_eq!(cand(None, pp), vec![4]);
        assert_eq!(ix.num_wild(), 0);
        assert_eq!(ix.num_wild_sequent(Side::Antecedent), 0);
        assert_eq!(ix.num_wild_sequent(Side::Succedent), 2);
    }

    #[test]
    fn match_sequent_at_one_formula() {
        let mut f = fx();
        let rules = sequent_rules(&mut f);
        let p = f.c(f.p);
        let pp = f.and(p, p);
        // pp, p ==> p, pp
        let seq = Sequent::new()
            .add_all(&f.pool, Side::Antecedent, &[pp, p])
            .unwrap()
            .sequent
            .add_all(&f.pool, Side::Succedent, &[p, pp])
            .unwrap()
            .sequent;
        let at = |rule: &Rule, focus: Option<(Side, usize)>| {
            let mut b = Bindings::new(rule);
            let mut found = Vec::new();
            let mut k = |m: &Bindings| {
                found.push(m.matched().to_vec());
                ControlFlow::Continue(())
            };
            let _ = match focus {
                Some((side, i)) => match_sequent_at(&f.pool, rule, &seq, side, i, &mut b, &mut k),
                None => match_sequent(&f.pool, rule, &seq, &mut b, &mut k),
            };
            found
        };
        let (a, s) = (Side::Antecedent, Side::Succedent);
        assert_eq!(at(&rules[0], Some((a, 0))), vec![vec![(a, 0)]]);
        assert!(at(&rules[0], Some((a, 1))).is_empty());
        // no find part on the focused side, or no such formula
        assert!(at(&rules[0], Some((s, 1))).is_empty());
        assert!(at(&rules[0], Some((a, 5))).is_empty());
        assert_eq!(at(&rules[1], Some((s, 1))), vec![vec![(s, 1)]]);
        // the assumes part still ranges over its side
        assert_eq!(at(&rules[2], Some((s, 0))), vec![vec![(s, 0), (a, 1)]]);
        assert_eq!(at(&rules[2], Some((s, 1))), vec![vec![(s, 1), (a, 0)]]);
        // a find with two parts: the other part ranges over its side, antecedent first
        assert_eq!(
            at(&rules[3], Some((a, 0))),
            vec![vec![(a, 0), (s, 0)], vec![(a, 0), (s, 1)]]
        );
        assert_eq!(at(&rules[3], Some((s, 1))), vec![vec![(a, 0), (s, 1)]]);
        assert_eq!(at(&rules[3], None).len(), 2);
        // a term find is not a sequent find
        assert!(at(&rules[4], Some((a, 0))).is_empty());
    }
}

mod proof_tree {
    use super::*;

    fn app(rule: u32) -> Application {
        Application {
            rule,
            find: Box::new([]),
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
                ViewId::ZERO,
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
                .apply(a, app(1), ViewId::ZERO, vec![Branch::new(Sequent::new())])
                .unwrap();
            t.step(s).children().first().unwrap()
        };
        assert_eq!(t.node(c).depth(), 2);
        assert_eq!(t.ancestors(c).collect::<Vec<_>>(), vec![c, a, r]);
        assert!(!t.is_closed(a) && !t.is_closed(c));

        // closing the grandchild closes its parent, but not the root: b is still open
        let s = t.apply(c, app(2), ViewId::ZERO, vec![]).unwrap();
        assert!(t.step(s).closes());
        assert!(t.is_closed(c) && t.is_closed(a) && !t.is_closed(r));
        assert!(t.is_closed(c) && !t.is_open_goal(c));
        assert_eq!(t.open_goals(), &[b]);
        assert!(!t.is_proof_closed());
        t.apply(b, app(2), ViewId::ZERO, vec![]).unwrap();
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
        t.apply(r, app(0), ViewId::ZERO, vec![]).unwrap();
        assert!(t.is_proof_closed());
        assert_eq!(
            t.apply(r, app(0), ViewId::ZERO, vec![]),
            Err(ProofError::NotAnOpenGoal(r))
        );
        let bogus = NodeId::from_u32(5);
        assert_eq!(
            t.apply(bogus, app(0), ViewId::ZERO, vec![]),
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
                ViewId::ZERO,
                (0..4).map(|_| Branch::new(s.clone())).collect(),
            )
            .unwrap();
        let kids: Vec<_> = t.step(step).children().into_iter().collect();
        t.apply(kids[1], app(2), ViewId::ZERO, vec![]).unwrap();
        t.apply(kids[0], app(2), ViewId::ZERO, vec![]).unwrap();
        let mut left = t.open_goals().to_vec();
        left.sort();
        assert_eq!(left, vec![kids[2], kids[3]]);
        assert!(left.iter().all(|&g| t.is_open_goal(g)));
        assert!(!t.is_closed(t.root()));
        t.apply(kids[3], app(2), ViewId::ZERO, vec![]).unwrap();
        t.apply(kids[2], app(2), ViewId::ZERO, vec![]).unwrap();
        assert!(t.is_proof_closed());
    }
}

mod threads {
    use std::ops::ControlFlow;

    use super::*;

    fn send_sync<T: Send + Sync>() {}

    /// What parallel proof search can already rely on: these are shared between threads.
    #[test]
    fn shared_types_are_send_and_sync() {
        send_sync::<TermPool>();
        send_sync::<Sequent>();
        send_sync::<Rule>();
        send_sync::<RuleIndex>();
        send_sync::<ProofTree>();
        send_sync::<Application>();
    }

    /// Matching only reads the pool, so threads can match against one pool at the same time.
    #[test]
    fn concurrent_matching_on_a_shared_pool() {
        let mut f = fx();
        let mut r = RuleBuilder::new(f.pool.signature().clone(), f.names.intern("eq_and"));
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let and = r.app(f.and, &[], &[], &[pv, pv]).unwrap();
        r.find_term(and);
        let rule = r.finish().unwrap();
        let p = f.c(f.p);
        let pp = f.and(p, p);
        let terms: Vec<_> = (0..64).map(|_| f.and(pp, pp)).collect();
        let seq = Sequent::new();
        let (pool, rule, seq) = (&f.pool, &rule, &seq);
        std::thread::scope(|s| {
            let hs: Vec<_> = (0..4)
                .map(|_| {
                    s.spawn(|| {
                        let mut b = Bindings::new(rule);
                        let mut n = 0;
                        for &t in &terms {
                            let _ = match_term(pool, rule, seq, t, &mut b, &mut |_| {
                                n += 1;
                                ControlFlow::Continue(())
                            });
                        }
                        n
                    })
                })
                .collect();
            for h in hs {
                assert_eq!(h.join().unwrap(), 64);
            }
        });
    }
}

mod forking {
    use super::*;

    /// A chain of distinct int terms `ifThenElse(p, prev, three)` (arity 3, so the argument
    /// storage crosses chunk boundaries); returns the last one.
    fn chain(f: &mut Fx, n: usize) -> Vec<TermIdx> {
        let z = f.c(f.zero);
        chain_from(f, z, n)
    }

    fn chain_from(f: &mut Fx, start: TermIdx, n: usize) -> Vec<TermIdx> {
        let (p, three) = (f.c(f.p), f.c(f.three));
        let mut t = start;
        let mut v = Vec::new();
        for _ in 0..n {
            t = f.pool.mk_inferred(f.ite, &[], &[p, t, three]).unwrap();
            v.push(t);
        }
        v
    }

    fn check_chain(pool: &TermPool, v: &[TermIdx]) {
        for w in v.windows(2) {
            let subs = pool.subs_of(w[1]);
            assert_eq!(subs.len(), 3);
            assert_eq!(subs[1], w[0]);
        }
    }

    #[test]
    fn fork_shares_the_past_and_diverges() {
        let mut f = fx();
        let old = chain(&mut f, 10);
        let mut other = f.pool.fork();
        // everything created before the fork reads the same in both pools
        check_chain(&f.pool, &old);
        check_chain(&other, &old);
        for &t in &old {
            assert_eq!(f.pool.subs_of(t), other.subs_of(t));
        }
        // building an old term again finds it, in both
        let (p, three) = (f.c(f.p), f.c(f.three));
        let again = other.mk_inferred(f.ite, &[], &[p, old[2], three]).unwrap();
        assert_eq!(again, old[3]);
        let again = f.pool.mk_inferred(f.ite, &[], &[p, old[2], three]).unwrap();
        assert_eq!(again, old[3]);
        // new terms are independent: the same index denotes different terms
        let mine = f.and(p, p);
        let (theirs, q) = {
            let q = other.mk_inferred(f.le, &[], &[three, three]).unwrap();
            (other.mk_inferred(f.and, &[], &[q, q]).unwrap(), q)
        };
        assert_ne!(f.pool.subs_of(mine), other.subs_of(theirs));
        assert_eq!(other.subs_of(theirs), &[q, q]);
        // and each is canonical in its own pool
        assert_eq!(f.and(p, p), mine);
        let q2 = other.mk_inferred(f.le, &[], &[three, three]).unwrap();
        assert_eq!(other.mk_inferred(f.and, &[], &[q2, q2]).unwrap(), theirs);
        // a term of the other pool is not visible here
        assert!(other.num_terms() != f.pool.num_terms() || mine != theirs);
    }

    #[test]
    fn many_forks_keep_the_layers_few() {
        let mut f = fx();
        let mut all = chain(&mut f, 5);
        let mut pools = Vec::new();
        for round in 0..200 {
            // a few new terms, then a fork; the fork is kept so that it shares the layers
            let start = *all.last().unwrap();
            let more = chain_from(&mut f, start, 6 + round % 3);
            all.extend(more);
            pools.push(f.pool.fork());
        }
        assert!(
            f.pool.term_table_layers() <= 12,
            "{} layers",
            f.pool.term_table_layers()
        );
        // everything is still found, with the same index, in the donor and in the last fork
        let (p, three) = (f.c(f.p), f.c(f.three));
        let last = pools.last().unwrap();
        let mut ids = std::collections::HashSet::new();
        for w in all.windows(2) {
            if f.pool.subs_of(w[1]).len() == 3 && f.pool.subs_of(w[1])[1] == w[0] {
                let t = f.pool.mk_inferred(f.ite, &[], &[p, w[0], three]).unwrap();
                assert_eq!(t, w[1]);
                assert_eq!(last.subs_of(t)[1], w[0]);
                ids.insert(t);
            }
        }
        assert!(ids.len() > 1000);
    }

    #[test]
    fn crossing_chunk_boundaries() {
        let mut f = fx();
        let a = chain(&mut f, 3000);
        let mut other = f.pool.fork();
        let b = chain(&mut f, 3000); // re-finds the first 3000, then... the same chain
        assert_eq!(a, b);
        check_chain(&f.pool, &a);
        check_chain(&other, &a);
        // extend in the fork past several chunk boundaries
        let (p, three) = (f.c(f.p), f.c(f.three));
        let mut t = *a.last().unwrap();
        let mut v = a.clone();
        for _ in 0..5000 {
            t = other.mk_inferred(f.ite, &[], &[p, t, three]).unwrap();
            v.push(t);
        }
        check_chain(&other, &v);
        // freeze and thaw
        let frozen = other.freeze();
        let mut thawed = frozen.thaw();
        check_chain(&frozen, &v);
        check_chain(&thawed, &v);
        let more = thawed.mk_inferred(f.ite, &[], &[p, t, three]).unwrap();
        assert_eq!(thawed.subs_of(more)[1], t);
        assert_eq!(frozen.num_terms() + 1, thawed.num_terms());
    }

    #[test]
    fn rollback_stops_at_the_fork() {
        let mut f = fx();
        let p = f.c(f.p);
        let cp = f.pool.checkpoint();
        let _a = f.and(p, p);
        // rolling back within the private part is fine
        f.pool.rollback(cp).unwrap();
        let cp = f.pool.checkpoint();
        let _a = f.and(p, p);
        let _other = f.pool.fork();
        assert_eq!(f.pool.rollback(cp), Err(LogicError::RollbackPastFork));
        // a checkpoint taken after the fork works again
        let cp = f.pool.checkpoint();
        let n = f.pool.num_terms();
        let c3 = f.c(f.three);
        let q = f.pool.mk_inferred(f.le, &[], &[c3, c3]).unwrap();
        let _ = f.and(q, q);
        f.pool.rollback(cp).unwrap();
        assert_eq!(f.pool.num_terms(), n);
    }

    #[test]
    fn threads_extend_a_frozen_pool() {
        let mut f = fx();
        let base = chain(&mut f, 100);
        let sig = f.pool.signature().clone();
        let frozen = std::mem::replace(&mut f.pool, TermPool::new(sig)).freeze();
        let (and, le, p0, three0) = (f.and, f.le, f.p, f.three);
        let base = &base;
        let frozen = &frozen;
        let results: Vec<(TermIdx, usize)> = std::thread::scope(|s| {
            let hs: Vec<_> = (0..4usize)
                .map(|k| {
                    s.spawn(move || {
                        let mut pool = frozen.thaw();
                        let p = pool.mk_inferred(p0, &[], &[]).unwrap();
                        let three = pool.mk_inferred(three0, &[], &[]).unwrap();
                        // worker k builds k+1 different formulas on top of the shared chain
                        let mut last = p;
                        for j in 0..=k {
                            let q = pool.mk_inferred(le, &[], &[three, three]).unwrap();
                            let top = base[base.len() - 1 - j];
                            let r = pool.mk_inferred(le, &[], &[top, three]).unwrap();
                            last = pool.mk_inferred(and, &[], &[q, r]).unwrap();
                        }
                        // the shared past is intact and canonical
                        assert_eq!(pool.subs_of(*base.last().unwrap()).len(), 3);
                        (last, pool.num_terms())
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        // workers started at the same point, so their first new term has the same index even though
        // the terms differ, and the frozen pool did not change
        let n = frozen.num_terms();
        for (k, (_, count)) in results.iter().enumerate() {
            assert!(*count > n, "worker {k} added terms");
        }
        assert_eq!(frozen.num_terms(), n);
        let _ = results;
    }
}

mod fragments {
    use super::*;

    fn app(rule: u32) -> Application {
        Application {
            rule,
            find: Box::new([]),
            assumes: Box::new([]),
            inst: Instantiation {
                svs: Box::new([]),
                slots: Box::new([]),
            },
        }
    }

    /// What a worker does with its goal: `k + 2` rule applications that each add a new formula
    /// `and(prev, prev)` (new in the worker's own pool), then a closing one.
    fn work(
        mut pool: TermPool,
        goal: Sequent,
        base: TermIdx,
        and: OpDeclId,
        k: usize,
    ) -> (ProofTree, TermIdx) {
        let mut tree = ProofTree::new(goal.clone());
        let (mut cur, mut seq, mut prev) = (tree.root(), goal, base);
        let mut first = None;
        for _ in 0..k + 2 {
            prev = pool.mk_inferred(and, &[], &[prev, prev]).unwrap();
            first.get_or_insert(prev);
            seq = seq.add(&pool, Side::Antecedent, prev).unwrap().sequent;
            let step = tree
                .apply(cur, app(1), ViewId::ZERO, vec![Branch::new(seq.clone())])
                .unwrap();
            cur = tree.step(step).children().first().unwrap();
        }
        tree.apply(cur, app(2), ViewId::ZERO, vec![]).unwrap();
        tree.set_view(ViewId::ZERO, pool.freeze()).unwrap();
        (tree, first.unwrap())
    }

    /// The whole scenario; `reverse` starts the workers in the opposite order.
    fn scenario(reverse: bool) -> (Fx, ProofTree, Vec<TermIdx>) {
        let mut f = fx();
        let p = f.c(f.p);
        let mut base = vec![p];
        for _ in 0..4 {
            let last = *base.last().unwrap();
            base.push(f.and(last, p));
        }
        let root = Sequent::new();
        let mut tree = ProofTree::new(root.clone());
        tree.set_view(ViewId::ZERO, f.pool.fork().freeze()).unwrap();
        let branches = base[..4]
            .iter()
            .map(|&b| Branch::new(root.add(&f.pool, Side::Succedent, b).unwrap().sequent))
            .collect();
        let step = tree
            .apply(tree.root(), app(0), ViewId::ZERO, branches)
            .unwrap();
        let goals: Vec<_> = tree.step(step).children().into_iter().collect();
        // every worker gets its own fork, taken before any of them runs
        let jobs: Vec<_> = goals
            .iter()
            .enumerate()
            .map(|(k, &g)| (k, f.pool.fork(), tree.sequent(g).clone()))
            .collect();
        let and = f.and;
        let mut results: Vec<Option<(ProofTree, TermIdx)>> = (0..4).map(|_| None).collect();
        std::thread::scope(|s| {
            let mut jobs = jobs;
            if reverse {
                jobs.reverse();
            }
            let hs: Vec<_> = jobs
                .into_iter()
                .map(|(k, pool, seq)| {
                    let b = base[k + 1];
                    (k, s.spawn(move || work(pool, seq, b, and, k)))
                })
                .collect();
            for (k, h) in hs {
                results[k] = Some(h.join().unwrap());
            }
        });
        let mut firsts = Vec::new();
        for (k, r) in results.into_iter().enumerate() {
            let (frag, first) = r.unwrap();
            firsts.push(first);
            tree.graft(goals[k], frag).unwrap();
        }
        (f, tree, firsts)
    }

    fn shape(t: &ProofTree) -> Vec<(Option<NodeId>, u32, Option<u32>)> {
        (0..t.num_nodes() as u32)
            .map(NodeId::from_u32)
            .map(|n| {
                (
                    t.parent(n),
                    t.node(n).depth(),
                    t.step_of(n).map(|s| s.application().rule),
                )
            })
            .collect()
    }

    #[test]
    fn workers_with_forked_pools_graft_into_one_proof() {
        let (f, tree, firsts) = scenario(false);
        assert!(tree.is_proof_closed() && tree.open_goals().is_empty());
        // root + 4 goals + (2 + 3 + 4 + 5) nodes below them; 1 + (3 + 4 + 5 + 6) steps
        assert_eq!(tree.num_nodes(), 19);
        assert_eq!(tree.num_steps(), 19);
        // the workers' first new terms have the same index but are different terms, each in its
        // own pool snapshot
        assert!(firsts.windows(2).all(|w| w[0] == w[1]));
        let mut shown = Vec::new();
        for n in (0..tree.num_nodes() as u32).map(NodeId::from_u32) {
            let Some(pool) = tree.view_of(n) else {
                panic!("no view for {n:?}")
            };
            for (_, t) in tree.sequent(n).iter() {
                assert!(pool.is_formula(t));
            }
            if tree.node(n).depth() == 2 {
                let a = tree.sequent(n).antecedent();
                if let Some(&t) = a.as_slice().first()
                    && t == firsts[0]
                {
                    shown.push(pool.show_term(&f.names, t).to_string());
                }
            }
        }
        shown.sort();
        shown.dedup();
        assert_eq!(shown.len(), 4, "{shown:?}");
        // depth bookkeeping: a node of fragment k at step j is at depth 2 + j
        let deepest = (0..tree.num_nodes() as u32)
            .map(|n| tree.node(NodeId::from_u32(n)).depth())
            .max()
            .unwrap();
        assert_eq!(deepest, 1 + 5);
    }

    #[test]
    fn grafting_is_independent_of_scheduling() {
        let (_, a, _) = scenario(false);
        let (_, b, _) = scenario(true);
        assert_eq!(shape(&a), shape(&b));
        assert_eq!(a.num_steps(), b.num_steps());
    }

    #[test]
    fn graft_errors_and_open_fragments() {
        let mut f = fx();
        let p = f.c(f.p);
        let s = Sequent::new()
            .add(&f.pool, Side::Antecedent, p)
            .unwrap()
            .sequent;
        let mut tree = ProofTree::new(s.clone());
        let r = tree.root();
        // a fragment whose view was never set
        let mut frag = ProofTree::new(s.clone());
        frag.apply(
            frag.root(),
            app(1),
            ViewId::ZERO,
            vec![Branch::new(s.clone())],
        )
        .unwrap();
        let fcopy_root = frag.root();
        assert_eq!(
            tree.graft(r, ProofTree::new(Sequent::new())),
            Err(ProofError::GraftMismatch(r))
        );
        let _ = fcopy_root;
        assert_eq!(
            tree.graft(r, frag),
            Err(ProofError::ViewMissing(ViewId::ZERO))
        );
        // a fragment that did nothing leaves the goal open
        let mut empty = ProofTree::new(s.clone());
        empty
            .set_view(ViewId::ZERO, f.pool.fork().freeze())
            .unwrap();
        tree.graft(r, empty).unwrap();
        assert!(tree.is_open_goal(r) && tree.num_nodes() == 1);
        // a fragment with open goals: they become goals of the tree, and grafting twice fails
        let mut frag = ProofTree::new(s.clone());
        frag.apply(
            frag.root(),
            app(1),
            ViewId::ZERO,
            vec![Branch::new(s.clone()), Branch::new(s.clone())],
        )
        .unwrap();
        frag.set_view(ViewId::ZERO, f.pool.fork().freeze()).unwrap();
        let again = ProofTree::new(s.clone());
        tree.graft(r, frag).unwrap();
        assert_eq!(tree.open_goals().len(), 2);
        assert!(!tree.is_open_goal(r) && tree.step_of(r).is_some());
        assert_eq!(tree.graft(r, again), Err(ProofError::NotAnOpenGoal(r)));
        // views: setting twice fails
        tree.set_view(ViewId::ZERO, f.pool.fork().freeze()).unwrap();
        assert_eq!(
            tree.set_view(ViewId::ZERO, f.pool.fork().freeze()),
            Err(ProofError::ViewAlreadySet(ViewId::ZERO))
        );
        assert_eq!(
            tree.set_view(ViewId::from_u32(99), f.pool.fork().freeze()),
            Err(ProofError::UnknownView(ViewId::from_u32(99)))
        );
    }
}

mod applying {
    use std::{ops::ControlFlow, slice};

    use super::*;

    fn rb(f: &mut Fx, name: &str) -> RuleBuilder {
        RuleBuilder::new(f.pool.signature().clone(), f.names.intern(name))
    }

    /// The first complete match of a term-find rule at `pos` (a position in `seq`).
    fn first_match(f: &Fx, rule: &Rule, seq: &Sequent, pos: &PosInOccurrence) -> Option<Bindings> {
        let t = pos.sub_term(&f.pool, seq).unwrap();
        let mut b = Bindings::new(rule);
        let mut found = None;
        let _ = match_term(&f.pool, rule, seq, t, &mut b, &mut |m| {
            found = Some(m.clone());
            ControlFlow::Break(())
        });
        found
    }

    fn pos(side: Side, index: usize, path: &[u16]) -> PosInOccurrence {
        PosInOccurrence::new(side, index, PosInTerm::from_path(path))
    }

    fn seq1(f: &Fx, side: Side, fs: &[TermIdx]) -> Sequent {
        Sequent::new().add_all(&f.pool, side, fs).unwrap().sequent
    }

    #[test]
    fn rewrite_with_a_repeated_variable() {
        let mut f = fx();
        // \find(phi & phi) \replacewith(phi)
        let mut r = rb(&mut f, "eq_and");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let and = r.app(f.and, &[], &[], &[pv, pv]).unwrap();
        r.find_term(and);
        let tv = r.tmpl_sv(phi).unwrap();
        r.goal(GoalTemplate::new(Replace::Term(tv)));
        let rule = r.finish().unwrap();
        let (p, three, zero) = (f.c(f.p), f.c(f.three), f.c(f.zero));
        let q = f.pool.mk_inferred(f.le, &[], &[three, zero]).unwrap();
        let (qq, qp) = (f.and(q, q), f.and(q, p));
        let top = f.and(qq, p);
        let seq = seq1(&f, Side::Succedent, &[top]);
        let at = pos(Side::Succedent, 0, &[0]);
        let b = first_match(&f, &rule, &seq, &at).expect("matches");
        let applied = apply_rule(
            &mut f.pool,
            &rule,
            3,
            &seq,
            Some(&at),
            &b,
            SortCheck::Enforce,
        )
        .unwrap();
        assert_eq!(applied.branches.len(), 1);
        let expect = f.and(q, p);
        assert_eq!(
            applied.branches[0].sequent.succedent().as_slice(),
            &[expect]
        );
        // the old sequent is untouched, and the record says what happened
        assert_eq!(seq.succedent().as_slice(), &[top]);
        assert_eq!(applied.app.rule, 3);
        assert_eq!(&*applied.app.find, slice::from_ref(&at));
        assert_eq!(applied.app.inst.svs[0], Some(q));
        assert!(first_match(&f, &rule, &seq, &pos(Side::Succedent, 0, &[])).is_none());
        let _ = qp;
    }

    #[test]
    fn quantifier_removal_shifts_the_body() {
        let mut f = fx();
        let int = f.sort(f.int);
        // \find(\all(phi)) \varcond(\noFreeVar(phi, 0)) \replacewith(phi): phi moves out of one
        // binder, so its remaining free variables are lowered
        let mut r = rb(&mut f, "all_unused");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let all = r.app(f.all, &[GenPat::Slot(0)], &[], &[pv]).unwrap();
        r.find_term(all);
        r.cond(Cond::NoFreeVar { sv: phi, index: 0 });
        let tv = r.tmpl_sv(phi).unwrap();
        r.goal(GoalTemplate::new(Replace::Term(tv)));
        let rule = r.finish().unwrap();
        let (three, v0, v1) = (f.c(f.three), f.var(int, 0), f.var(int, 1));
        let uses_outer = f.pool.mk_inferred(f.le, &[], &[v1, three]).unwrap();
        let inner = f.all(int, uses_outer).unwrap();
        let outer = f.all(int, inner).unwrap(); // closed: v1 is bound by the outer quantifier
        let seq = seq1(&f, Side::Antecedent, &[outer]);
        // at the inner quantifier (position [0]) the rule fires: le(v1, three) becomes le(v0, three)
        let at = pos(Side::Antecedent, 0, &[0]);
        let b = first_match(&f, &rule, &seq, &at).expect("matches");
        let applied = apply_rule(
            &mut f.pool,
            &rule,
            0,
            &seq,
            Some(&at),
            &b,
            SortCheck::Enforce,
        )
        .unwrap();
        let lowered = f.pool.mk_inferred(f.le, &[], &[v0, three]).unwrap();
        let expect = f.all(int, lowered).unwrap();
        assert_eq!(
            applied.branches[0].sequent.antecedent().as_slice(),
            &[expect]
        );
        // the outer quantifier's body uses its variable: no match
        assert!(first_match(&f, &rule, &seq, &pos(Side::Antecedent, 0, &[])).is_none());
        // without the condition the rule would capture: the application reports it
        let mut r = rb(&mut f, "all_bad");
        let phi = r.declare_sv(f.names.intern("phi"), SvKind::Formula, SortReq::Any);
        let pv = r.sv(phi).unwrap();
        let all = r.app(f.all, &[GenPat::Slot(0)], &[], &[pv]).unwrap();
        r.find_term(all);
        let tv = r.tmpl_sv(phi).unwrap();
        r.goal(GoalTemplate::new(Replace::Term(tv)));
        let bad = r.finish().unwrap();
        let at = pos(Side::Antecedent, 0, &[]);
        let b = first_match(&f, &bad, &seq, &at).expect("matches");
        assert!(matches!(
            apply_rule(
                &mut f.pool,
                &bad,
                0,
                &seq,
                Some(&at),
                &b,
                SortCheck::Enforce
            ),
            Err(ApplyError::CapturesBinder(_))
        ));
    }

    #[test]
    fn sequent_rules_branch_and_close() {
        let mut f = fx();
        let (p, three, zero) = (f.c(f.p), f.c(f.three), f.c(f.zero));
        let q = f.pool.mk_inferred(f.le, &[], &[three, zero]).unwrap();
        // andRight: \find(==> a & b) \replacewith(==> a) | \replacewith(==> b)
        let mut r = rb(&mut f, "andRight");
        let a = r.declare_sv(f.names.intern("a"), SvKind::Formula, SortReq::Any);
        let bb = r.declare_sv(f.names.intern("b"), SvKind::Formula, SortReq::Any);
        let (pa, pb) = (r.sv(a).unwrap(), r.sv(bb).unwrap());
        let and = r.app(f.and, &[], &[], &[pa, pb]).unwrap();
        r.find_sequent(None, Some(and));
        let (ta, tb) = (r.tmpl_sv(a).unwrap(), r.tmpl_sv(bb).unwrap());
        let first = f.names.intern("first");
        let mut g1 = GoalTemplate::new(Replace::Sequent {
            ante: vec![],
            succ: vec![ta],
        });
        g1.label = Some(first);
        r.goal(g1);
        r.goal(GoalTemplate::new(Replace::Sequent {
            ante: vec![],
            succ: vec![tb],
        }));
        let and_right = r.finish().unwrap();
        // close: \find(false-ish) : here a rule without goals, found as `p ==> p` style: antecedent
        // formula x and succedent formula x
        let mut r = rb(&mut f, "close");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Formula, SortReq::Any);
        let px = r.sv(x).unwrap();
        r.find_sequent(Some(px), None);
        r.assume(Side::Succedent, px);
        let close = r.finish().unwrap();

        let pq = f.and(p, q);
        let seq = Sequent::new()
            .add(&f.pool, Side::Antecedent, p)
            .unwrap()
            .sequent
            .add(&f.pool, Side::Succedent, pq)
            .unwrap()
            .sequent;
        let mut b = Bindings::new(&and_right);
        let mut m = None;
        let _ = match_sequent(&f.pool, &and_right, &seq, &mut b, &mut |x| {
            m = Some(x.clone());
            ControlFlow::Break(())
        });
        let m = m.unwrap();
        let applied = apply_rule(
            &mut f.pool,
            &and_right,
            1,
            &seq,
            None,
            &m,
            SortCheck::Enforce,
        )
        .unwrap();
        assert_eq!(applied.branches.len(), 2);
        assert_eq!(applied.branches[0].label, Some(first));
        assert_eq!(applied.branches[0].sequent.succedent().as_slice(), &[p]);
        assert_eq!(applied.branches[1].sequent.succedent().as_slice(), &[q]);
        assert_eq!(applied.branches[1].sequent.antecedent().as_slice(), &[p]);
        assert_eq!(&*applied.app.find, &[pos(Side::Succedent, 0, &[])]);

        // into a proof tree: two goals; the first closes with `close` (p |- p), the second stays
        let mut tree = ProofTree::new(seq.clone());
        let step = tree
            .apply(tree.root(), applied.app, ViewId::ZERO, applied.branches)
            .unwrap();
        let kids: Vec<_> = tree.step(step).children().into_iter().collect();
        let g = tree.sequent(kids[0]).clone();
        let mut b = Bindings::new(&close);
        let mut m = None;
        let _ = match_sequent(&f.pool, &close, &g, &mut b, &mut |x| {
            m = Some(x.clone());
            ControlFlow::Break(())
        });
        let applied = apply_rule(
            &mut f.pool,
            &close,
            2,
            &g,
            None,
            &m.unwrap(),
            SortCheck::Enforce,
        )
        .unwrap();
        assert!(applied.branches.is_empty());
        assert_eq!(&*applied.app.assumes, &[pos(Side::Succedent, 0, &[])]);
        tree.apply(kids[0], applied.app, ViewId::ZERO, applied.branches)
            .unwrap();
        assert!(tree.is_closed(kids[0]) && !tree.is_proof_closed());
        assert_eq!(tree.open_goals(), &[kids[1]]);
    }

    #[test]
    fn assumes_add_and_inferred_generics() {
        let mut f = fx();
        let (three, zero, negone) = (f.c(f.three), f.c(f.zero), f.c(f.one_int));
        // \assumes(le(a, b) ==>) \find(a) \replacewith(ifThenElse(le(b, a), b, a)) \add(le(b, a) ==>)
        let int = f.sort(f.int);
        let mut r = rb(&mut f, "demo");
        let a = r.declare_sv(f.names.intern("a"), SvKind::Term, SortReq::Sub(int));
        let b = r.declare_sv(f.names.intern("b"), SvKind::Term, SortReq::Sub(int));
        let (pa, pb) = (r.sv(a).unwrap(), r.sv(b).unwrap());
        let le = r.app(f.le, &[], &[], &[pa, pb]).unwrap();
        r.assume(Side::Antecedent, le);
        r.find_term(pa);
        let (ta, tb) = (r.tmpl_sv(a).unwrap(), r.tmpl_sv(b).unwrap());
        let cond = r.tmpl_app(f.le, &[], &[], &[tb, ta]).unwrap();
        let ite = r
            .tmpl_app(f.ite, &[GenTmpl::Infer], &[], &[cond, tb, ta])
            .unwrap();
        let mut g = GoalTemplate::new(Replace::Term(ite));
        g.add_ante.push(cond);
        r.goal(g);
        let rule = r.finish().unwrap();
        let hyp = f.pool.mk_inferred(f.le, &[], &[three, negone]).unwrap();
        let goal = f.pool.mk_inferred(f.le, &[], &[three, zero]).unwrap();
        let seq = Sequent::new()
            .add(&f.pool, Side::Antecedent, hyp)
            .unwrap()
            .sequent
            .add(&f.pool, Side::Succedent, goal)
            .unwrap()
            .sequent;
        let at = pos(Side::Succedent, 0, &[0]); // the `three` in le(three, zero)
        let m = first_match(&f, &rule, &seq, &at).expect("matches");
        assert_eq!(m.matched(), &[(Side::Antecedent, 0)]);
        let applied = apply_rule(
            &mut f.pool,
            &rule,
            0,
            &seq,
            Some(&at),
            &m,
            SortCheck::Enforce,
        )
        .unwrap();
        let s = &applied.branches[0].sequent;
        let cond = f.pool.mk_inferred(f.le, &[], &[negone, three]).unwrap();
        let ite = f
            .pool
            .mk_inferred(f.ite, &[], &[cond, negone, three])
            .unwrap();
        let rewritten = f.pool.mk_inferred(f.le, &[], &[ite, zero]).unwrap();
        assert_eq!(s.succedent().as_slice(), &[rewritten]);
        assert_eq!(s.antecedent().as_slice(), &[hyp, cond]);
        assert_eq!(&*applied.app.assumes, &[pos(Side::Antecedent, 0, &[])]);
    }

    #[test]
    fn builder_rejects_inconsistent_rules() {
        let mut f = fx();
        let mut r = rb(&mut f, "bad");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Formula, SortReq::Any);
        let y = r.declare_sv(f.names.intern("y"), SvKind::Formula, SortReq::Any);
        let px = r.sv(x).unwrap();
        r.find_term(px);
        let ty = r.tmpl_sv(y).unwrap();
        r.goal(GoalTemplate::new(Replace::Term(ty)));
        // `y` is bound by no pattern: it is free, to be supplied when applying
        let rule = r.finish().unwrap();
        assert_eq!(rule.free_svs(), &[y]);
        assert!(rule.is_free(y) && !rule.is_free(x));
        let mut r = rb(&mut f, "bad2");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Formula, SortReq::Any);
        let px = r.sv(x).unwrap();
        r.find_term(px);
        r.goal(GoalTemplate::new(Replace::Sequent {
            ante: vec![],
            succ: vec![],
        }));
        assert!(matches!(r.finish(), Err(PatternError::BadReplace)));
    }
}

mod no_find {
    use std::ops::ControlFlow;

    use super::*;

    fn rb(f: &mut Fx, name: &str) -> RuleBuilder {
        RuleBuilder::new(f.pool.signature().clone(), f.names.intern(name))
    }

    /// `cut { \schemaVar c: Formula; \add(c ==>); \add(==> c) }`
    fn cut(f: &mut Fx) -> (Rule, SvId) {
        let mut r = rb(f, "cut");
        let c = r.declare_sv(f.names.intern("c"), SvKind::Formula, SortReq::Any);
        r.no_find();
        let tc = r.tmpl_sv(c).unwrap();
        let mut t = GoalTemplate::new(Replace::Nothing);
        t.add_ante = vec![tc];
        r.goal(t);
        let mut t = GoalTemplate::new(Replace::Nothing);
        t.add_succ = vec![tc];
        r.goal(t);
        (r.finish().unwrap(), c)
    }

    fn all_matches(f: &Fx, rule: &Rule, seq: &Sequent, b: &mut Bindings) -> Vec<Vec<(Side, u32)>> {
        let mut out = Vec::new();
        let _ = match_no_find(&f.pool, rule, seq, b, &mut |m| {
            out.push(m.matched().to_vec());
            ControlFlow::Continue(())
        });
        out
    }

    fn at(f: &Fx, rule: &Rule, seq: &Sequent, a: usize, i: usize) -> Vec<Vec<(Side, u32)>> {
        let mut b = Bindings::new(rule);
        let mut out = Vec::new();
        let _ = match_no_find_at(&f.pool, rule, seq, a, i, &mut b, &mut |m| {
            out.push(m.matched().to_vec());
            ControlFlow::Continue(())
        });
        out
    }

    #[test]
    fn cut_with_a_supplied_formula() {
        let mut f = fx();
        let (rule, c) = cut(&mut f);
        assert!(matches!(rule.find(), Find::None));
        assert_eq!(rule.free_svs(), &[c]);
        let p = f.c(f.p);
        let (three, zero) = (f.c(f.three), f.c(f.zero));
        let q = f.pool.mk_inferred(f.le, &[], &[three, zero]).unwrap();
        let seq = Sequent::new()
            .add(&f.pool, Side::Succedent, p)
            .unwrap()
            .sequent;
        let mut b = Bindings::new(&rule);
        // without the cut formula the rule matches (once), but cannot be applied
        assert_eq!(all_matches(&f, &rule, &seq, &mut b), vec![vec![]]);
        assert_eq!(
            apply_rule(&mut f.pool, &rule, 0, &seq, None, &b, SortCheck::Enforce).unwrap_err(),
            ApplyError::UnboundSv(c)
        );
        // a term or an open formula is refused
        assert_eq!(b.set(&f.pool, &rule, c, three), Err(SetError::Mismatch));
        let int = f.sort(f.int);
        let v0 = f.var(int, 0);
        let open = f.pool.mk_inferred(f.le, &[], &[v0, zero]).unwrap();
        assert_eq!(b.set(&f.pool, &rule, c, open), Err(SetError::NotClosed));
        assert_eq!(
            b.set(&f.pool, &rule, SvId(7), q),
            Err(SetError::UnknownSv(SvId(7)))
        );
        b.set(&f.pool, &rule, c, q).unwrap();
        // matching keeps what was set
        assert_eq!(all_matches(&f, &rule, &seq, &mut b).len(), 1);
        assert_eq!(b.get(c), Some(q));
        let applied =
            apply_rule(&mut f.pool, &rule, 5, &seq, None, &b, SortCheck::Enforce).unwrap();
        assert!(applied.app.find.is_empty() && applied.app.assumes.is_empty());
        assert_eq!(applied.app.rule, 5);
        assert_eq!(applied.app.inst.svs[0], Some(q));
        let [t, e] = &applied.branches[..] else {
            panic!("two branches")
        };
        assert_eq!(t.sequent.antecedent().as_slice(), &[q]);
        assert_eq!(t.sequent.succedent().as_slice(), &[p]);
        assert_eq!(e.sequent.antecedent().as_slice(), &[]);
        assert_eq!(e.sequent.succedent().as_slice(), &[p, q]);
        // rules without find match nothing of the other kinds, and vice versa
        let mut b = Bindings::new(&rule);
        let mut n = 0;
        let _ = match_term(&f.pool, &rule, &seq, p, &mut b, &mut |_| {
            n += 1;
            ControlFlow::Continue(())
        });
        let _ = match_sequent(&f.pool, &rule, &seq, &mut b, &mut |_| {
            n += 1;
            ControlFlow::Continue(())
        });
        assert_eq!(n, 0);
    }

    #[test]
    fn a_free_variable_with_a_slot_sort() {
        let mut f = fx();
        // \schemaVar t: S (generic) ; \add(le(t, t) ==>)  -- only to exercise slot binding
        let mut r = rb(&mut f, "refl");
        let t = r.declare_sv(f.names.intern("t"), SvKind::Term, SortReq::Slot(0));
        r.no_find();
        let tt = r.tmpl_sv(t).unwrap();
        let le = r.tmpl_app(f.le, &[], &[], &[tt, tt]).unwrap();
        let mut g = GoalTemplate::new(Replace::Nothing);
        g.add_ante = vec![le];
        r.goal(g);
        let rule = r.finish().unwrap();
        let three = f.c(f.three);
        let mut b = Bindings::new(&rule);
        b.set(&f.pool, &rule, t, three).unwrap();
        assert_eq!(b.slot(0), Some(GenArg::Sort(f.sort(f.int))));
        // the same again is fine
        b.set(&f.pool, &rule, t, three).unwrap();
        let p = f.c(f.p);
        assert_eq!(b.set(&f.pool, &rule, t, p), Err(SetError::Mismatch));
    }

    #[test]
    fn no_find_rules_only_add() {
        let mut f = fx();
        let mut r = rb(&mut f, "bad");
        let c = r.declare_sv(f.names.intern("c"), SvKind::Formula, SortReq::Any);
        r.no_find();
        let tc = r.tmpl_sv(c).unwrap();
        r.goal(GoalTemplate::new(Replace::Term(tc)));
        assert_eq!(r.finish().unwrap_err(), PatternError::BadReplace);
        let mut r = rb(&mut f, "bad");
        let c = r.declare_sv(f.names.intern("c"), SvKind::Formula, SortReq::Any);
        r.no_find();
        let tc = r.tmpl_sv(c).unwrap();
        r.goal(GoalTemplate::new(Replace::Sequent {
            ante: vec![tc],
            succ: vec![],
        }));
        assert_eq!(r.finish().unwrap_err(), PatternError::BadReplace);
        // a rule without anything is still `NoFind`
        assert_eq!(rb(&mut f, "x").finish().unwrap_err(), PatternError::NoFind);
    }

    /// `lemma { \assumes(le(x, y) ==>) \add(le(x, y) ==>) }`, with `x`, `y` of sort int.
    fn lemma(f: &mut Fx) -> Rule {
        let int = f.sort(f.int);
        let mut r = rb(f, "lemma");
        let x = r.declare_sv(f.names.intern("x"), SvKind::Term, SortReq::Sub(int));
        let y = r.declare_sv(f.names.intern("y"), SvKind::Term, SortReq::Sub(int));
        r.no_find();
        let (px, py) = (r.sv(x).unwrap(), r.sv(y).unwrap());
        let le = r.app(f.le, &[], &[], &[px, py]).unwrap();
        r.assume(Side::Antecedent, le);
        let (tx, ty) = (r.tmpl_sv(x).unwrap(), r.tmpl_sv(y).unwrap());
        let le = r.tmpl_app(f.le, &[], &[], &[ty, tx]).unwrap();
        let mut g = GoalTemplate::new(Replace::Nothing);
        g.add_succ = vec![le];
        r.goal(g);
        r.finish().unwrap()
    }

    /// `pair { \assumes(a, b ==>) \closegoal }`: two bare formula patterns on one side.
    fn pair(f: &mut Fx) -> Rule {
        let mut r = rb(f, "pair");
        let a = r.declare_sv(f.names.intern("a"), SvKind::Formula, SortReq::Any);
        let b = r.declare_sv(f.names.intern("b"), SvKind::Formula, SortReq::Any);
        r.no_find();
        let (pa, pb) = (r.sv(a).unwrap(), r.sv(b).unwrap());
        r.assume(Side::Antecedent, pa);
        r.assume(Side::Antecedent, pb);
        r.finish().unwrap()
    }

    #[test]
    fn the_index_keys_no_find_rules_by_their_assumes() {
        let mut f = fx();
        let (c, _) = cut(&mut f);
        let lemma = lemma(&mut f);
        let pair = pair(&mut f);
        let rules = [c, lemma, pair];
        let ix = RuleIndex::build(&f.pool, &rules);
        assert_eq!(ix.no_find(), &[0, 1, 2]);
        assert_eq!(ix.no_find_plain(), &[0]);
        // term look-ups never see them
        let (three, zero, p) = (f.c(f.three), f.c(f.zero), f.c(f.p));
        let le = f.pool.mk_inferred(f.le, &[], &[three, zero]).unwrap();
        let mut out = Vec::new();
        ix.candidates(&f.pool, le, &mut out);
        assert!(out.is_empty());
        assert_eq!(ix.num_wild(), 0);
        let mut out = Vec::new();
        ix.assumes_candidates(&f.pool, Side::Antecedent, le, &mut out);
        out.sort();
        assert_eq!(out, vec![(1, 0), (2, 0), (2, 1)]);
        ix.assumes_candidates(&f.pool, Side::Antecedent, p, &mut out);
        out.sort();
        assert_eq!(out, vec![(2, 0), (2, 1)]);
        ix.assumes_candidates(&f.pool, Side::Succedent, le, &mut out);
        assert!(out.is_empty());

        // matching the lemma at the formula the index named, and applying it
        let seq = Sequent::new()
            .add_all(&f.pool, Side::Antecedent, &[p, le])
            .unwrap()
            .sequent;
        assert_eq!(
            at(&f, &rules[1], &seq, 0, 1),
            vec![vec![(Side::Antecedent, 1)]]
        );
        assert!(at(&f, &rules[1], &seq, 0, 0).is_empty());
        let mut b = Bindings::new(&rules[1]);
        let mut found = None;
        let _ = match_no_find(&f.pool, &rules[1], &seq, &mut b, &mut |m| {
            found = Some(m.clone());
            ControlFlow::Break(())
        });
        let m = found.expect("matches");
        let applied = apply_rule(
            &mut f.pool,
            &rules[1],
            1,
            &seq,
            None,
            &m,
            SortCheck::Enforce,
        )
        .unwrap();
        let flipped = f.pool.mk_inferred(f.le, &[], &[zero, three]).unwrap();
        assert_eq!(
            applied.branches[0].sequent.succedent().as_slice(),
            &[flipped]
        );
        assert_eq!(
            &*applied.app.assumes,
            &[PosInOccurrence::new(Side::Antecedent, 1, PosInTerm::top())]
        );
    }

    #[test]
    fn focused_matching_finds_each_match_with_a_formula_once() {
        let mut f = fx();
        let pair = pair(&mut f);
        let (three, zero, p) = (f.c(f.three), f.c(f.zero), f.c(f.p));
        let le = f.pool.mk_inferred(f.le, &[], &[three, zero]).unwrap();
        let q = f.and(p, le);
        let seq = Sequent::new()
            .add_all(&f.pool, Side::Antecedent, &[p, le, q])
            .unwrap()
            .sequent;
        let mut b = Bindings::new(&pair);
        let all = all_matches(&f, &pair, &seq, &mut b);
        assert_eq!(all.len(), 9);
        for i in 0..3u32 {
            let mut focused = at(&f, &pair, &seq, 0, i as usize);
            focused.extend(at(&f, &pair, &seq, 1, i as usize));
            focused.sort_by_key(|m| m.iter().map(|x| x.1).collect::<Vec<_>>());
            let mut expect: Vec<_> = all
                .iter()
                .filter(|m| m.contains(&(Side::Antecedent, i)))
                .cloned()
                .collect();
            expect.sort_by_key(|m| m.iter().map(|x| x.1).collect::<Vec<_>>());
            assert_eq!(focused, expect, "formula {i}");
        }
        // out of range
        assert!(at(&f, &pair, &seq, 2, 0).is_empty());
        assert!(at(&f, &pair, &seq, 0, 3).is_empty());
    }
}
