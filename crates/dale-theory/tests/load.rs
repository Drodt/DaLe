//! Lowering `RustyDL.theokey` and small theories into `dale-logic`.

mod common;

use std::ops::ControlFlow;

use common::*;
use dale_logic::*;
use dale_theory::{LoadError, Theory};

const BASE: &str = r#"
\theory T {
  \sorts { \top Any; \formula Formula; }
  \operators { \rigid not(Formula) -> Formula; \rigid and(Formula, Formula) -> Formula; }
  \predicates { p; q; }
"#;

fn load(rules: &str) -> Result<Theory, LoadError> {
    Theory::load_text("T.theokey", format!("{BASE} \\rules {{ {rules} }} }}"))
}

#[test]
fn rustydl_signature() {
    let th = Theory::load(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../dale-syn/examples/RustyDL.theokey"
    ))
    .unwrap_or_else(|e| panic!("{e}"));
    let op = |n: &str| {
        let [id] = th.ops_named(n) else {
            panic!("{n}: {:?}", th.ops_named(n))
        };
        th.sig.op_decl(*id)
    };
    // binders bind argument 0, the variable has the first generic parameter as sort
    assert_eq!(op("all").bind, Some(Binding::one(0, SortPat::Param(0))));
    assert_eq!(op("exists").generics.len(), 1);
    // `\\boundVar var<S>(NUMBER) -> S` is the bound variable operator, NUMBER a meta argument
    assert_eq!(op("var").kind, OpKind::BoundVar);
    assert_eq!(op("var").meta, [MetaKind::Nat]);
    assert_eq!(th.sig.bound_var(), th.ops_named("var").first().copied());
    // predicates are formulas; flags carry over
    assert_eq!(op("leq").result, SortPat::simple(th.sig.formula()));
    assert!(op("add").flags.rigid && !op("rdiv").flags.rigid);
    // const generics: `len<E, const N: int>(Array<E, const N>) -> int`
    let len = op("len");
    assert!(matches!(len.generics[1].kind, GenKind::Const(_)));
    let SortPat::Inst { args, .. } = &len.args[0] else {
        panic!()
    };
    assert_eq!(
        args[..],
        [GenArgPat::Sort(SortPat::Param(0)), GenArgPat::Const(1)]
    );
    // the meta sort is not a sort of the signature
    assert!(
        th.sig
            .sort_decls()
            .all(|(_, d)| th.name(d.name) != "NUMBER")
    );

    // every rule but `cut` (no \find) is loaded
    let skipped: Vec<String> = th.skipped.iter().map(|d| th.render(d)).collect();
    assert_eq!(skipped.len(), 1, "{skipped:?}");
    assert!(
        skipped[0].contains("`cut`") && skipped[0].contains("RustyDL.theokey:"),
        "{skipped:?}"
    );
    assert_eq!(th.rules.len(), 45);
    let close = th.rule_named("close").unwrap() as usize;
    assert_eq!(th.name(th.info[close].rule_sets[0]), "closure");
    assert_eq!(
        th.info[close].display_name.map(|s| th.name(s)),
        Some("close")
    );
}

/// The first match of `rule` in `seq` (term finds: at a top-level formula), applied.
fn apply_first(
    th: &mut Theory,
    rule: &str,
    seq: &Sequent,
) -> Option<Vec<(Option<String>, String)>> {
    let r = th.rule_named(rule).unwrap();
    let rule = &th.rules[r as usize];
    let mut b = Bindings::new(rule);
    let mut hit = None;
    match rule.find() {
        Find::Sequent { .. } => {
            let _ = match_sequent(&th.pool, rule, seq, &mut b, &mut |m| {
                hit = Some((None, m.clone()));
                ControlFlow::Break(())
            });
        }
        Find::Term(_) => {
            for (i, (side, f)) in seq.iter().enumerate() {
                let index = if side == Side::Antecedent {
                    i
                } else {
                    i - seq.antecedent().as_slice().len()
                };
                let pos = PosInOccurrence::top_level(side, index);
                let _ = match_term(&th.pool, rule, seq, f, &mut b, &mut |m| {
                    hit = Some((Some(pos.clone()), m.clone()));
                    ControlFlow::Break(())
                });
                if hit.is_some() {
                    break;
                }
            }
        }
    }
    let (pos, m) = hit?;
    let a = apply_rule(
        &mut th.pool,
        rule,
        r,
        seq,
        pos.as_ref(),
        &m,
        SortCheck::Enforce,
    )
    .unwrap();
    let names = th.names();
    Some(
        a.branches
            .iter()
            .map(|b| {
                let show = |s: &Semisequent| {
                    s.as_slice()
                        .iter()
                        .map(|t| th.pool.show_term(names, *t).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                (
                    b.label.map(|l| th.name(l).to_owned()),
                    format!(
                        "{} ==> {}",
                        show(b.sequent.antecedent()),
                        show(b.sequent.succedent())
                    ),
                )
            })
            .collect(),
    )
}

#[test]
fn rules_from_the_file_apply_like_keys() {
    let g = |v: &[(Option<&str>, &str)]| -> Vec<(Option<String>, String)> {
        v.iter()
            .map(|(l, s)| (l.map(str::to_owned), s.to_string()))
            .collect()
    };
    let apply = |rule: &str, seq: &str| {
        let (mut th, s) = prop_goal(seq);
        apply_first(&mut th, rule, &s)
    };

    // `\\replacewith( ==> b)` for an antecedent find moves the formula to the other side
    assert_eq!(
        apply("notLeft", "not(p0) ==> p1"),
        Some(g(&[(None, " ==> p1, p0")]))
    );
    // two goals, labelled
    assert_eq!(
        apply("orLeft", "or(p0, p1) ==>"),
        Some(g(&[(Some("#b"), "p0 ==> "), (Some("#c"), "p1 ==> ")]))
    );
    // `\\assumes` and `\\closegoal`
    assert_eq!(apply("close", "p0 ==> p0"), Some(vec![]));
    assert_eq!(apply("close", "p0 ==> p1"), None);
    // a term find with a ground pattern
    assert_eq!(
        apply("concrete_and_3", "==> and(p0, true)"),
        Some(g(&[(None, " ==> p0")]))
    );
    // `\\replacewith` and `\\add` in one goal
    assert_eq!(
        apply("cut_direct", "==> p2"),
        Some(g(&[
            (Some("CUT: #cutFormula TRUE"), "p2 ==> true"),
            (Some("CUT: #cutFormula FALSE"), " ==> false, p2"),
        ]))
    );
}

#[test]
fn unsupported_rules_are_skipped_with_a_reason() {
    let th = load(
        r#"
        ok { \schemaVar b: Formula; \find(not(not(b))) \replacewith(b) }
        shape { \schemaVar b: Formula; \find(not(b) ==>) \replacewith(b) }
        two { \schemaVar b: Formula; \find(b, b ==>) \closegoal }
        nofind { \schemaVar b: Formula; \add(b ==>) }
        "#,
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(th.rules.len(), 1);
    let msgs: Vec<String> = th.skipped.iter().map(|d| th.render(d)).collect();
    assert_eq!(msgs.len(), 3, "{msgs:?}");
    assert!(
        msgs[0].contains("`shape`") && msgs[0].contains("\\replacewith must have the shape"),
        "{msgs:?}"
    );
    assert!(
        msgs[1].contains("`two`") && msgs[1].contains("at most one formula per side"),
        "{msgs:?}"
    );
    assert!(
        msgs[2].contains("`nofind`") && msgs[2].contains("without \\find"),
        "{msgs:?}"
    );
    assert!(msgs[0].starts_with("T.theokey:"), "{msgs:?}");
}

#[test]
fn front_end_errors_are_passed_on() {
    let e = Theory::load_text("T.theokey", "\\theory T { \\sorts { \\top Any; ").unwrap_err();
    assert!(matches!(e, LoadError::Front(_)), "{e}");
}

const BINDERS: &str = r#"
\theory B {
  \sorts { \top Any; \formula Formula; \meta NUMBER; int; }
  \functions { \rigid \boundVar var<S>(NUMBER) -> S; \rigid zero -> int; }
  \predicates { le(int, int); }
  \operators {
    \rigid both<S>{S: 0, 1}(Formula, Formula) -> Formula;
    \rigid second<S>{S: 1}(Formula, Formula) -> Formula;
    \rigid allInt{int: 0}(Formula) -> Formula;
  }
}
"#;

#[test]
fn binders_of_several_arguments() {
    let mut th = Theory::load_text("B.theokey", BINDERS).unwrap_or_else(|e| panic!("{e}"));
    let decl = |th: &Theory, n: &str| th.ops_named(n)[0];
    let both = decl(&th, "both");
    assert_eq!(th.sig.op_decl(both).bind.as_ref().unwrap().args, [0, 1]);
    // a fixed sort for the variable
    let all_int = decl(&th, "allInt");
    let int = th
        .sig
        .sort_decls()
        .find(|(_, d)| th.name(d.name) == "int")
        .unwrap()
        .0;
    assert_eq!(
        th.sig.op_decl(all_int).bind,
        Some(Binding::one(0, SortPat::simple(int)))
    );

    let (zero, le, second) = (decl(&th, "zero"), decl(&th, "le"), decl(&th, "second"));
    let p = &mut th.pool;
    let int = p.mk_sort(int, &[]).unwrap();
    let zero = p.mk_inferred(zero, &[], &[]).unwrap();
    let v = p.mk_var(int, 0).unwrap();
    let a = p.mk_inferred(le, &[], &[v, zero]).unwrap();
    let b = p.mk_inferred(le, &[], &[zero, v]).unwrap();
    let both_int = p.mk_op(both, &[GenArg::Sort(int)], &[]).unwrap();
    let second_int = p.mk_op(second, &[GenArg::Sort(int)], &[]).unwrap();
    // bound in both arguments: closed
    let t = p.mk(both_int, &[a, b]).unwrap();
    assert!(p[t].is_closed());
    // bound only in the second: the variable in the first stays free
    let u = p.mk(second_int, &[a, b]).unwrap();
    assert_eq!(p[u].loose(), 1);
    // instantiation names the argument
    assert!(matches!(
        p.instantiate(t, zero),
        Err(LogicError::SeveralBoundArgs)
    ));
    let zz = p.mk_inferred(le, &[], &[zero, zero]).unwrap();
    assert_eq!(p.instantiate_arg(t, 1, zero).unwrap(), zz);
    assert!(matches!(
        p.instantiate_arg(u, 0, zero),
        Err(LogicError::NotBoundArg(0))
    ));
    assert_eq!(p.instantiate(u, zero).unwrap(), zz);
    // the sort of the variable is checked in every bound argument: `var<int>(0)` in the second
    // argument is not a variable of `both<Any>`
    let any = p.top();
    let bad = p.mk_op(both, &[GenArg::Sort(any)], &[]).unwrap();
    assert!(matches!(
        p.mk(bad, &[zz, b]),
        Err(LogicError::BindSort { .. })
    ));
}

#[test]
fn bad_binders_and_bound_variables() {
    let err = |ops: &str, fns: &str| {
        let text = format!(
            "\\theory B {{ \\sorts {{ \\top Any; \\formula Formula; \\meta NUMBER; }} \\functions {{ {fns} }} \\operators {{ {ops} }} }}"
        );
        match Theory::load_text("B.theokey", text) {
            Err(LoadError::Signature(m)) => m.join("\n"),
            Err(e) => panic!("{e}"),
            Ok(_) => panic!("loaded"),
        }
    };
    let m = err("\\rigid q<S>{S: 1, 0}(Formula, Formula) -> Formula;", "");
    assert!(m.contains("BindingOutOfRange"), "{m}");
    let m = err("\\rigid q<S>{S: 2}(Formula, Formula) -> Formula;", "");
    assert!(m.contains("BindingOutOfRange"), "{m}");
    let m = err("", "\\rigid \\boundVar v -> Any;");
    assert!(m.contains("BadBoundVar"), "{m}");
    let m = err(
        "",
        "\\rigid \\boundVar v<S>(NUMBER) -> S; \\rigid \\boundVar w<S>(NUMBER) -> S;",
    );
    assert!(
        m.contains("`w` is marked \\boundVar, but `v` already is"),
        "{m}"
    );
}

/// `BINDERS` with a problem.
fn binder_problem(seq: &str) -> Result<Theory, LoadError> {
    Theory::load_text(
        "B.theokey",
        format!("{BINDERS}\n\\problem {{ \\use B::*; {seq} }}"),
    )
}

fn problem_error(seq: &str) -> String {
    match binder_problem(seq) {
        Err(e) => e.to_string(),
        Ok(_) => panic!("loaded: {seq}"),
    }
}

#[test]
fn problems_with_binders_and_de_bruijn_indices() {
    let th = binder_problem(
        "le(zero, zero) ==> allInt(le(var(0), zero)), both<int>(le(var(0), zero), le(zero, var(0)))",
    )
    .unwrap_or_else(|e| panic!("{e}"));
    let seq = th.problem.clone().unwrap();
    let show = |t: TermIdx| th.pool.show_term(th.names(), t).to_string();
    let ante: Vec<String> = seq
        .antecedent()
        .as_slice()
        .iter()
        .map(|&t| show(t))
        .collect();
    let succ: Vec<String> = seq
        .succedent()
        .as_slice()
        .iter()
        .map(|&t| show(t))
        .collect();
    assert_eq!(ante, ["le(zero, zero)"]);
    assert_eq!(succ.len(), 2);
    for &t in seq.succedent().as_slice() {
        assert!(th.pool[t].is_closed());
    }
    // nested binders: var(1) is the outer variable
    let th =
        binder_problem("==> allInt(allInt(le(var(1), var(0))))").unwrap_or_else(|e| panic!("{e}"));
    let t = th.problem.as_ref().unwrap().succedent().as_slice()[0];
    assert!(th.pool[t].is_closed());
    // an explicit sort for the variable is checked against its binder
    let m = problem_error("==> allInt(le(var<Any>(0), zero))");
    assert!(m.contains("B.theokey:"), "{m}");
    // a file without a problem
    assert!(
        Theory::load_text("B.theokey", BINDERS)
            .unwrap()
            .problem
            .is_none()
    );
}

#[test]
fn bad_problems() {
    let m = problem_error("==> allInt(le(var(1), zero))");
    assert!(m.contains("`var(1)` is not bound here"), "{m}");
    let m = problem_error("==> both(le(var(0), zero), le(zero, var(0)))");
    assert!(m.contains("explicit generic arguments"), "{m}");
    let m = problem_error("==> zero");
    assert!(m.contains("not a formula"), "{m}");
    let m = problem_error("==> le(zero, 0)");
    assert!(m.contains("only be a meta argument"), "{m}");
    let m = problem_error("==> le(zero, unknown)");
    assert!(
        matches!(
            binder_problem("==> le(zero, unknown)"),
            Err(LoadError::Front(_))
        ),
        "{m}"
    );
    // one problem, at the top level
    let two =
        format!("{BINDERS}\n\\problem {{ \\use B::*; ==> }}\n\\problem {{ \\use B::*; ==> }}");
    let m = Theory::load_text("B.theokey", two).unwrap_err().to_string();
    assert!(m.contains("only one \\problem"), "{m}");
    let nested = format!("{BINDERS}\n\\module M {{ \\problem {{ \\use B::*; ==> }} }}");
    let m = Theory::load_text("B.theokey", nested)
        .unwrap_err()
        .to_string();
    assert!(m.contains("top level of the main file"), "{m}");
}

const SUBSORTS: &str = r#"
\theory S {
  \sorts { \top Any; \formula Formula; int; nat \extends int; Seq<E>; NeSeq<E> \extends Seq<E>; }
  \functions {
    \rigid zero -> nat;
    \rigid minusOne -> int;
    \rigid empty<E> -> Seq<E>;
    \rigid single<E>(E) -> NeSeq<E>;
  }
  \predicates { le(int, int); isEmpty<E>(Seq<E>); }
  \rules {
    // `zero: nat` may be passed where an `int` is expected, also in a rule
    leZero { \find(le(zero, zero)) \replacewith(le(zero, minusOne)) }
  }
}
"#;

#[test]
fn supersorts() {
    let mut th = Theory::load_text(
        "S.theokey",
        format!("{SUBSORTS}\n\\problem {{ \\use S::*; le(zero, minusOne) ==> isEmpty<nat>(single<nat>(zero)) }}"),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    let decl = |n: &str| {
        th.sig
            .sort_decls()
            .find(|(_, d)| th.name(d.name) == n)
            .unwrap()
    };
    let (int, _) = decl("int");
    assert_eq!(decl("nat").1.extends, Some(SortPat::simple(int)));
    assert_eq!(decl("int").1.extends, None);
    let (seq, _) = decl("Seq");
    assert_eq!(
        decl("NeSeq").1.extends,
        Some(SortPat::Inst {
            decl: seq,
            args: vec![GenArgPat::Sort(SortPat::Param(0))]
        })
    );
    let (nat_d, ne_d) = (decl("nat").0, decl("NeSeq").0);
    let pool = &mut th.pool;
    let (nat, int) = (
        pool.mk_sort(nat_d, &[]).unwrap(),
        pool.mk_sort(int, &[]).unwrap(),
    );
    assert!(pool.is_subsort(nat, int) && !pool.is_subsort(int, nat));
    let ne = pool.mk_sort(ne_d, &[GenArg::Sort(nat)]).unwrap();
    let s = pool.mk_sort(seq, &[GenArg::Sort(nat)]).unwrap();
    let s_int = pool.mk_sort(seq, &[GenArg::Sort(int)]).unwrap();
    assert!(pool.is_subsort(ne, s) && !pool.is_subsort(ne, s_int));
    assert_eq!(th.problem.as_ref().unwrap().len(), 2);
    assert!(th.skipped.is_empty(), "{:?}", th.skipped);
}

#[test]
fn only_one_supersort() {
    let text = "\\theory S { \\sorts { \\top Any; \\formula Formula; a; b; c \\extends a, b; } }";
    let e = Theory::load_text("S.theokey", text)
        .unwrap_err()
        .to_string();
    assert!(e.contains("error[parse]"), "{e}");
}

/// A predicate in a rule needs a formula sort; without one the checker reports it.
#[test]
fn predicates_without_a_formula_sort() {
    let text = "\\theory T { \\sorts { \\top Any; } \\predicates { p; } \\rules { r { \\find(p) \\replacewith(p) } } }";
    let e = Theory::load_text("T.theokey", text)
        .unwrap_err()
        .to_string();
    assert!(e.contains("Missing formula sort"), "{e}");
}
