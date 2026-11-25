use lalrpop_util::{ParseError, lalrpop_mod};

use dale_index::Idx;
use dale_util::symbol::{Interner, Symbol};

use crate::{dl_theory::Token, raw_theory::NodeId};

pub mod check;
pub mod cost;
mod ctx;
pub mod ir;
pub mod raw_theory;
mod resolve;

pub struct ParserCtx {
    next_id: NodeId,
    interner: Interner<Symbol>,
}

impl Default for ParserCtx {
    fn default() -> Self {
        Self::new()
    }
}

impl ParserCtx {
    pub fn new() -> Self {
        Self {
            next_id: NodeId::ZERO,
            interner: Default::default(),
        }
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    pub fn get_symbol(&self, sym: Symbol) -> &str {
        self.interner.get(sym)
    }

    pub fn id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id.increment_by(1);
        id
    }
}

lalrpop_mod!(
    #[allow(clippy::type_complexity)]
    dl_theory
);

fn get_line(input: &str, loc: usize) -> &str {
    let mut start = loc;
    while start > 0 {
        if &input[start - 1..start] == "\n" {
            break;
        }
        start -= 1;
    }
    let mut end = loc + 1;
    while end < input.len() - 1 {
        if &input[end..end + 1] == "\n" {
            break;
        }
        end += 1;
    }

    &input[start..end]
}

pub struct Pos {
    pub line: usize,
    pub column: usize,
}

pub fn loc_to_pos(input: &str, loc: usize) -> Pos {
    let mut line = 1;
    let mut column = 1;
    for c in input.chars().take(loc) {
        match c {
            '\n' => {
                line += 1;
                column = 1;
            }
            _ => {
                column += 1;
            }
        }
    }
    Pos { line, column }
}

pub fn err_report(input: &str, e: ParseError<usize, Token<'_>, &str>) -> String {
    match e {
        ParseError::InvalidToken { location } => {
            let pos = loc_to_pos(input, location);
            let line = get_line(input, location);
            format!(
                "Error at line {}, column {}: Invalid token\n\t{}",
                pos.line, pos.column, line
            )
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let pos = loc_to_pos(input, location);
            let line = get_line(input, location);
            let one_of = expected.join(", ");
            format!(
                "Unexpected end of file at line {}, column {}; expected one of {}\n{}",
                pos.line, pos.column, one_of, line
            )
        }
        ParseError::UnrecognizedToken { token, expected } => {
            let pos = loc_to_pos(input, token.0);
            let line = get_line(input, token.0);
            let one_of = expected.join(", ");
            format!(
                "Unexpected token {} at line {}, column {}; expected one of {}\n{}",
                token.1, pos.line, pos.column, one_of, line
            )
        }
        ParseError::ExtraToken { token } => {
            let pos = loc_to_pos(input, token.0);
            let line = get_line(input, token.0);
            format!(
                "Unexpected token at line {}, column {}; expected end of file\n{}",
                pos.line, pos.column, line
            )
        }
        ParseError::User { error } => error.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use dale_util::arena::DroplessArena;

    use crate::{ctx::Ctx, resolve::resolve};

    use super::*;

    #[test]
    fn basic() {
        let mut ctx = ParserCtx::new();
        let theory = dl_theory::TheoryParser::new()
            .parse(
                &mut ctx,
                r#"
      \theory Seq {
        \use int;
        \use fol::{quant::a, rel::*};
        \sorts {}
      }
      "#,
            )
            .unwrap();
        println!("{theory:?}");
        assert_eq!("Seq", ctx.get_symbol(theory.name.node));
        assert_eq!(3, theory.items.len());
    }

    fn parse_expr(input: &str) -> crate::raw_theory::Expr {
        let mut ctx = ParserCtx::new();
        match dl_theory::ExprParser::new().parse(&mut ctx, input) {
            Ok(e) => e,
            Err(e) => panic!("{}", err_report(input, e)),
        }
    }

    #[test]
    fn cost_expr_precedence() {
        use crate::raw_theory::{BinOp, ExprKind};

        // `-5000` is a literal, not a negation.
        assert!(matches!(parse_expr("-5000").kind, ExprKind::Int(-5000)));
        // `a - b` is a subtraction (the minus is not part of the literal).
        assert!(matches!(
            parse_expr("a -5").kind,
            ExprKind::Binary(BinOp::Sub, ..)
        ));
        // * binds tighter than +, && tighter than ||, comparison tighter than &&.
        match parse_expr("1 + 2 * 3").kind {
            ExprKind::Binary(BinOp::Add, _, rhs) => {
                assert!(matches!(rhs.kind, ExprKind::Binary(BinOp::Mul, ..)))
            }
            k => panic!("{k:?}"),
        }
        match parse_expr("a || b && c < 3").kind {
            ExprKind::Binary(BinOp::Or, _, rhs) => match rhs.kind {
                ExprKind::Binary(BinOp::And, _, cmp) => {
                    assert!(matches!(cmp.kind, ExprKind::Binary(BinOp::Lt, ..)))
                }
                k => panic!("{k:?}"),
            },
            k => panic!("{k:?}"),
        }
        parse_expr("if a && !b { top } else { Cost::REWRITE + scale(count_max_d_path, 10.0) }");
        parse_expr("let x = find_depth; x + 1");
        parse_expr("sum(t in sub_terms(find): if is_literal(t) { 1 } else { 0 })");
    }

    #[test]
    fn rulesets_and_costs() {
        let mut ctx = ParserCtx::new();
        let input = r#"
          \theory T {
            \costs {
              const BASE: Cost = -5000;
              fn base(extra: Cost, t: Term<Any>) -> Cost { BASE + extra }
            }
            \rulesets {
              plain { Cost::CLOSE }
              param(t: Term<Any>, k: Int) \stage(app) {
                guard no_duplicate_app;
                if k < 3 { base(0, t) } else { top }
              }
              extend plain { Cost::CLOSE }
              override other { 0 }
            }
            \rules {
              r {
                \schemaVar b: Any;
                \find(b)
                \replacewith(b)
                \rulesets(plain, param(b, 3))
              }
            }
          }
        "#;
        let file = match dl_theory::FileParser::new().parse(&mut ctx, input) {
            Ok(f) => f,
            Err(e) => panic!("{}", err_report(input, e)),
        };
        assert_eq!(1, file.theories.len());
        assert_eq!(3, file.theories[0].items.len());
    }

    /// Runs the whole pipeline on `input` (which must resolve) and returns the check results.
    fn check_src(input: &str) -> Vec<check::CheckErr> {
        let mut ctx = ParserCtx::new();
        let file = match dl_theory::FileParser::new().parse(&mut ctx, input) {
            Ok(f) => f,
            Err(e) => panic!("{}", err_report(input, e)),
        };
        let ir_arena = DroplessArena::default();
        let cx = Ctx {
            ir_arena: &ir_arena,
            interner: &ctx.interner,
        };
        let resolver = resolve(cx, &file);
        assert!(
            resolver.errors.is_empty(),
            "{:?}",
            resolver
                .errors
                .iter()
                .map(|e| e.format(cx))
                .collect::<Vec<_>>()
        );
        let (ir, errors) = ir::lower::lower(file, cx, resolver);
        assert!(errors.is_empty(), "unresolved names");
        check::check_ir(&mut ir.clone(), cx.ir_arena, cx.interner)
    }

    /// A theory with a formula sort, the given cost items / rulesets, and a rule `r` that has a
    /// schema variable `b: Formula`, a schema variable `x: Any` and the given `\rulesets(...)`.
    fn prop_theory(costs: &str, rulesets: &str, rule_sets: &str) -> String {
        format!(
            r#"
            \theory P {{
              \sorts {{ \top Any; \formula Formula; }}
              \operators {{ \rigid imp(Formula, Formula) -> Formula; }}
              \costs {{ {costs} }}
              \rulesets {{ {rulesets} }}
              \rules {{
                r {{
                  \schemaVar b: Formula;
                  \schemaVar x: Any;
                  \find(b)
                  \closegoal
                  \rulesets({rule_sets})
                }}
              }}
            }}
            "#
        )
    }

    use check::CheckErr;

    #[test]
    fn cost_checks_accept_valid_theory() {
        let errs = check_src(&prop_theory(
            r#"
              const BASE: Cost = -5000;
              fn f(extra: Cost) -> Cost { BASE + scale(count_max_d_path, 10.0) + extra }
            "#,
            r#"
              a \stage(formula) { f(0) }
              p(t: Term<Formula>, k: Int) \stage(app) {
                guard no_duplicate_app;
                if k < 3 && is_literal(t) { Cost::REWRITE } else { top }
              }
              below { if directly_below_op_at_idx(imp, 1) { 100 } else { 0 } }
              total { sum(t in sub_terms(find): if is_literal(t) { 1 } else { 0 }) }
            "#,
            "a, p(b, 3), below, total",
        ));
        assert!(errs.is_empty(), "{errs:?}");
    }

    #[test]
    fn stage_assertion_error_and_warning() {
        // Body needs `formula` but only `app` is asserted: error.
        let errs = check_src(&prop_theory(
            "",
            r#"g \stage(app) { Cost::DEFER + count_max_d_path }"#,
            "g",
        ));
        assert!(
            matches!(errs[..], [CheckErr::StageExceedsAssertion(..)]),
            "{errs:?}"
        );
        assert!(!errs[0].is_warning());

        // Body needs only `taclet` but `app` is asserted: warning.
        let errs = check_src(&prop_theory("", r#"c \stage(app) { Cost::REWRITE }"#, "c"));
        assert!(
            matches!(errs[..], [CheckErr::StageAssertionStronger(..)]),
            "{errs:?}"
        );
        assert!(errs[0].is_warning());

        // `sequent` and `history` are independent: neither implies the other.
        let errs = check_src(&prop_theory(
            "",
            r#"h \stage(history) { count_occurrences(find) }"#,
            "h",
        ));
        assert!(
            matches!(errs[..], [CheckErr::StageExceedsAssertion(..)]),
            "{errs:?}"
        );
    }

    #[test]
    fn constant_conditions_prune_branches() {
        // The condition is a compile-time constant, so the `formula` branch is never taken.
        let errs = check_src(&prop_theory(
            "const K: Int = 2;",
            r#"d \stage(taclet) { if K < 3 { 0 } else { count_max_d_path } }"#,
            "d",
        ));
        assert!(errs.is_empty(), "{errs:?}");

        // A non-constant condition counts both branches.
        let errs = check_src(&prop_theory(
            "",
            r#"d \stage(taclet) { if branch_count < 3 { 0 } else { count_max_d_path } }"#,
            "d",
        ));
        assert!(
            matches!(errs[..], [CheckErr::StageExceedsAssertion(..)]),
            "{errs:?}"
        );
    }

    #[test]
    fn ruleset_arguments_are_checked() {
        let costs = "";
        let rulesets = r#"p(t: Term<Formula>, k: Int) { if is_literal(t) { k } else { 0 } }"#;

        // Too few arguments.
        let errs = check_src(&prop_theory(costs, rulesets, "p(b)"));
        assert!(
            matches!(errs[..], [CheckErr::RuleSetArity(_, _, 2, 1)]),
            "{errs:?}"
        );
        // A bare name for a parameterised ruleset.
        let errs = check_src(&prop_theory(costs, rulesets, "p"));
        assert!(
            matches!(errs[..], [CheckErr::RuleSetArity(_, _, 2, 0)]),
            "{errs:?}"
        );
        // A term where an integer is expected.
        let errs = check_src(&prop_theory(costs, rulesets, "p(b, b)"));
        assert!(
            matches!(errs[..], [CheckErr::RuleSetArgMismatch(_, _, 1)]),
            "{errs:?}"
        );
        // `x: Any` is not a subsort of `Formula`.
        let errs = check_src(&prop_theory(costs, rulesets, "p(x, 1)"));
        assert!(
            matches!(errs[..], [CheckErr::RuleSetArgMismatch(_, _, 0)]),
            "{errs:?}"
        );
        // A `Formula` is fine where `Any` is expected.
        let errs = check_src(&prop_theory(costs, r#"q(t: Term<Any>) { 0 }"#, "q(b)"));
        assert!(errs.is_empty(), "{errs:?}");
    }

    #[test]
    fn cost_expressions_are_typed() {
        let bad = |costs: &str, rulesets: &str| check_src(&prop_theory(costs, rulesets, ""));

        // `if` needs a Bool condition.
        let errs = bad("", "a { if 1 { 0 } else { 1 } }");
        assert!(
            matches!(errs[..], [CheckErr::CostTypeMismatch(..)]),
            "{errs:?}"
        );
        // `top` is a cost, not an integer.
        let errs = bad("fn n() -> Int { top }", "");
        assert!(
            matches!(errs[..], [CheckErr::CostTypeMismatch(..)]),
            "{errs:?}"
        );
        // A ruleset body is a cost, not a Bool.
        let errs = bad("", "a { matched_assumes }");
        assert!(
            matches!(errs[..], [CheckErr::CostTypeMismatch(..)]),
            "{errs:?}"
        );
        // Branches of an `if` must agree.
        let errs = bad(
            "",
            "a { if matched_assumes { 1 } else { matched_assumes } }",
        );
        assert!(
            matches!(errs[..], [CheckErr::CostTypeMismatch(..)]),
            "{errs:?}"
        );
        // Arity of a builtin / of a cost function.
        let errs = bad("", "a { scale(1) }");
        assert!(
            matches!(errs[..], [CheckErr::CostArity(_, 2, 1)]),
            "{errs:?}"
        );
        let errs = bad("fn f(a: Cost) -> Cost { a }", "a { f(1, 2) }");
        assert!(
            matches!(errs[..], [CheckErr::CostArity(_, 1, 2)]),
            "{errs:?}"
        );
        // Only the binder builtins take a binder, and they take nothing else.
        let errs = bad("", "a { sum(1) }");
        assert!(
            matches!(errs[..], [CheckErr::CostBinderMisuse(_)]),
            "{errs:?}"
        );
        // The generator of a binder must be a generator.
        let errs = bad("", "a { sum(t in find: 1) }");
        assert!(
            matches!(errs[..], [CheckErr::CostTypeMismatch(..)]),
            "{errs:?}"
        );
        // The body of `sum` is a cost, the body of `any` a Bool.
        let errs = bad("", "a { if any(t in sub_terms(find): 1) { 0 } else { 1 } }");
        assert!(
            matches!(errs[..], [CheckErr::CostTypeMismatch(..)]),
            "{errs:?}"
        );
        // A constant must not read the application.
        let errs = bad("const C: Cost = count_max_d_path;", "");
        assert!(
            matches!(errs[..], [CheckErr::CostConstNotConstant(..)]),
            "{errs:?}"
        );
        // Unknown type names.
        let errs = bad("const C: Frob = 1;", "");
        assert!(
            matches!(errs[..], [CheckErr::UnknownCostType(_)]),
            "{errs:?}"
        );
        // Locals are typed.
        let errs = bad("", "a { let n = find_depth; n + 1 }");
        assert!(errs.is_empty(), "{errs:?}");
    }

    #[test]
    fn extend_keeps_parameters() {
        let theories = |ext: &str| {
            format!(
                r#"
                \theory A {{
                  \sorts {{ \top Any; \formula Formula; }}
                  \rulesets {{ p(k: Int) {{ 0 }} }}
                }}
                \theory B {{
                  \use A::*;
                  \rulesets {{ {ext} }}
                }}
                "#
            )
        };
        let errs = check_src(&theories("extend p(k: Int) { 1 }"));
        assert!(errs.is_empty(), "{errs:?}");
        let errs = check_src(&theories("extend p(k: Cost) { 1 }"));
        assert!(
            matches!(errs[..], [CheckErr::ExtendParamMismatch(..)]),
            "{errs:?}"
        );
        let errs = check_src(&theories("override p { 1 }"));
        assert!(
            matches!(errs[..], [CheckErr::ExtendParamMismatch(..)]),
            "{errs:?}"
        );
    }

    #[test]
    fn rustydl() {
        use std::fs::read_to_string;

        let mut ctx = ParserCtx::new();
        let input = read_to_string("examples/RustyDL.theokey").unwrap();

        let rusty = match dl_theory::FileParser::new().parse(&mut ctx, &input) {
            Ok(r) => r,
            Err(e) => panic!("{}", err_report(&input, e)),
        };

        assert_eq!(6, rusty.theories.len());

        let ir_arena = DroplessArena::default();
        let cx = Ctx {
            ir_arena: &ir_arena,
            interner: &ctx.interner,
        };
        let resolver = resolve(cx, &rusty);
        if !resolver.errors.is_empty() {
            let e = &resolver.errors[0];
            let start = e.span().0;
            let pos = loc_to_pos(&input, start);
            panic!(
                "{}:{} '{}': {}",
                pos.line,
                pos.column,
                get_line(&input, start),
                e.format(cx)
            )
        }
        let (ir, errors) = ir::lower::lower(rusty, cx, resolver);
        if !errors.is_empty() {
            let e = &errors[0];
            let start = e.start;
            let pos = loc_to_pos(&input, start);
            panic!(
                "{}:{} '{}': {}",
                pos.line,
                pos.column,
                get_line(&input, start),
                "No resolution"
            )
        }
        let all = check::check_ir(&mut ir.clone(), cx.ir_arena, cx.interner);
        for w in all.iter().filter(|e| e.is_warning()) {
            println!("warning: {}", w.format());
        }
        let errors: Vec<_> = all.into_iter().filter(|e| !e.is_warning()).collect();
        if !errors.is_empty() {
            let e = &errors[0];
            let id = e.id();
            for i in ir.items {
                println!("{:?}", i.1)
            }
            println!("{}", get_line(&input, 837));
            panic!("{id:?} {}", e.format());
        }
    }
}
