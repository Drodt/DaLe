//! Compile-time knowledge about the cost language of `\rulesets`: the *stages* an expression can
//! live in (what information it needs) and the catalogue of builtin constants, functions and
//! guards.
//!
//! The stages mirror the locality scale of KeY's strategy features (`@StableCost`,
//! `@WeakStableCost`, volatile):
//!
//! | stage     | reads                                                      | KeY                |
//! |-----------|------------------------------------------------------------|--------------------|
//! | `taclet`  | the rule only (and constants)                              | rule-set binding   |
//! | `app`     | the application: instantiations, matched `\assumes`, find subterm | `@StableCost` |
//! | `formula` | additionally the whole find formula (ancestors, polarity)  | `@WeakStableCost`  |
//! | `sequent` | other formulas of the sequent                              | volatile           |
//! | `history` | the applied rules / the path to the node                   | volatile           |
//!
//! `taclet < app < formula` form a chain. `sequent` and `history` are independent flags on top of
//! it: they select which incremental structure the runtime has to maintain, so a cost that only
//! needs a per-node counter does not force an occurrence index of the sequent.
//!
//! Features that deal with programs / modalities are intentionally not part of the catalogue yet.

/// The information a cost expression needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Stage {
    pub level: Level,
    /// Reads other formulas of the sequent.
    pub sequent: bool,
    /// Reads the proof history (applied rules, path to the node).
    pub history: bool,
}

/// The chain part of a [`Stage`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Level {
    Taclet,
    App,
    Formula,
}

impl Stage {
    pub const TACLET: Stage = Stage::chain(Level::Taclet);
    pub const APP: Stage = Stage::chain(Level::App);
    pub const FORMULA: Stage = Stage::chain(Level::Formula);
    pub const SEQUENT: Stage = Stage {
        level: Level::Taclet,
        sequent: true,
        history: false,
    };
    pub const HISTORY: Stage = Stage {
        level: Level::Taclet,
        sequent: false,
        history: true,
    };

    const fn chain(level: Level) -> Stage {
        Stage {
            level,
            sequent: false,
            history: false,
        }
    }

    /// The least stage that is at least as informative as both arguments.
    pub fn join(self, other: Stage) -> Stage {
        Stage {
            level: self.level.max(other.level),
            sequent: self.sequent || other.sequent,
            history: self.history || other.history,
        }
    }

    /// `self` needs no more information than `other` provides.
    pub fn le(self, other: Stage) -> bool {
        self.level <= other.level
            && (!self.sequent || other.sequent)
            && (!self.history || other.history)
    }

    /// Volatile stages are never cached across strategy rounds.
    pub fn is_volatile(self) -> bool {
        self.sequent || self.history
    }

    /// The name used in `\stage(...)` assertions.
    pub fn parse(name: &str) -> Option<Stage> {
        Some(match name {
            "taclet" => Stage::TACLET,
            "app" => Stage::APP,
            "formula" => Stage::FORMULA,
            "sequent" => Stage::SEQUENT,
            "history" => Stage::HISTORY,
            _ => return None,
        })
    }
}

impl std::fmt::Display for Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut parts: Vec<&str> = Vec::new();
        if self.level != Level::Taclet || (!self.sequent && !self.history) {
            parts.push(match self.level {
                Level::Taclet => "taclet",
                Level::App => "app",
                Level::Formula => "formula",
            });
        }
        if self.sequent {
            parts.push("sequent");
        }
        if self.history {
            parts.push("history");
        }
        f.write_str(&parts.join("+"))
    }
}

/// Types of the cost language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty {
    Cost,
    Int,
    Float,
    Bool,
    /// A term (of any sort).
    Term,
    /// An operator, function or predicate.
    Op,
    Sort,
    /// A generator of terms, only allowed as the generator of a binder.
    Gen,
    /// Checked by the individual builtin.
    Any,
}

/// A builtin function or constant. `stage` is the information the builtin itself needs; the
/// stage of a call is the join of this and the stages of the arguments.
#[derive(Debug, Clone, Copy)]
pub struct Builtin {
    pub name: &'static str,
    pub params: &'static [Ty],
    pub ret: Ty,
    pub stage: Stage,
}

const fn b(name: &'static str, params: &'static [Ty], ret: Ty, stage: Stage) -> Builtin {
    Builtin {
        name,
        params,
        ret,
        stage,
    }
}

use Ty::{Any, Bool, Cost, Float, Gen, Int, Op, Term};

/// The builtin catalogue (without everything that deals with programs).
pub static BUILTINS: &[Builtin] = &[
    // The application: find, instantiations, matched \assumes
    b("find", &[], Term, Stage::APP),
    b("find_formula", &[], Term, Stage::FORMULA),
    b("assumes", &[Int], Term, Stage::APP),
    b("sub", &[Term, Int], Term, Stage::APP),
    b("inst", &[Term], Term, Stage::APP),
    b("is_instantiated", &[Term], Bool, Stage::APP),
    b("matched_assumes", &[], Bool, Stage::APP),
    b("different_replacewith_and_find", &[], Bool, Stage::APP),
    b("different_assumes_and_find", &[], Bool, Stage::FORMULA),
    b("find_depth", &[], Int, Stage::APP),
    b("find_at_top_level", &[], Bool, Stage::APP),
    b("find_in_antec", &[], Bool, Stage::APP),
    b("find_rightish", &[], Bool, Stage::APP),
    b("no_self_application", &[], Bool, Stage::APP),
    // The taclet
    b("branch_count", &[], Int, Stage::TACLET),
    // Term comparisons and predicates (pure functions of the terms)
    b("term_smaller_than", &[Term, Term], Bool, Stage::APP),
    b("atoms_smaller_than", &[Term, Term], Bool, Stage::APP),
    b("monomials_smaller_than", &[Term, Term], Bool, Stage::APP),
    b("literals_smaller_than", &[Term, Term], Bool, Stage::APP),
    b("contains", &[Term, Term], Bool, Stage::APP),
    b("eq", &[Term, Term], Bool, Stage::APP),
    b("is_polynomial", &[Term], Bool, Stage::APP),
    b("is_literal", &[Term], Bool, Stage::APP),
    b("is_atom", &[Term], Bool, Stage::APP),
    b("has_op", &[Term, Op], Bool, Stage::APP),
    // Arithmetic on costs
    b("scale", &[Cost, Float], Cost, Stage::TACLET),
    b("min", &[Cost, Cost], Cost, Stage::TACLET),
    b("max", &[Cost, Cost], Cost, Stage::TACLET),
    b("abs", &[Cost], Cost, Stage::TACLET),
    // The position of the find term in its formula
    b("directly_below_op_at_idx", &[Op, Int], Bool, Stage::FORMULA),
    b("is_below", &[Any], Bool, Stage::FORMULA),
    b("not_below_quantifier", &[], Bool, Stage::FORMULA),
    b("below_binder", &[], Bool, Stage::FORMULA),
    b("prefix_restriction", &[Any], Bool, Stage::FORMULA),
    b("count_max_d_path", &[], Int, Stage::FORMULA),
    b("count_pos_d_path", &[], Int, Stage::FORMULA),
    b("pure_pos_d_path", &[], Int, Stage::FORMULA),
    // The sequent
    b("count_occurrences", &[Term], Int, Stage::SEQUENT),
    // The proof history
    b("introduced_symbol_by", &[Any], Bool, Stage::HISTORY),
    b("formula_added_by_rule", &[Any], Bool, Stage::HISTORY),
    b("branch_mult_count", &[Any], Int, Stage::HISTORY),
    // Generators, only usable as `x in generator: body`
    b("sub_terms", &[Term], Gen, Stage::APP),
    b("super_terms", &[Term], Gen, Stage::FORMULA),
    b("sequent_formulas", &[], Gen, Stage::SEQUENT),
    b("antec_formulas", &[], Gen, Stage::SEQUENT),
    b("succ_formulas", &[], Gen, Stage::SEQUENT),
    b("relevant_formulas", &[Any], Gen, Stage::SEQUENT),
    // Binders: `sum(x in gen: cost)`, `any(x in gen: bool)`, `all(x in gen: bool)`. The argument
    // list is checked specially.
    b("sum", &[Any], Cost, Stage::TACLET),
    b("any", &[Any], Bool, Stage::TACLET),
    b("all", &[Any], Bool, Stage::TACLET),
];

/// The predefined cost bands, written `Cost::NAME`. They are the shared priority ladder of the
/// theories: a theory defines its own bands relative to these.
pub static COST_CONSTS: &[(&str, i64)] = &[
    ("CLOSE", -15000),
    ("REWRITE", -11000),
    ("SUBST", -10000),
    ("ELIMINATE", -8000),
    ("DECOMPOSE", -7000),
    ("TYPE", -6000),
    ("NORMALIZE", -5000),
    ("SIMPLIFY", -4500),
    ("EXECUTE", -4000),
    ("SOLVE", -3000),
    ("ENLARGE", -2000),
    ("PREFER", -500),
    ("DEFAULT", 0),
    ("DEFER", 500),
    ("DEFER_STRONG", 10000),
    ("LAST_RESORT", 1_000_000),
];

/// The vetoes usable in a ruleset's `guard` clauses (not part of the cost value).
pub static GUARDS: &[&str] = &[
    "no_duplicate_app",
    "no_duplicate_app_mod_pos",
    "eq_no_duplicate_app",
];

pub fn builtin(name: &str) -> Option<&'static Builtin> {
    BUILTINS.iter().find(|b| b.name == name)
}

pub fn cost_const(name: &str) -> Option<i64> {
    COST_CONSTS
        .iter()
        .find(|(n, _)| *n == name)
        .map(|(_, v)| *v)
}

pub fn is_guard(name: &str) -> bool {
    GUARDS.contains(&name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stage_lattice() {
        assert!(Stage::TACLET.le(Stage::APP));
        assert!(Stage::APP.le(Stage::FORMULA));
        assert!(!Stage::FORMULA.le(Stage::APP));
        assert!(!Stage::SEQUENT.le(Stage::FORMULA));
        assert!(!Stage::SEQUENT.le(Stage::HISTORY));
        let both = Stage::SEQUENT.join(Stage::HISTORY);
        assert!(Stage::SEQUENT.le(both) && Stage::HISTORY.le(both));
        assert_eq!(Stage::APP.join(Stage::FORMULA), Stage::FORMULA);
        assert!(both.is_volatile());
        assert!(!Stage::FORMULA.is_volatile());
    }

    #[test]
    fn catalogue_names_are_unique() {
        for (i, a) in BUILTINS.iter().enumerate() {
            for b in &BUILTINS[i + 1..] {
                assert_ne!(a.name, b.name);
            }
        }
    }
}
