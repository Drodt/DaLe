//! Automatic proof search: a best-first strategy over the rules of a [`Theory`].
//!
//! On every open goal the strategy collects the *candidates*, the applications of automatic
//! rules that match the goal's sequent, gives each a cost ([`CostModel`]), and applies the
//! cheapest. The candidates of a goal form its [`Agenda`]. Goals are worked on depth-first.
//!
//! This is step 2 of the strategy design: the agenda is rebuilt from scratch for every step, so
//! one step costs time in proportion to the size of the goal. Making agendas incremental (a child
//! goal inherits its parent's agenda and only looks at its new formulas) is a later step; it must
//! not change which proof is found, and the agendas built here are the reference it is tested
//! against.
//!
//! # Choosing a candidate
//!
//! The candidate with the least *key* is applied. The key is, in order:
//!
//! 1. the cost from the [`CostModel`] plus the *age term*: `age_weight` times the candidate's
//!    birth, which is the depth of the proof node where the newest formula the candidate uses
//!    entered the sequent (KeY's `AgeFeature`). Older candidates win among equal costs, and a
//!    candidate is not pushed back forever by a stream of cheaper new ones;
//! 2. a tie-break that only depends on the candidate: the rule number, then the side, position
//!    and term position of the `\find`, then the formulas of the `\assumes`.
//!
//! Everything the choice reads belongs to the goal itself, so the proof does not depend on the
//! order in which goals are worked on. That is what parallel search (later) relies on.
//!
//! TODO: profile the effect of the age term carefully (on by default, as in KeY); see
//! [`Config::age_weight`].
//!
//! # What is automatic
//!
//! A rule is automatic if the cost model says so ([`CostModel::is_automatic`]) and it has no free
//! schema variables (the strategy cannot supply them yet; this excludes `cut`). A candidate whose
//! cost is [`Cost::TOP`] is never applied.
//!
//! # Costs
//!
//! Until the `\rulesets` cost expressions are compiled, [`RulesetCosts`] gives every ruleset a
//! fixed cost; the cost of a rule is the sum over its rulesets, as in KeY.
//!
//! [`Theory`]: dale_theory::Theory

mod agenda;
mod cost;
mod prover;

pub use agenda::{Agenda, Candidate};
pub use cost::{Cost, CostCtx, CostModel, RulesetCosts};
pub use prover::{Config, Outcome, prove, prove_theory};
