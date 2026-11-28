//! Costs of rule applications.

use std::fmt;
use std::ops::Add;

use dale_logic::{Bindings, PosInOccurrence, Rule, RuleId, Sequent, TermPool};
use dale_theory::Theory;

/// The cost of a rule application: smaller is preferred. [`Cost::TOP`] is infinite: such an
/// application is never chosen. Addition saturates and `TOP` absorbs everything.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cost(pub i64);

impl Cost {
    pub const TOP: Cost = Cost(i64::MAX);
    pub const ZERO: Cost = Cost(0);

    pub fn is_top(self) -> bool {
        self == Cost::TOP
    }
}

impl Add for Cost {
    type Output = Cost;
    fn add(self, o: Cost) -> Cost {
        if self.is_top() || o.is_top() {
            Cost::TOP
        } else {
            // saturating at the bottom too; at the top it must not turn into `TOP` by accident
            Cost(self.0.saturating_add(o.0).min(i64::MAX - 1))
        }
    }
}

impl fmt::Display for Cost {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_top() {
            f.write_str("top")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

/// What a cost can depend on: one candidate in its goal.
pub struct CostCtx<'a> {
    pub pool: &'a TermPool,
    pub rule_id: RuleId,
    pub rule: &'a Rule,
    pub seq: &'a Sequent,
    /// Where a term `\find` matched; `None` for sequent finds and rules without find (their
    /// formulas are in [`Bindings::matched`]).
    pub find: Option<&'a PosInOccurrence>,
    pub bindings: &'a Bindings,
}

/// Assigns costs to candidates.
pub trait CostModel {
    /// Whether the strategy considers `rule` at all. Rules that are not are never matched.
    fn is_automatic(&self, rule: RuleId) -> bool;

    /// The cost of a candidate of an automatic rule.
    fn cost(&self, cx: &CostCtx<'_>) -> Cost;
}

/// A fixed cost per ruleset: the cost of a rule is the sum of the costs of its rulesets. A rule
/// none of whose rulesets has a cost is not automatic; neither is one whose sum is `TOP`.
///
/// A stand-in for the compiled `\rulesets` expressions, which may depend on the application.
#[derive(Clone, Debug)]
pub struct RulesetCosts {
    by_rule: Vec<Option<Cost>>,
}

impl RulesetCosts {
    /// Costs from `(ruleset name, cost)` pairs; rulesets that are not listed are ignored.
    pub fn from_table(th: &Theory, table: &[(&str, Cost)]) -> Self {
        let by_rule = th
            .info
            .iter()
            .map(|info| {
                let mut sum = None;
                for s in &info.rule_sets {
                    if let Some((_, c)) = table.iter().find(|(n, _)| *n == th.name(*s)) {
                        sum = Some(sum.unwrap_or(Cost::ZERO) + *c);
                    }
                }
                sum.filter(|c| !c.is_top())
            })
            .collect();
        RulesetCosts { by_rule }
    }

    /// The cost of every application of `rule`, if it is automatic.
    pub fn rule_cost(&self, rule: RuleId) -> Option<Cost> {
        self.by_rule.get(rule as usize).copied().flatten()
    }
}

impl CostModel for RulesetCosts {
    fn is_automatic(&self, rule: RuleId) -> bool {
        self.rule_cost(rule).is_some()
    }

    fn cost(&self, cx: &CostCtx<'_>) -> Cost {
        self.rule_cost(cx.rule_id).unwrap_or(Cost::TOP)
    }
}
