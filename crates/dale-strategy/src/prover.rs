//! The sequential prover: depth-first over goals, best-first within a goal.

use std::collections::HashMap;

use dale_logic::{
    NodeId, ProofTree, Rule, RuleIndex, Sequent, Side, SortCheck, TermIdx, TermPool, ViewId,
    apply_rule,
};
use dale_theory::Theory;

use crate::agenda::{Agenda, Env, side_ix};
use crate::cost::CostModel;

/// Settings of a proof search.
#[derive(Clone, Debug)]
pub struct Config {
    /// At most this many rule applications.
    pub max_steps: usize,
    /// The weight of the age term (see the crate documentation); 0 turns it off.
    ///
    /// TODO: profile carefully whether the default (1, as in KeY) is right, and how it interacts
    /// with the magnitude of the ruleset costs.
    pub age_weight: i64,
    /// Stop at the first goal without candidates: it cannot be closed automatically, so neither
    /// can the proof.
    pub stop_on_failure: bool,
    /// Whether rule applications check the sorts of the terms they build again.
    pub checks: SortCheck,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            max_steps: 100_000,
            age_weight: 1,
            stop_on_failure: false,
            checks: SortCheck::Skip,
        }
    }
}

/// The result of a proof search.
#[derive(Debug)]
pub struct Outcome {
    pub tree: ProofTree,
    /// Whether the proof is closed.
    pub closed: bool,
    /// Rule applications done.
    pub steps: usize,
    /// Goals where no candidate was left, in the order they were found.
    pub stuck: Vec<NodeId>,
    /// Whether the search stopped at [`Config::max_steps`].
    pub out_of_steps: bool,
}

impl Outcome {
    /// The open goals of the proof (stuck, or not worked on).
    pub fn open(&self) -> usize {
        self.tree.open_goals().len()
    }
}

/// An open goal waiting to be worked on, with the depth at which each of its formulas entered
/// the sequent (per side and position).
struct Goal {
    node: NodeId,
    births: [Vec<u32>; 2],
}

/// Searches a proof of `root` with the rules of `th` (the theory's pool is used for new terms).
pub fn prove_theory<C: CostModel>(
    th: &mut Theory,
    costs: &C,
    root: Sequent,
    cfg: &Config,
) -> Outcome {
    prove(&mut th.pool, &th.rules, &th.index, costs, root, cfg)
}

/// Searches a proof of `root`. Goals are worked on depth-first, the first branch of a rule first;
/// on each goal the best candidate of its [`Agenda`] is applied.
pub fn prove<C: CostModel>(
    pool: &mut TermPool,
    rules: &[Rule],
    index: &RuleIndex,
    costs: &C,
    root: Sequent,
    cfg: &Config,
) -> Outcome {
    let auto: Vec<bool> = (0..rules.len())
        .map(|r| rules[r].free_svs().is_empty() && costs.is_automatic(r as u32))
        .collect();
    let births = [
        vec![0; root.antecedent().len()],
        vec![0; root.succedent().len()],
    ];
    let mut tree = ProofTree::new(root);
    let mut stack = vec![Goal {
        node: tree.root(),
        births,
    }];
    let (mut steps, mut stuck, mut out_of_steps) = (0, Vec::new(), false);
    while let Some(g) = stack.pop() {
        if steps >= cfg.max_steps {
            out_of_steps = true;
            break;
        }
        let seq = tree.sequent(g.node).clone();
        let env = Env {
            pool,
            rules,
            index,
            costs,
            auto: &auto,
            age_weight: cfg.age_weight,
        };
        let agenda = Agenda::for_goal(&env, &seq, &g.births);
        let Some(best) = agenda.best() else {
            stuck.push(g.node);
            if cfg.stop_on_failure {
                break;
            }
            continue;
        };
        let rule = &rules[best.rule as usize];
        let applied = apply_rule(
            pool,
            rule,
            best.rule,
            &seq,
            best.find.as_ref(),
            &best.bindings,
            cfg.checks,
        )
        // a match of a checked rule always applies; anything else is a bug in a lower layer
        .unwrap_or_else(|e| panic!("applying rule {} failed: {e:?}", best.rule));
        steps += 1;
        let depth = tree.node(g.node).depth() + 1;
        let kid_births: Vec<[Vec<u32>; 2]> = applied
            .branches
            .iter()
            .map(|b| child_births(&seq, &g.births, &b.sequent, b.new_formulas.as_ref(), depth))
            .collect();
        let step = tree
            .apply(g.node, applied.app, ViewId::ZERO, applied.branches)
            .expect("an open goal");
        let kids: Vec<NodeId> = tree.step(step).children().into_iter().collect();
        for (node, births) in kids.into_iter().zip(kid_births).rev() {
            stack.push(Goal { node, births });
        }
    }
    Outcome {
        closed: tree.is_proof_closed(),
        tree,
        steps,
        stuck,
        out_of_steps,
    }
}

/// The births of a child's formulas: those the step put in are born at `depth`, the others keep
/// their birth in the parent.
fn child_births(
    parent: &Sequent,
    births: &[Vec<u32>; 2],
    child: &Sequent,
    new: Option<&[dale_logic::NewFormulas; 2]>,
    depth: u32,
) -> [Vec<u32>; 2] {
    [Side::Antecedent, Side::Succedent].map(|side| {
        let k = side_ix(side);
        let old: HashMap<TermIdx, u32> = parent
            .semisequent(side)
            .iter()
            .zip(&births[k])
            .map(|(&f, &b)| (f, b))
            .collect();
        let mut out: Vec<u32> = child
            .semisequent(side)
            .iter()
            .map(|f| old.get(f).copied().unwrap_or(depth))
            .collect();
        if let Some(new) = new {
            for i in new[k].positions() {
                out[i] = depth;
            }
        }
        out
    })
}
