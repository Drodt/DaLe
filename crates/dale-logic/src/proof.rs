//! The proof tree.
//!
//! A proof is a tree of [`Node`]s. Each node holds a [`Sequent`]. The root holds the sequent to
//! prove. Applying a rule to a leaf (an *open goal*) records a [`Step`] and creates one child node
//! per goal template of the rule; the tree branches only when the rule has more than one. A rule
//! without goal templates *closes* the node. A node is closed when it was closed by a rule, or
//! when all children of its step are closed; the proof is closed when the root is.
//!
//! Immutability: nodes and steps never change after their creation, so every sequent and every
//! recorded rule application of the tree stays inspectable. The tree only grows, and only through
//! [`ProofTree::apply`], which turns one open goal into a step with children. What changes is
//! derived bookkeeping (which nodes are open goals, how many children of a step are still open),
//! which is private and read through [`ProofTree::is_closed`] and [`ProofTree::open_goals`].
//!
//! Nodes are numbered in creation order, the children of one step are consecutive, and a node's
//! children always have larger ids than the node.
//!
//! Terms: the term indices in a node's sequent only mean something in the pool of the worker that
//! created the node (see [`TermPool::fork`](crate::TermPool::fork)). Every node therefore refers
//! to a [`ViewId`], a pool snapshot registered with the tree. For parallel proof search each worker
//! builds its own *fragment* (a small `ProofTree` for one goal, with its own pool), and the
//! fragments are [grafted](ProofTree::graft) into the main tree in a deterministic order after
//! the parallel phase; node ids of the result do not depend on thread scheduling.
//!
//! This module does not compute rule applications: [`ProofTree::apply`] takes the already
//! computed [`Application`] and the resulting [`Branch`]es.

use dale_util::symbol::Symbol;

use crate::ids::{NodeId, StepId, ViewId};
use crate::index::RuleId;
use crate::matcher::Instantiation;
use crate::pool::FrozenPool;
use crate::pos::PosInOccurrence;
use crate::sequent::{NewFormulas, Sequent};

/// How a rule was applied: enough to display, check or replay the step.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Application {
    pub rule: RuleId,
    /// Where the `\find` matched: one position for a term find, one per formula for a sequent
    /// find (antecedent first); empty for rules without find, e.g. cuts.
    pub find: Box<[PosInOccurrence]>,
    /// For each `\assumes` formula, where in the sequent it was found.
    pub assumes: Box<[PosInOccurrence]>,
    pub inst: Instantiation,
}

/// One goal template's result: the new sequent and, if the rule names it, its label.
#[derive(Clone, Debug)]
pub struct Branch {
    pub label: Option<Symbol>,
    pub sequent: Sequent,
    /// Where the formulas are that the step put into `sequent` (`[antecedent, succedent]`), if
    /// known: [`apply_rule`](crate::apply_rule) fills it in, so that a strategy only has to look
    /// at the new formulas. The proof tree does not keep it.
    pub new_formulas: Option<[NewFormulas; 2]>,
}

impl Branch {
    pub fn new(sequent: Sequent) -> Self {
        Self {
            label: None,
            sequent,
            new_formulas: None,
        }
    }
}

#[derive(Debug)]
pub struct Node {
    parent: Option<NodeId>,
    /// The step that created this node (`None` for the root) and the goal template number.
    created_by: Option<(StepId, u32)>,
    label: Option<Symbol>,
    depth: u32,
    view: ViewId,
    sequent: Sequent,
}

impl Node {
    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }
    /// The step that created the node and which of the step's branches it is.
    pub fn created_by(&self) -> Option<(StepId, u32)> {
        self.created_by
    }
    pub fn label(&self) -> Option<Symbol> {
        self.label
    }
    /// Distance from the root (the root has depth 0).
    pub fn depth(&self) -> u32 {
        self.depth
    }
    pub fn sequent(&self) -> &Sequent {
        &self.sequent
    }
    /// The pool snapshot the sequent's term indices belong to; see [`ProofTree::view`].
    pub fn view(&self) -> ViewId {
        self.view
    }
}

/// A consecutive range of nodes, the children of a step.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeRange {
    start: u32,
    len: u32,
}

impl NodeRange {
    pub fn len(&self) -> usize {
        self.len as usize
    }
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
    pub fn get(&self, i: usize) -> Option<NodeId> {
        (i < self.len()).then(|| NodeId::from_u32(self.start + i as u32))
    }
    pub fn first(&self) -> Option<NodeId> {
        self.get(0)
    }
}

impl IntoIterator for NodeRange {
    type Item = NodeId;
    type IntoIter = std::iter::Map<std::ops::Range<u32>, fn(u32) -> NodeId>;
    fn into_iter(self) -> Self::IntoIter {
        (self.start..self.start + self.len).map(NodeId::from_u32 as fn(u32) -> NodeId)
    }
}

#[derive(Debug)]
pub struct Step {
    node: NodeId,
    app: Application,
    children: NodeRange,
}

impl Step {
    /// The node the rule was applied to.
    pub fn node(&self) -> NodeId {
        self.node
    }
    pub fn application(&self) -> &Application {
        &self.app
    }
    pub fn children(&self) -> NodeRange {
        self.children
    }
    /// The rule closed the node: it has no goal templates.
    pub fn closes(&self) -> bool {
        self.children.is_empty()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProofError {
    UnknownNode(NodeId),
    /// The node already has a step (it is not a leaf).
    NotAnOpenGoal(NodeId),
    UnknownView(ViewId),
    /// The view already has a pool.
    ViewAlreadySet(ViewId),
    /// A fragment refers to a view whose pool was never set.
    ViewMissing(ViewId),
    /// The root of the fragment does not have the sequent of the goal it is grafted onto.
    GraftMismatch(NodeId),
}

const NO_GOAL: u32 = u32::MAX;

#[derive(Debug)]
pub struct ProofTree {
    nodes: Vec<Node>,
    steps: Vec<Step>,
    // ---- derived bookkeeping, one entry per node ----
    /// The step applied to the node.
    step_of: Vec<Option<StepId>>,
    /// For a node with a step: how many of its children are not closed yet.
    open_children: Vec<u32>,
    /// Position in `goals`, or `NO_GOAL`.
    goal_pos: Vec<u32>,
    goals: Vec<NodeId>,
    /// Pool snapshots; `None` until set. View 0 is the one of the tree's creator.
    views: Vec<Option<FrozenPool>>,
}

impl ProofTree {
    /// A tree consisting of the root with the sequent to prove: one open goal. The root's view is
    /// [`ViewId::ZERO`], whose pool is set later with [`set_view`](Self::set_view).
    pub fn new(root: Sequent) -> Self {
        let mut t = Self {
            views: vec![None],
            nodes: Vec::new(),
            steps: Vec::new(),
            step_of: Vec::new(),
            open_children: Vec::new(),
            goal_pos: Vec::new(),
            goals: Vec::new(),
        };
        t.push_node(Node {
            parent: None,
            created_by: None,
            label: None,
            depth: 0,
            view: ViewId::ZERO,
            sequent: root,
        });
        t
    }

    /// Reserves a view (a pool snapshot to be set later) and returns its id.
    pub fn add_view(&mut self) -> ViewId {
        self.views.push(None);
        ViewId::from_u32(self.views.len() as u32 - 1)
    }

    /// Sets the pool of a view, once.
    pub fn set_view(&mut self, id: ViewId, pool: FrozenPool) -> Result<(), ProofError> {
        match self.views.get_mut(id.index()) {
            None => Err(ProofError::UnknownView(id)),
            Some(Some(_)) => Err(ProofError::ViewAlreadySet(id)),
            Some(slot) => {
                *slot = Some(pool);
                Ok(())
            }
        }
    }

    /// The pool of a view, if it has been set.
    pub fn view(&self, id: ViewId) -> Option<&FrozenPool> {
        self.views.get(id.index())?.as_ref()
    }

    /// The pool that the sequent of `id` lives in.
    pub fn view_of(&self, id: NodeId) -> Option<&FrozenPool> {
        self.view(self.nodes[id.index()].view)
    }

    pub fn root(&self) -> NodeId {
        NodeId::ZERO
    }

    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }

    pub fn num_steps(&self) -> usize {
        self.steps.len()
    }

    /// Panics if `id` is not a node of this tree.
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.index()]
    }

    pub fn sequent(&self, id: NodeId) -> &Sequent {
        &self.nodes[id.index()].sequent
    }

    pub fn parent(&self, id: NodeId) -> Option<NodeId> {
        self.nodes[id.index()].parent
    }

    pub fn step(&self, id: StepId) -> &Step {
        &self.steps[id.index()]
    }

    /// The step applied to `id`, `None` for open goals.
    pub fn step_of(&self, id: NodeId) -> Option<&Step> {
        self.step_of[id.index()].map(|s| &self.steps[s.index()])
    }

    /// The children of `id`: empty for open goals and for nodes closed by their rule.
    pub fn children(&self, id: NodeId) -> NodeRange {
        match self.step_of(id) {
            Some(s) => s.children,
            None => NodeRange { start: 0, len: 0 },
        }
    }

    /// The node and its ancestors up to the root.
    pub fn ancestors(&self, id: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        std::iter::successors(Some(id), move |&n| self.parent(n))
    }

    /// A node without a step that has not been closed: a goal the prover still has to work on.
    pub fn is_open_goal(&self, id: NodeId) -> bool {
        self.goal_pos[id.index()] != NO_GOAL
    }

    /// The node was closed by a rule, or all children of its step are closed.
    pub fn is_closed(&self, id: NodeId) -> bool {
        self.step_of[id.index()].is_some() && self.open_children[id.index()] == 0
    }

    pub fn is_proof_closed(&self) -> bool {
        self.is_closed(self.root())
    }

    /// The open goals, in no particular order (the order changes when goals are closed).
    pub fn open_goals(&self) -> &[NodeId] {
        &self.goals
    }

    /// Applies a rule to the open goal `goal`: records the step and creates one child per branch.
    /// No branches closes the goal. `view` is the view of the pool the branches' sequents were
    /// built in. Returns the step.
    pub fn apply(
        &mut self,
        goal: NodeId,
        app: Application,
        view: ViewId,
        branches: Vec<Branch>,
    ) -> Result<StepId, ProofError> {
        if view.index() >= self.views.len() {
            return Err(ProofError::UnknownView(view));
        }
        if goal.index() >= self.nodes.len() {
            return Err(ProofError::UnknownNode(goal));
        }
        if !self.is_open_goal(goal) {
            return Err(ProofError::NotAnOpenGoal(goal));
        }
        self.remove_goal(goal);
        let step = StepId::from_u32(self.steps.len() as u32);
        let start = self.nodes.len() as u32;
        let depth = self.nodes[goal.index()].depth + 1;
        let n = branches.len() as u32;
        for (i, b) in branches.into_iter().enumerate() {
            self.push_node(Node {
                parent: Some(goal),
                created_by: Some((step, i as u32)),
                label: b.label,
                depth,
                view,
                sequent: b.sequent,
            });
        }
        self.steps.push(Step {
            node: goal,
            app,
            children: NodeRange { start, len: n },
        });
        self.step_of[goal.index()] = Some(step);
        self.open_children[goal.index()] = n;
        if n == 0 {
            self.child_closed(goal);
        }
        Ok(step)
    }

    fn push_node(&mut self, n: Node) {
        let id = NodeId::from_u32(self.nodes.len() as u32);
        self.nodes.push(n);
        self.step_of.push(None);
        self.open_children.push(0);
        self.goal_pos.push(self.goals.len() as u32);
        self.goals.push(id);
    }

    fn remove_goal(&mut self, g: NodeId) {
        let pos = self.goal_pos[g.index()] as usize;
        self.goals.swap_remove(pos);
        if let Some(&moved) = self.goals.get(pos) {
            self.goal_pos[moved.index()] = pos as u32;
        }
        self.goal_pos[g.index()] = NO_GOAL;
    }

    /// `n` just became closed: tell its ancestors, as long as they become closed too.
    fn child_closed(&mut self, mut n: NodeId) {
        while let Some(p) = self.nodes[n.index()].parent {
            let c = &mut self.open_children[p.index()];
            *c -= 1;
            if *c > 0 {
                return;
            }
            n = p;
        }
    }
}

impl ProofTree {
    /// Replaces the open goal `goal` by the work done in `frag`, a tree that was started with
    /// `goal`'s sequent as its root (typically by a worker with a [forked](crate::TermPool::fork)
    /// pool). The fragment's steps and nodes are appended, in the order they have in the
    /// fragment; its views are appended too. The fragment's root is not copied: `goal` plays that
    /// role, so `goal` gets the fragment root's step.
    ///
    /// All views of the fragment must have been set. A fragment without any step changes
    /// nothing.
    pub fn graft(&mut self, goal: NodeId, frag: ProofTree) -> Result<(), ProofError> {
        if goal.index() >= self.nodes.len() {
            return Err(ProofError::UnknownNode(goal));
        }
        if !self.is_open_goal(goal) {
            return Err(ProofError::NotAnOpenGoal(goal));
        }
        if frag.nodes[0].sequent != self.nodes[goal.index()].sequent {
            return Err(ProofError::GraftMismatch(goal));
        }
        if let Some(i) = frag.views.iter().position(Option::is_none) {
            return Err(ProofError::ViewMissing(ViewId::from_u32(i as u32)));
        }
        let ProofTree {
            nodes,
            steps,
            step_of,
            open_children,
            goals,
            views,
            ..
        } = frag;
        let Some(root_step) = step_of[0] else {
            return Ok(());
        };
        let node_base = self.nodes.len() as u32;
        let step_base = self.steps.len() as u32;
        let view_base = self.views.len() as u32;
        let depth0 = self.nodes[goal.index()].depth;
        // fragment node 0 is `goal`, fragment node f >= 1 becomes node_base + f - 1
        let map = |n: NodeId| match n.as_u32() {
            0 => goal,
            f => NodeId::from_u32(node_base + f - 1),
        };
        let map_step = |s: StepId| StepId::from_u32(step_base + s.as_u32());
        self.views.extend(views);
        for (f, n) in nodes.into_iter().enumerate().skip(1) {
            self.nodes.push(Node {
                parent: n.parent.map(map),
                created_by: n.created_by.map(|(s, k)| (map_step(s), k)),
                label: n.label,
                depth: n.depth + depth0,
                view: ViewId::from_u32(view_base + n.view.as_u32()),
                sequent: n.sequent,
            });
            self.step_of.push(step_of[f].map(map_step));
            self.open_children.push(open_children[f]);
            self.goal_pos.push(NO_GOAL);
        }
        for s in steps {
            self.steps.push(Step {
                node: map(s.node),
                app: s.app,
                children: NodeRange {
                    start: map(NodeId::from_u32(s.children.start)).as_u32(),
                    len: s.children.len,
                },
            });
        }
        self.remove_goal(goal);
        self.step_of[goal.index()] = Some(map_step(root_step));
        self.open_children[goal.index()] = open_children[0];
        for g in goals {
            let g = map(g);
            self.goal_pos[g.index()] = self.goals.len() as u32;
            self.goals.push(g);
        }
        if open_children[0] == 0 {
            self.child_closed(goal);
        }
        Ok(())
    }
}
