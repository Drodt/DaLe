//! From `.theokey` files to a prover-ready logic: the bridge between the front end (`dale-syn`)
//! and the term and rule layer (`dale-logic`).
//!
//! [`Theory::load`] parses and checks a file with [`dale_syn::parse`] and then lowers *all*
//! theories of the file (and of the modules it loads) into
//!
//! * one [`Signature`]: sorts, operators, functions and predicates ([`sig`]);
//! * one [`TermPool`] that holds the ground parts of the rules;
//! * the [`Rule`]s with a [`RuleIndex`] over them, and per rule a [`RuleInfo`] (name, display
//!   name, rulesets) for the strategy ([`rules`]);
//! * the sequent of the file's `\problem`, if it has one ([`problem`]).
//!
//! The two crates stay independent of each other; only this crate knows both.
//!
//! Errors in the signature make loading fail. A rule that cannot be expressed in `dale-logic`
//! yet (for instance one with meta arguments) is *skipped* and reported in
//! [`Theory::skipped`], so that the rest of a theory stays usable while the rule layer grows.
//!
//! Symbols: the IR and `dale-logic` use the same [`Symbol`] type, and the front end's interner
//! ([`Theory::names`]) is the one to print terms with.

use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::sync::Arc;

use dale_logic::{OpDeclId, Rule, RuleId, RuleIndex, Sequent, Signature, SortDeclId, TermPool};
use dale_syn::Parsed;
use dale_syn::ir::{DefId, Span};
use dale_util::symbol::{Interner, Symbol};

pub mod problem;
pub mod rules;
pub mod sig;

pub use rules::RuleInfo;

/// A message about a declaration or rule, with the span it is about.
#[derive(Clone, Debug)]
pub struct Diag {
    pub span: Option<Span>,
    pub message: String,
}

impl Diag {
    pub fn new(span: Option<Span>, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

#[derive(Debug)]
pub enum LoadError {
    /// The file does not parse, resolve or check.
    Front(dale_syn::Error),
    /// The checked declarations do not form a valid `dale-logic` signature. The strings are
    /// rendered diagnostics.
    Signature(Vec<String>),
    /// The `\problem` is not a sequent of closed, well-sorted formulas (a rendered diagnostic).
    Problem(String),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Front(e) => write!(f, "{e}"),
            LoadError::Signature(ds) => f.write_str(&ds.join("\n")),
            LoadError::Problem(d) => f.write_str(d),
        }
    }
}

impl std::error::Error for LoadError {}

impl From<dale_syn::Error> for LoadError {
    fn from(e: dale_syn::Error) -> Self {
        LoadError::Front(e)
    }
}

/// A loaded logic: the signature, the rules and the pool their ground parts live in.
pub struct Theory {
    pub parsed: Parsed,
    pub sig: Arc<Signature>,
    /// The pool the rules were built in. Terms of problems must be built in it (or in a pool
    /// forked from it), since rules refer to its ground terms.
    pub pool: TermPool,
    pub rules: Vec<Rule>,
    /// Indexed like [`rules`](Self::rules).
    pub info: Vec<RuleInfo>,
    pub index: RuleIndex,
    /// Rules that were not loaded, with the reason.
    pub skipped: Vec<Diag>,
    /// The sequent of the `\problem`, built in [`pool`](Self::pool).
    pub problem: Option<Sequent>,
    pub(crate) sorts: HashMap<DefId, SortDeclId>,
    pub(crate) ops: HashMap<DefId, OpDeclId>,
    pub(crate) op_names: HashMap<Symbol, Vec<OpDeclId>>,
}

impl Theory {
    /// Parses, checks and lowers the `.theokey` file at `path`.
    pub fn load(path: impl AsRef<Path>) -> Result<Theory, LoadError> {
        Self::from_parsed(dale_syn::parse(path)?)
    }

    /// Like [`load`](Self::load) for text in memory; `path` decides where `\module` files are
    /// looked up.
    pub fn load_text(path: impl AsRef<Path>, text: impl Into<String>) -> Result<Theory, LoadError> {
        Self::from_parsed(dale_syn::parse_text(
            path.as_ref().to_path_buf(),
            text.into(),
        )?)
    }

    /// Lowers an already checked file.
    pub fn from_parsed(parsed: Parsed) -> Result<Theory, LoadError> {
        let lowered = match sig::lower_signature(&parsed) {
            Ok(l) => l,
            Err(diags) => {
                let msgs = diags.iter().map(|d| render(&parsed, d)).collect();
                return Err(LoadError::Signature(msgs));
            }
        };
        let sig = Arc::new(lowered.sig);
        let mut pool = TermPool::new(sig.clone());
        let mut op_names: HashMap<Symbol, Vec<OpDeclId>> = HashMap::new();
        for (id, d) in sig.op_decls() {
            op_names.entry(d.name).or_default().push(id);
        }
        let (rules, info, skipped) =
            rules::lower_rules(&parsed, &sig, &mut pool, &lowered.sorts, &lowered.ops);
        let index = RuleIndex::build(&pool, &rules);
        let problem =
            problem::lower_problem(&parsed, &sig, &mut pool, &lowered.sorts, &lowered.ops)
                .map_err(|d| LoadError::Problem(render(&parsed, &d)))?;
        Ok(Theory {
            parsed,
            sig,
            pool,
            rules,
            info,
            index,
            skipped,
            problem,
            sorts: lowered.sorts,
            ops: lowered.ops,
            op_names,
        })
    }

    /// The interner of all names (sorts, operators, rules, labels).
    pub fn names(&self) -> &'static Interner<Symbol> {
        self.parsed.interner
    }

    pub fn name(&self, s: Symbol) -> &str {
        self.parsed.interner.get(s)
    }

    /// The operator declarations with this name (several if theories reuse a name).
    pub fn ops_named(&self, name: &str) -> &[OpDeclId] {
        self.op_names
            .iter()
            .find(|(s, _)| self.name(**s) == name)
            .map_or(&[], |(_, v)| v.as_slice())
    }

    /// The `dale-logic` sort declaration of an IR sort declaration.
    pub fn sort_decl_of(&self, def: DefId) -> Option<SortDeclId> {
        self.sorts.get(&def).copied()
    }

    /// The `dale-logic` operator declaration of an IR operator, function or predicate.
    pub fn op_decl_of(&self, def: DefId) -> Option<OpDeclId> {
        self.ops.get(&def).copied()
    }

    /// The rule with this name, if it was loaded.
    pub fn rule_named(&self, name: &str) -> Option<RuleId> {
        self.info
            .iter()
            .position(|i| self.name(i.name) == name)
            .map(|i| i as RuleId)
    }

    /// `path:line:col: message`.
    pub fn render(&self, d: &Diag) -> String {
        render(&self.parsed, d)
    }
}

impl fmt::Debug for Theory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Theory")
            .field("sorts", &self.sig.sort_decls().count())
            .field("ops", &self.sig.op_decls().count())
            .field("rules", &self.rules.len())
            .field("skipped", &self.skipped.len())
            .finish()
    }
}

pub(crate) fn render(parsed: &Parsed, d: &Diag) -> String {
    match d.span.and_then(|s| parsed.sources.location(s)) {
        Some((path, lc)) => format!(
            "{}:{}:{}: {}",
            path.display(),
            lc.line,
            lc.column,
            d.message
        ),
        None => d.message.clone(),
    }
}
