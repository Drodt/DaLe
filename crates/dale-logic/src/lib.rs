//! Hash-consed logical terms, sorts and operators.
//!
//! A [`Signature`] declares (possibly generic) sorts and operators; a [`TermPool`] holds the
//! *instantiated* sorts (`Seq<int>`), operators (`ifThenElse<int>`, `var<int>(0)`) and terms. Terms
//! are shared: building the same operator application twice yields the same [`TermIdx`].
//!
//! Variables are de Bruijn indices (the bound variable operator `var<S>(n)`), and binders
//! (`all<S>{0}(Formula)`) only say which argument binds a variable.
//!
//! The crate knows nothing about a specific logic; only the roles of the `\top` and `\formula`
//! sorts and the shape of the bound variable operator are fixed.
//!
//! Rules have a separate pattern layer ([`Rule`], [`RuleBuilder`]) that is matched against pool
//! terms and sequents ([`match_term`], [`match_sequent`]); a [`RuleIndex`] finds the rules that
//! can match a term.
//!
//! TODO: the dale-syn side still has to report the sort of the variable an operator binds, mark
//! the bound variable operator, and reject duplicate numbers in `{..}` (at most one variable).
//!

mod apply;
mod arena;
mod display;
mod error;
mod ids;
mod index;
mod matcher;
mod pattern;
mod pool;
mod pos;
mod proof;
mod sequent;
mod sig;
mod subst;
mod table;

#[cfg(test)]
mod tests;

pub use apply::{Applied, ApplyError, apply_rule};
pub use display::Show;
pub use error::LogicError;
pub use ids::{NodeId, OpDeclId, OpIdx, SortDeclId, SortIdx, StepId, TermIdx, ViewId};
pub use index::{RuleId, RuleIndex};
pub use matcher::{Bindings, Instantiation, match_sequent, match_term};
pub use pattern::{
    Assumes, Cond, Find, GenPat, MetaPat, PatId, PatNode, PatternError, Rule, RuleBuilder, SortReq,
    SvDecl, SvId, SvKind,
};
pub use pattern::{GenTmpl, GoalTemplate, Replace, TmplId, TmplNode};
pub use pool::{Checkpoint, FrozenPool, GenArg, OpData, SortData, Term, TermPool};
pub use pos::{PosInOccurrence, PosInTerm, SortCheck};
pub use proof::{Application, Branch, Node, NodeRange, ProofError, ProofTree, Step};
pub use sequent::{SemiChange, Semisequent, Sequent, SequentChange, SequentError, Side};
pub use sig::{
    Binding, GenArgPat, GenKind, GenParam, MetaKind, MetaValue, OpDecl, OpFlags, OpKind, SigError,
    Signature, SignatureBuilder, SortDecl, SortKind, SortPat,
};
