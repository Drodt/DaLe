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
//! TODO: schema variables only occur in rules. Decide between a separate pattern layer (rule
//! patterns as their own structure that can be matched against pool terms) and schema variable
//! operators/sorts inside the regular term structure.
//!
//! TODO: the dale-syn side still has to report the sort of the variable an operator binds, mark
//! the bound variable operator, and reject duplicate numbers in `{..}` (at most one variable).
//!

mod display;
mod error;
mod ids;
mod pool;
mod pos;
mod sequent;
mod sig;
mod subst;
mod table;

#[cfg(test)]
mod tests;

pub use display::Show;
pub use error::LogicError;
pub use ids::{OpDeclId, OpIdx, SortDeclId, SortIdx, TermIdx};
pub use pool::{Checkpoint, GenArg, OpData, SortData, Term, TermPool};
pub use pos::{PosInOccurrence, PosInTerm, SortCheck};
pub use sequent::{SemiChange, Semisequent, Sequent, SequentChange, SequentError, Side};
pub use sig::{
    Binding, GenArgPat, GenKind, GenParam, MetaKind, MetaValue, OpDecl, OpFlags, OpKind, SigError,
    Signature, SignatureBuilder, SortDecl, SortKind, SortPat,
};
