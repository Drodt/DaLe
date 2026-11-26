//! Index types. They are made with `newtype_index!` like the rest of the project; the ones that
//! point into a [`TermPool`] (`SortIdx`, `OpIdx`, `TermIdx`) are only meaningful for the pool that
//! created them.
//!
//! [`TermPool`]: crate::TermPool

use dale_macros::newtype_index;

newtype_index! {
    /// A sort declaration of a [`Signature`](crate::Signature), possibly with generic parameters.
    #[orderable]
    #[debug_format = "SortDeclId({})"]
    pub struct SortDeclId {}
}

newtype_index! {
    /// An operator declaration of a [`Signature`](crate::Signature), possibly generic.
    #[orderable]
    #[debug_format = "OpDeclId({})"]
    pub struct OpDeclId {}
}

newtype_index! {
    /// An instantiated sort: a declaration with all generic arguments given.
    #[orderable]
    #[debug_format = "SortIdx({})"]
    pub struct SortIdx {}
}

newtype_index! {
    /// An instantiated operator: a declaration with generic and meta arguments given.
    /// Comparing operators is comparing these.
    #[orderable]
    #[debug_format = "OpIdx({})"]
    pub struct OpIdx {}
}

newtype_index! {
    /// A (shared) term of a pool. Equal terms have equal indices.
    #[orderable]
    #[debug_format = "TermIdx({})"]
    pub struct TermIdx {}
}
