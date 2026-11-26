//! Index types. All of them are `u32` newtypes; the ones that point into a [`TermPool`]
//! (`SortIdx`, `OpIdx`, `TermIdx`) are only meaningful for the pool that created them.
//!
//! [`TermPool`]: crate::TermPool

use std::fmt;

use dale_index::Idx;

macro_rules! index_type {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub(crate) u32);

        impl $name {
            #[inline]
            pub const fn as_u32(self) -> u32 {
                self.0
            }
        }

        impl Idx for $name {
            #[inline]
            fn new(idx: usize) -> Self {
                assert!(idx < u32::MAX as usize, concat!(stringify!($name), " overflow"));
                Self(idx as u32)
            }

            #[inline]
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($name), "({})"), self.0)
            }
        }
    };
}

index_type!(
    /// A sort declaration of a [`Signature`](crate::Signature), possibly with generic parameters.
    SortDeclId
);
index_type!(
    /// An operator declaration of a [`Signature`](crate::Signature), possibly generic.
    OpDeclId
);
index_type!(
    /// An instantiated sort: a declaration with all generic arguments given.
    SortIdx
);
index_type!(
    /// An instantiated operator: a declaration with generic and meta arguments given.
    /// Comparing operators is comparing these.
    OpIdx
);
index_type!(
    /// A (shared) term of a pool. Equal terms have equal indices.
    TermIdx
);
