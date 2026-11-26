//! Positions inside terms and sequents, and replacing the subterm at a position.

use crate::error::LogicError;
use crate::ids::TermIdx;
use crate::pool::TermPool;
use crate::sequent::{Sequent, Side};

/// Whether rebuilding a term after a replacement re-checks the sorts (argument sorts and the
/// sorts of bound variables). Rules are checked for well-formedness when they are loaded, so the
/// prover can skip these checks per application.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SortCheck {
    Enforce,
    Skip,
}

/// A position in a term: the child numbers on the path from the root. The empty path is the root.
// TODO: the path is a heap allocated vector; if positions turn out to be created in hot loops,
// use an inline small vector.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PosInTerm(Vec<u16>);

impl PosInTerm {
    pub fn top() -> Self {
        Self(Vec::new())
    }

    pub fn from_path(path: &[u16]) -> Self {
        Self(path.to_vec())
    }

    pub fn is_top(&self) -> bool {
        self.0.is_empty()
    }

    pub fn depth(&self) -> usize {
        self.0.len()
    }

    pub fn as_slice(&self) -> &[u16] {
        &self.0
    }

    /// The position of the `i`-th child.
    #[must_use]
    pub fn down(&self, i: u16) -> Self {
        let mut v = Vec::with_capacity(self.0.len() + 1);
        v.extend_from_slice(&self.0);
        v.push(i);
        Self(v)
    }

    /// The position of the parent, `None` at the root.
    #[must_use]
    pub fn up(&self) -> Option<Self> {
        let (_, rest) = self.0.split_last()?;
        Some(Self(rest.to_vec()))
    }

    /// The subterm of `root` at this position, `None` if the position does not exist.
    pub fn sub_term(&self, pool: &TermPool, root: TermIdx) -> Option<TermIdx> {
        self.0
            .iter()
            .try_fold(root, |t, &i| pool.subs_of(t).get(i as usize).copied())
    }
}

/// A position in a sequent: a formula of one side (by index into the semi-sequent) and a position
/// in that formula. It refers to the sequent it was found in; sequents are immutable, so it stays
/// valid for that sequent.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PosInOccurrence {
    pub side: Side,
    pub index: usize,
    pub term_pos: PosInTerm,
}

impl PosInOccurrence {
    pub fn new(side: Side, index: usize, term_pos: PosInTerm) -> Self {
        Self {
            side,
            index,
            term_pos,
        }
    }

    /// The position of the formula itself.
    pub fn top_level(side: Side, index: usize) -> Self {
        Self::new(side, index, PosInTerm::top())
    }

    pub fn is_top_level(&self) -> bool {
        self.term_pos.is_top()
    }

    /// The pointed-to term, `None` if the position does not exist in `seq`.
    pub fn sub_term(&self, pool: &TermPool, seq: &Sequent) -> Option<TermIdx> {
        let f = seq.get(self.side, self.index)?;
        self.term_pos.sub_term(pool, f)
    }
}

impl TermPool {
    /// `root` with the subterm at `path` replaced by `new`. Only the terms on the path are
    /// rebuilt; everything else is shared.
    ///
    /// With [`SortCheck::Enforce`] every rebuilt term is created with [`mk`](Self::mk). With
    /// [`SortCheck::Skip`] the caller guarantees well-formedness (argument sorts, binder sorts)
    /// and [`mk_unchecked`](Self::mk_unchecked) is used.
    pub fn replace_at(
        &mut self,
        root: TermIdx,
        path: &[u16],
        new: TermIdx,
        checks: SortCheck,
    ) -> Result<TermIdx, LogicError> {
        let mut spine = Vec::with_capacity(path.len());
        let mut cur = root;
        for &i in path {
            let child = *self
                .subs_of(cur)
                .get(i as usize)
                .ok_or(LogicError::BadPosition)?;
            spine.push(cur);
            cur = child;
        }
        let mut result = new;
        for (&parent, &i) in spine.iter().zip(path).rev() {
            let mut args = self.subs_of(parent).to_vec();
            args[i as usize] = result;
            let op = self.op_of(parent);
            result = match checks {
                SortCheck::Enforce => self.mk(op, &args)?,
                SortCheck::Skip => self.mk_unchecked(op, &args),
            };
        }
        Ok(result)
    }
}

impl Side {
    #[must_use]
    pub fn other(self) -> Side {
        match self {
            Side::Antecedent => Side::Succedent,
            Side::Succedent => Side::Antecedent,
        }
    }
}
