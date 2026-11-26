//! Sequents: an antecedent and a succedent, each a duplicate-free list of closed formulas.
//!
//! Sequents are immutable values. An operation returns a new sequent (in a [`SequentChange`], with
//! a description of what changed) and leaves the old one untouched, so every node of a proof tree
//! keeps its sequent. A semi-sequent is an `Arc<[TermIdx]>`: an operation on one side copies only
//! that side's list (a memcpy of a few `u32`s), and the other side is shared with the original.
//! Operations that change nothing return the original lists without copying.
//!
//! Formulas are [`TermIdx`]es of one [`TermPool`], which is shared by all sequents of a proof.
//! Since terms are hash-consed, a formula's index is its identity: equality, duplicate detection
//! and cache keys need no wrapper object.

use std::ops::Deref;
use std::sync::Arc;

use crate::error::LogicError;
use crate::ids::TermIdx;
use crate::pool::TermPool;
use crate::pos::{PosInOccurrence, SortCheck};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Side {
    Antecedent,
    Succedent,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SequentError {
    /// The term's sort is not the formula sort or a subsort of it.
    NotAFormula(TermIdx),
    /// The formula has a bound variable that no binder in it binds.
    NotClosed(TermIdx),
    IndexOutOfRange {
        side: Side,
        index: usize,
    },
    /// A position that does not exist in the formula.
    BadPosition,
    /// Rebuilding a term failed (only with [`SortCheck::Enforce`]).
    Term(LogicError),
}

impl From<LogicError> for SequentError {
    fn from(e: LogicError) -> Self {
        match e {
            LogicError::BadPosition => SequentError::BadPosition,
            e => SequentError::Term(e),
        }
    }
}

/// An ordered list of distinct closed formulas.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Semisequent(Arc<[TermIdx]>);

impl Semisequent {
    pub fn empty() -> Self {
        Self(Arc::from([]))
    }

    pub fn as_slice(&self) -> &[TermIdx] {
        &self.0
    }

    pub fn index_of(&self, f: TermIdx) -> Option<usize> {
        self.0.iter().position(|&g| g == f)
    }

    pub fn contains(&self, f: TermIdx) -> bool {
        self.0.contains(&f)
    }
}

impl Deref for Semisequent {
    type Target = [TermIdx];
    fn deref(&self) -> &[TermIdx] {
        &self.0
    }
}

/// What an operation did to one semi-sequent.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SemiChange {
    /// Formulas inserted.
    pub added: Vec<TermIdx>,
    /// Formulas deleted (and not replaced).
    pub removed: Vec<TermIdx>,
    /// `(old, new)`: a formula replaced by another one.
    pub modified: Vec<(TermIdx, TermIdx)>,
    /// Formulas that were to be added but are already present.
    pub rejected: Vec<TermIdx>,
}

impl SemiChange {
    pub fn has_changed(&self) -> bool {
        !(self.added.is_empty() && self.removed.is_empty() && self.modified.is_empty())
    }
}

/// The result of an operation: the new sequent and what changed on each side.
#[derive(Clone, Debug)]
pub struct SequentChange {
    pub sequent: Sequent,
    pub antecedent: SemiChange,
    pub succedent: SemiChange,
}

impl SequentChange {
    pub fn side(&self, side: Side) -> &SemiChange {
        match side {
            Side::Antecedent => &self.antecedent,
            Side::Succedent => &self.succedent,
        }
    }

    pub fn has_changed(&self) -> bool {
        self.antecedent.has_changed() || self.succedent.has_changed()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sequent {
    antecedent: Semisequent,
    succedent: Semisequent,
}

impl Default for Sequent {
    fn default() -> Self {
        Self::new()
    }
}

/// Checks the invariant of sequent formulas. Both tests read cached header fields.
fn check_formula(pool: &TermPool, f: TermIdx) -> Result<(), SequentError> {
    if !pool.is_formula(f) {
        Err(SequentError::NotAFormula(f))
    } else if !pool[f].is_closed() {
        Err(SequentError::NotClosed(f))
    } else {
        Ok(())
    }
}

impl Sequent {
    pub fn new() -> Self {
        Self {
            antecedent: Semisequent::empty(),
            succedent: Semisequent::empty(),
        }
    }

    /// A sequent from formula lists; duplicates (within a side) are dropped.
    pub fn from_formulas(
        pool: &TermPool,
        antecedent: &[TermIdx],
        succedent: &[TermIdx],
    ) -> Result<Self, SequentError> {
        fn dedup(pool: &TermPool, fs: &[TermIdx]) -> Result<Semisequent, SequentError> {
            let mut v: Vec<TermIdx> = Vec::with_capacity(fs.len());
            for &f in fs {
                check_formula(pool, f)?;
                if !v.contains(&f) {
                    v.push(f);
                }
            }
            Ok(Semisequent(v.into()))
        }
        Ok(Self {
            antecedent: dedup(pool, antecedent)?,
            succedent: dedup(pool, succedent)?,
        })
    }

    pub fn antecedent(&self) -> &Semisequent {
        &self.antecedent
    }

    pub fn succedent(&self) -> &Semisequent {
        &self.succedent
    }

    pub fn semisequent(&self, side: Side) -> &Semisequent {
        match side {
            Side::Antecedent => &self.antecedent,
            Side::Succedent => &self.succedent,
        }
    }

    pub fn get(&self, side: Side, index: usize) -> Option<TermIdx> {
        self.semisequent(side).get(index).copied()
    }

    pub fn len(&self) -> usize {
        self.antecedent.len() + self.succedent.len()
    }

    pub fn is_empty(&self) -> bool {
        self.antecedent.is_empty() && self.succedent.is_empty()
    }

    /// All formulas, antecedent first.
    pub fn iter(&self) -> impl Iterator<Item = (Side, TermIdx)> + '_ {
        let a = self.antecedent.iter().map(|&f| (Side::Antecedent, f));
        let s = self.succedent.iter().map(|&f| (Side::Succedent, f));
        a.chain(s)
    }

    // ------------------------------------------------------------ operations

    /// Applies `edit` to a copy of one side. The result shares the other side, and the edited
    /// side too if `edit` made no change.
    fn edit_side<R>(
        &self,
        side: Side,
        edit: impl FnOnce(&mut Vec<TermIdx>, &mut SemiChange) -> R,
    ) -> (SequentChange, R) {
        let old = self.semisequent(side);
        let mut v = old.to_vec();
        let mut change = SemiChange::default();
        let r = edit(&mut v, &mut change);
        let new = if change.has_changed() {
            Semisequent(v.into())
        } else {
            old.clone()
        };
        let mut result = self.clone();
        match side {
            Side::Antecedent => result.antecedent = new,
            Side::Succedent => result.succedent = new,
        }
        let (antecedent, succedent) = match side {
            Side::Antecedent => (change, SemiChange::default()),
            Side::Succedent => (SemiChange::default(), change),
        };
        (
            SequentChange {
                sequent: result,
                antecedent,
                succedent,
            },
            r,
        )
    }

    fn range_check(&self, side: Side, index: usize) -> Result<(), SequentError> {
        if index < self.semisequent(side).len() {
            Ok(())
        } else {
            Err(SequentError::IndexOutOfRange { side, index })
        }
    }

    /// Adds a formula. If it is already present, the sequent is unchanged and the formula is
    /// reported as rejected.
    pub fn add(
        &self,
        pool: &TermPool,
        side: Side,
        f: TermIdx,
    ) -> Result<SequentChange, SequentError> {
        self.add_all(pool, side, &[f])
    }

    pub fn add_all(
        &self,
        pool: &TermPool,
        side: Side,
        fs: &[TermIdx],
    ) -> Result<SequentChange, SequentError> {
        for &f in fs {
            check_formula(pool, f)?;
        }
        Ok(self
            .edit_side(side, |v, c| {
                for &f in fs {
                    if v.contains(&f) {
                        c.rejected.push(f);
                    } else {
                        v.push(f);
                        c.added.push(f);
                    }
                }
            })
            .0)
    }

    /// Removes the formula at `index`.
    pub fn remove(&self, side: Side, index: usize) -> Result<SequentChange, SequentError> {
        self.range_check(side, index)?;
        Ok(self
            .edit_side(side, |v, c| c.removed.push(v.remove(index)))
            .0)
    }

    /// Replaces the formula at `index` by `new`, which may be empty (like [`remove`](Self::remove))
    /// or have several formulas, which are inserted in order at that place. Formulas that are
    /// already present are rejected. The first formula that is accepted is reported as the
    /// modification of the old one.
    pub fn replace_formula(
        &self,
        pool: &TermPool,
        side: Side,
        index: usize,
        new: &[TermIdx],
    ) -> Result<SequentChange, SequentError> {
        self.range_check(side, index)?;
        for &f in new {
            check_formula(pool, f)?;
        }
        Ok(self
            .edit_side(side, |v, c| {
                let old = v[index];
                if new == [old] {
                    return;
                }
                v.remove(index);
                let mut at = index;
                let mut replaced = false;
                for &n in new {
                    if v.contains(&n) {
                        c.rejected.push(n);
                        continue;
                    }
                    v.insert(at, n);
                    at += 1;
                    if replaced {
                        c.added.push(n);
                    } else {
                        replaced = true;
                        c.modified.push((old, n));
                    }
                }
                if !replaced {
                    c.removed.push(old);
                }
            })
            .0)
    }

    /// Replaces the subterm at `pos` by `new`. Only the terms on the path to the formula are
    /// rebuilt. The resulting formula must again be a closed formula.
    ///
    /// `checks` says whether the sorts of the rebuilt terms are verified; rules that were
    /// checked when they were loaded do not need that again.
    pub fn replace_subterm(
        &self,
        pool: &mut TermPool,
        pos: &PosInOccurrence,
        new: TermIdx,
        checks: SortCheck,
    ) -> Result<SequentChange, SequentError> {
        let f = self
            .get(pos.side, pos.index)
            .ok_or(SequentError::IndexOutOfRange {
                side: pos.side,
                index: pos.index,
            })?;
        let nf = pool.replace_at(f, pos.term_pos.as_slice(), new, checks)?;
        self.replace_formula(pool, pos.side, pos.index, &[nf])
    }

    /// Moves the formula at `index` to the other side (appended there). If the other side already
    /// has it, the formula is just removed from its side.
    pub fn move_to_other_side(
        &self,
        side: Side,
        index: usize,
    ) -> Result<SequentChange, SequentError> {
        self.range_check(side, index)?;
        let (mut first, f) = self.edit_side(side, |v, c| {
            let f = v.remove(index);
            c.removed.push(f);
            f
        });
        let (second, ()) = first.sequent.edit_side(side.other(), |v, c| {
            if v.contains(&f) {
                c.rejected.push(f);
            } else {
                v.push(f);
                c.added.push(f);
            }
        });
        let (a, s) = (second.antecedent, second.succedent);
        match side {
            Side::Antecedent => {
                first.antecedent.rejected.extend(a.rejected);
                first.succedent = s;
            }
            Side::Succedent => {
                first.succedent.rejected.extend(s.rejected);
                first.antecedent = a;
            }
        }
        first.sequent = second.sequent;
        Ok(first)
    }
}
